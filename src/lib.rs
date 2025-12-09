use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use std::cell::RefCell;
use std::collections::HashMap;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Expr, ExprLit, ItemEnum, Lit, LitInt, LitStr,
    Meta, MetaNameValue, Variant,
};

thread_local! {
    /// Global registry of seen UUID values (as u128) within this compilation unit.
    ///
    /// This enforces "no UUID reuse" across all #[uuid_enum] enums in the same crate.
    static SEEN_UUIDS: RefCell<HashMap<u128, Span>> = RefCell::new(HashMap::new());
}

/// Attribute macro for UUID-backed enums.
///
/// Usage:
///
/// ```ignore
/// use uuid_enum::uuid_enum;
///
/// #[uuid_enum]
/// pub enum AccountGrant {
///     #[uuid("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb")]
///     Owner,
///     #[uuid("cccccccc-cccc-cccc-cccc-cccccccccccc")]
///     Manage,
/// }
/// ```
///
/// This expands to a `#[repr(u128)]` enum where each variant's discriminant
/// is the numerical `u128` value of its UUID. It also generates:
///
/// - `const ALL: &'static [Self]` containing all variants
/// - `pub const fn as_u128(self) -> u128`
/// - `pub const fn from_u128(raw: u128) -> Option<Self>`
/// - `pub const fn to_uuid(self) -> ::uuid_enum::uuid::Uuid`
/// - `pub const fn from_uuid(id: ::uuid_enum::uuid::Uuid) -> Option<Self>`
///
/// The UUID values are checked for global uniqueness across the crate.
/// Reuse of the same UUID in two variants (even in different enums using this
/// macro) is a compile error.
#[proc_macro_attribute]
pub fn uuid_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    // We don't currently support any arguments on #[uuid_enum].
    if !attr.is_empty() {
        let ts = proc_macro2::TokenStream::from(attr);
        return syn::Error::new_spanned(ts, "#[uuid_enum] does not take any arguments")
            .to_compile_error()
            .into();
    }

    let input = parse_macro_input!(item as ItemEnum);

    match expand_uuid_enum(input) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand_uuid_enum(item: ItemEnum) -> syn::Result<TokenStream> {
    // We keep the enum's visibility, name, and generics, but we currently only
    // support non-generic enums.
    if !item.generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            &item.generics,
            "#[uuid_enum] currently only supports non-generic enums",
        ));
    }

    // Reject enums with an existing #[repr(...)] attribute to avoid conflicts.
    for attr in &item.attrs {
        if attr.path().is_ident("repr") {
            return Err(syn::Error::new_spanned(
                attr,
                "#[uuid_enum] must not be combined with an explicit #[repr(..)] on the enum",
            ));
        }
    }

    let ident = &item.ident;

    // For each variant, we will compute a u128 discriminant from its #[uuid("…")] attribute.
    let mut variant_idents = Vec::new();
    let mut variant_values = Vec::new();

    for variant in &item.variants {
        ensure_unit_variant(variant)?;

        if variant.discriminant.is_some() {
            return Err(syn::Error::new_spanned(
                variant,
                "#[uuid_enum] does not allow explicit discriminants; they are derived from #[uuid(\"…\")]",
            ));
        }

        let uuid_attr = find_uuid_attribute(&variant.attrs).ok_or_else(|| {
            syn::Error::new(
                variant.span(),
                "each variant in a #[uuid_enum] must have a #[uuid(\"…\")] attribute",
            )
        })?;

        let uuid_str = parse_uuid_attribute(uuid_attr)?;
        let uuid = uuid::Uuid::parse_str(&uuid_str).map_err(|e| {
            syn::Error::new(
                uuid_attr.span(),
                format!("invalid UUID string in #[uuid(..)]: {e}"),
            )
        })?;
        let value = uuid.as_u128();

        // Global uniqueness check across all enums using this macro in this crate.
        let dup = SEEN_UUIDS.with(|cell| {
            let mut map = cell.borrow_mut();
            if let Some(prev_span) = map.get(&value) {
                Some(*prev_span)
            } else {
                map.insert(value, uuid_attr.span());
                None
            }
        });

        if let Some(prev_span) = dup {
            let mut err = syn::Error::new(
                uuid_attr.span(),
                "this UUID is already used elsewhere in this crate",
            );
            err.combine(syn::Error::new(
                prev_span,
                "previous use of this UUID is here",
            ));
            return Err(err);
        }

        variant_idents.push(&variant.ident);
        variant_values.push(value);
    }

    // Reconstruct the enum with #[repr(u128)] and explicit numeric discriminants.
    let mut new_enum = item.clone();
    new_enum.attrs.push(syn::parse_quote!(#[repr(u128)]));

    // Strip #[uuid(..)] attributes from the variants and set the discriminants.
    for (variant, value) in new_enum
        .variants
        .iter_mut()
        .zip(variant_values.iter().copied())
    {
        // Retain all attributes except #[uuid(..)] on the variant.
        variant.attrs.retain(|attr| !is_uuid_attr(attr));

        let lit = LitInt::new(&format!("{value}_u128"), variant.span());
        variant.discriminant = Some((
            syn::token::Eq {
                spans: [variant.span()],
            },
            Expr::Lit(ExprLit {
                attrs: Vec::new(),
                lit: Lit::Int(lit),
            }),
        ));
    }

    // Build the impl block. We use fully-qualified core and uuid_enum paths so
    // generated code is independent of user imports.
    let expanded = {
        let all_variants = &variant_idents;

        quote! {
            #new_enum

            impl #ident {
                /// All variants of this enum.
                pub const ALL: &'static [Self] = &[
                    #( Self::#all_variants, )*
                ];

                /// Convert this enum variant into its raw `u128` discriminant.
                pub const fn as_u128(self) -> u128 {
                    self as u128
                }

                /// Try to recover an enum variant from its raw `u128` discriminant.
                pub const fn from_u128(raw: u128) -> ::core::option::Option<Self> {
                    match raw {
                        #(
                            x if x == Self::#all_variants as u128 =>
                                ::core::option::Option::Some(Self::#all_variants),
                        )*
                        _ => ::core::option::Option::None,
                    }
                }

                /// Convert this enum variant into a `Uuid` from the `uuid_enum` façade crate.
                pub const fn to_uuid(self) -> ::uuid_enum::uuid::Uuid {
                    ::uuid_enum::uuid::Uuid::from_u128(self.as_u128())
                }

                /// Try to recover an enum variant from a `Uuid` value.
                pub const fn from_uuid(id: ::uuid_enum::uuid::Uuid) -> ::core::option::Option<Self> {
                    Self::from_u128(id.as_u128())
                }
            }
        }
    };

    Ok(expanded.into())
}

fn ensure_unit_variant(variant: &Variant) -> syn::Result<()> {
    if !variant.fields.is_empty() {
        return Err(syn::Error::new(
            variant.span(),
            "#[uuid_enum] only supports C-like (fieldless) enum variants",
        ));
    }
    Ok(())
}

fn is_uuid_attr(attr: &Attribute) -> bool {
    attr.path().is_ident("uuid")
}

fn find_uuid_attribute<'a>(attrs: &'a [Attribute]) -> Option<&'a Attribute> {
    attrs.iter().find(|a| is_uuid_attr(a))
}

/// Parse a `#[uuid("…")]` attribute into its string literal.
fn parse_uuid_attribute(attr: &Attribute) -> syn::Result<String> {
    // Accept either #[uuid("…")] or #[uuid = "…"].
    match &attr.meta {
        Meta::NameValue(MetaNameValue { value, .. }) => {
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                return Ok(s.value());
            } else {
                return Err(syn::Error::new_spanned(
                    value,
                    "#[uuid = ...] expects a string literal",
                ));
            }
        }
        Meta::Path(_) => {
            return Err(syn::Error::new_spanned(
                attr,
                "#[uuid] is missing the string literal value",
            ))
        }
        Meta::List(_) => {
            let s: LitStr = attr.parse_args()?;
            return Ok(s.value());
        }
    }
}
