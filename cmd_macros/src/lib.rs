use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::{End, ParseStream}, parse_macro_input, token::{Enum, Struct, Union}, Attribute, Data, DataStruct, DataUnion, DeriveInput, Error, Expr, ExprLit, Lit, LitStr, Result, Variant};

#[derive(Default)]
struct IncompleteAttrs<'a> {
    help: Option<&'a LitStr>,
    template: Option<&'a LitStr>,
}

struct Attrs<'a> {
    help: &'a LitStr,
    template: &'a LitStr,
}

impl<'a> Attrs<'a> {
    fn new(node: impl ToTokens, src: IncompleteAttrs<'a>) -> Result<Self> {
        let Some(help) = src.help else { return Err(Error::new_spanned(node, "missing help attribute")) };
        let Some(template) = src.template else { return Err(Error::new_spanned(node, "missing template attribute")) };
        Ok(Self {
            help,
            template,
        })
    }
}

fn get_attrs(input: &Variant) -> Result<Attrs<'_>> {
    let mut attrs = IncompleteAttrs::default();

    for attr in &input.attrs {
        if attr.path().is_ident("help") {
            if attrs.help.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[help] attribute"));
            }
            let value = &attr.meta.require_name_value()?.value;
            match value {
                Expr::Lit(ExprLit { lit: Lit::Str(string), .. }) => {
                    attrs.help = Some(string);
                }
                _ => return Err(Error::new_spanned(value, "help value must be a string literal")),
            }
        } else if attr.path().is_ident("template") {
            if attrs.template.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[template] attribute"));
            }
            let value = &attr.meta.require_name_value()?.value;
            match value {
                Expr::Lit(ExprLit { lit: Lit::Str(string), .. }) => {
                    attrs.template = Some(string);
                }
                _ => return Err(Error::new_spanned(value, "template value must be a string literal")),
            }
        }
    }

    Attrs::new(input, attrs)
}

#[proc_macro_derive(Command, attributes(help, template))]
pub fn derive_command(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_derive(input)
        .map_or_else(|e| e.into_compile_error().into(), |x| x.into())
}

fn impl_derive(input: DeriveInput) -> Result<TokenStream> {
    let ident = input.ident;
    match input.data {
        Data::Enum(data) => {
            let (variant_idents, (variant_help, variant_templates)) = data
                .variants
                .iter()
                .map(|node| {
                    let Attrs { help, template } = get_attrs(node)?;
                    Ok((&node.ident, (help, template)))
                })
                .collect::<Result<(Vec<_>, (Vec<_>, Vec<_>))>>()?;

            Ok(quote! {
                impl #ident {
                    pub const fn help(&self) -> &'static str {
                        match self {
                            #(Self::#variant_idents => #variant_help,)*
                        }
                    }

                    pub const fn template(&self) -> &'static str {
                        match self {
                            #(Self::#variant_idents => #variant_templates,)*
                        }
                    }
                }
            }.into())
        }

        | Data::Struct(DataStruct { struct_token, .. })
            => Err(Error::new_spanned(struct_token, "struct as commands are not supported")),

        | Data::Union(DataUnion { union_token, .. })
            => Err(Error::new_spanned(union_token, "union as commands are not supported")),
    }
}
