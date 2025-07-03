use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::{End, ParseStream}, parse_macro_input, token::{Enum, Struct, Union}, Attribute, Data, DataStruct, DataUnion, DeriveInput, Error, Expr, ExprLit, Lit, LitStr, Result, Variant};

#[proc_macro_derive(CommandEnum, attributes(help, input))]
pub fn derive_command_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_command_enum(input)
        .map_or_else(|e| e.into_compile_error().into(), |x| x.into())
}

#[proc_macro_derive(Command, attributes(help, template))]
pub fn derive_command(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_command(input)
        .map_or_else(|e| e.into_compile_error().into(), |x| x.into())
}

fn impl_command_enum(input: DeriveInput) -> Result<TokenStream> {
    let ident = input.ident;
    match input.data {
        Data::Enum(data) => {
            let (variant_idents, (variant_help, variant_templates)) = data
                .variants
                .iter()
                .map(|node| {
                    let mut help = None;
                    let mut input = None;

                    for attr in &node.attrs {
                        if attr.path().is_ident("help") {
                            if help.is_some() {
                                return Err(Error::new_spanned(attr, "duplicate #[help] attribute"));
                            }
                            let value = &attr.meta.require_name_value()?.value;
                            match value {
                                Expr::Lit(ExprLit { lit: Lit::Str(string), .. }) => {
                                    help = Some(string);
                                }
                                _ => return Err(Error::new_spanned(value, "help value must be a string literal")),
                            }
                        } else if attr.path().is_ident("input") {
                            if input.is_some() {
                                return Err(Error::new_spanned(attr, "duplicate #[input] attribute"));
                            }
                            let value = &attr.meta.require_name_value()?.value;
                            match value {
                                Expr::Lit(ExprLit { lit: Lit::Str(string), .. }) => {
                                    input = Some(string);
                                }
                                _ => return Err(Error::new_spanned(value, "input value must be a string literal")),
                            }
                        }
                    }

                    Ok((
                        &node.ident,
                        (
                            help.ok_or_else(|| Error::new_spanned(node, "missing help attribute"))?,
                            input.ok_or_else(|| Error::new_spanned(node, "missing input attribute"))?,
                        )
                    ))
                })
                .collect::<Result<(Vec<_>, (Vec<_>, Vec<_>))>>()?;

            Ok(quote! {
                impl #ident {
                    pub const fn help(&self) -> &'static str {
                        match self {
                            #(Self::#variant_idents {..} => #variant_help,)*
                        }
                    }

                    pub const fn template(&self) -> &'static str {
                        match self {
                            #(Self::#variant_idents {..} => #variant_templates,)*
                        }
                    }
                }
            }.into())
        }

        | Data::Struct(DataStruct { struct_token, .. })
            => Err(Error::new_spanned(struct_token, "struct as command enums are not supported")),

        | Data::Union(DataUnion { union_token, .. })
            => Err(Error::new_spanned(union_token, "union as command enums are not supported")),
    }
}

fn impl_command(input: DeriveInput) -> Result<TokenStream> {
    let ident = input.ident;
    match input.data {
        Data::Enum(data) => {
            let (variant_idents, (variant_help, variant_templates)) = data
                .variants
                .iter()
                .map(|node| {
                    let mut help = None;
                    let mut template = None;

                    for attr in &node.attrs {
                        if attr.path().is_ident("help") {
                            if help.is_some() {
                                return Err(Error::new_spanned(attr, "duplicate #[help] attribute"));
                            }
                            let value = &attr.meta.require_name_value()?.value;
                            match value {
                                Expr::Lit(ExprLit { lit: Lit::Str(string), .. }) => {
                                    help = Some(string);
                                }
                                _ => return Err(Error::new_spanned(value, "help value must be a string literal")),
                            }
                        } else if attr.path().is_ident("template") {
                            if template.is_some() {
                                return Err(Error::new_spanned(attr, "duplicate #[template] attribute"));
                            }
                            let value = &attr.meta.require_name_value()?.value;
                            match value {
                                Expr::Lit(ExprLit { lit: Lit::Str(string), .. }) => {
                                    template = Some(string);
                                }
                                _ => return Err(Error::new_spanned(value, "template value must be a string literal")),
                            }
                        }
                    }

                    Ok((
                        &node.ident,
                        (
                            help.ok_or_else(|| Error::new_spanned(node, "missing help attribute"))?,
                            template.ok_or_else(|| Error::new_spanned(node, "missing template attribute"))?,
                        )
                    ))
                })
                .collect::<Result<(Vec<_>, (Vec<_>, Vec<_>))>>()?;

            Ok(quote! {
                impl #ident {
                    pub const fn help(&self) -> &'static str {
                        match self {
                            #(Self::#variant_idents {..} => #variant_help,)*
                        }
                    }

                    pub const fn template(&self) -> &'static str {
                        match self {
                            #(Self::#variant_idents {..} => #variant_templates,)*
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
