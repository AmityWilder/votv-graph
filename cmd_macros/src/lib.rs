#![allow(unused)]

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{braced, bracketed, parenthesized, parse::{Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token, Attribute, Block, Error, Expr, ExprBlock, Field, Ident, Item, Lit, LitStr, Meta, MetaNameValue, Result, Token, Type, Visibility};

pub fn alloc(size: usize) -> NonNull<[MaybeUninit<u8>]> {
    NonNull::new(std::ptr::slice_from_raw_parts_mut(1usize as *mut _, size))
        .expect("1 is not null")
}

fn uppercase_start(s: &str) -> String {
    if let Some(ch) = s.chars().next() {
        format!("{}{}", ch.to_uppercase(), &s[ch.len_utf8()..])
    } else {
        String::new()
    }
}

fn doc_attrs<'a>(attrs: impl IntoIterator<Item = &'a Attribute>) -> String {
    attrs
        .into_iter()
        .filter_map(|attr| {
            if attr.path().is_ident("doc") {
                if let Meta::NameValue(MetaNameValue { value: Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(doc), .. }), .. }) = &attr.meta {
                    let mut string = doc.value();
                    if string.starts_with(' ') {
                        string.remove(0);
                    }
                    return Some(string);
                }
            }
            None
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[proc_macro]
pub fn cmd(input: TokenStream) -> TokenStream {
    let cmd_enum = parse_macro_input!(input as CmdEnumData);
    let vis = &cmd_enum.vis;
    let ident = &cmd_enum.ident;

    let (
        commands,
        inputs,
        help_general,
    ) = cmd_enum.commands.iter()
        .map(|cmd| {
            let idents = cmd.name.iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<_>>();
            (
                Ident::new(
                    &idents.iter()
                        .map(|s| uppercase_start(&s))
                        .collect::<String>(),
                    cmd.name.span(),
                ),
                idents.join("."),
                doc_attrs(&cmd.attrs),
            )
        })
        .collect::<(Vec<_>, Vec<_>, Vec<_>)>();

    let num_commands = commands.len();

    let (
        overloads,
        overload_args,
        overload_blocks,
    ) = cmd_enum.commands.iter()
        .map(|cmd| cmd.overloads.iter()
            .map(|usage| (
                &usage.ident,
                &usage.args,
                &usage.definition,
            ))
            .collect()
        )
        .collect::<(Vec<Vec<_>>, Vec<Vec<_>>, Vec<Vec<_>>)>();

    let (
        overload_args_struct,
        overload_rets_struct,
        overload_pascal,
        full_pascal,
    ) = cmd_enum.commands.iter()
        .zip(&commands)
        .map(|(cmd, cmd_pascal)| cmd.overloads.iter()
            .map(|usage| {
                let pascal = Ident::new(
                    &usage.ident.to_string()
                        .split("_")
                        .map(uppercase_start)
                        .collect::<String>(),
                    usage.ident.span(),
                );
                let pascal_full = format_ident!("{cmd_pascal}{pascal}");
                (format_ident!("{pascal_full}Args"), format_ident!("{pascal_full}Ret"), pascal_full, pascal)
            })
            .collect::<(Vec<_>, Vec<_>, Vec<_>, Vec<_>)>()
        )
        .collect::<(Vec<_>, Vec<_>, Vec<_>, Vec<_>)>();

    let (
        overload_rets_field_idents,
        overload_rets_field_types,
    ) = cmd_enum.commands.iter()
        .map(|cmd| cmd.overloads.iter()
            .map(|usage| match &usage.rets {
                Some(rets) => rets.rets.iter()
                    .map(|ret| (
                        ret.ident.as_ref().expect("Field::parse_named should ensure all idents are Some"),
                        &ret.ty,
                    ))
                    .collect::<(Vec<_>, Vec<_>)>(),
                None => (Vec::new(), Vec::new()),
            })
            .collect::<(Vec<_>, Vec<_>)>()
        )
        .collect::<(Vec<_>, Vec<_>)>();

    quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #vis enum #ident {
            #(
                #[doc = #help_general]
                #commands
            ),*
        }

        impl #ident {
            pub const LIST: [Self; #num_commands] = [
                #(Self::#commands),*
            ];

            pub const fn input(&self) -> &'static str {
                match self {
                    #(Self::#commands => #inputs),*
                }
            }

            pub const fn help_msg(&self) -> &'static str {
                match self {
                    #(Self::#commands => #help_general),*
                }
            }
        }

        impl std::str::FromStr for #ident {
            type Err = CmdError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #(#inputs => Ok(Self::#commands),)*
                    _ => Err(CmdError::NoSuchCmd(s.to_string())),
                }
            }
        }

        impl std::fmt::Display for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.input().fmt(f)
            }
        }

        mod cmd {
            use super::*;

            #(
                #(
                    pub struct #overload_rets_struct {
                        #(#overload_rets_field_idents: #overload_rets_field_types),*
                    }
                )

                pub enum #commands {
                    #(#overload_pascal),*
                }

                // impl #commands {
                //     #(
                //         pub fn #overloads(
                //             cout: &mut ConsoleOut,
                //             cin: &mut ConsoleIn,
                //             data: &mut ProgramData,
                //             #overload_args
                //         ) -> #overload_rets_struct {
                //             let ( #(#overload_rets_field_idents),* ) = #overload_blocks;
                //             #overload_rets_struct { #(#overload_rets_field_idents),* }
                //         }
                //     )*
                // }
            )*
        }
    }.into()
}

#[derive(Debug)]
struct CmdEnumData {
    vis: Visibility,
    enum_token: Token![enum],
    ident: Ident,
    brace_token: token::Brace,
    commands: Vec<CmdData>,
}

impl Parse for CmdEnumData {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            vis: input.parse()?,
            enum_token: input.parse()?,
            ident: input.parse()?,
            brace_token: braced!(content in input),
            commands: {
                let mut commands = Vec::new();
                while !content.is_empty() {
                    commands.push(content.parse()?);
                }
                commands
            },
        })
    }
}

type CmdName = Punctuated<Ident, Token![.]>;

#[derive(Debug)]
struct CmdData {
    attrs: Vec<Attribute>,
    name: CmdName,
    brace_token: token::Brace,
    overloads: Vec<UsageData>,
}

impl Parse for CmdData {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            name: CmdName::parse_separated_nonempty(input)?,
            brace_token: braced!(content in input),
            overloads: {
                let mut overloads = Vec::new();
                while !content.is_empty() {
                    overloads.push(content.parse()?);
                }
                overloads
            },
        })
    }
}

#[derive(Debug)]
struct UsageData {
    attrs: Vec<Attribute>,
    fn_token: Token![fn],
    ident: Ident,
    par_token: token::Paren,
    args: Punctuated<Field, Token![,]>,
    rets: Option<Return>,
    fat_arrow: Token![=>],
    definition: Block,
    semi: Token![;],
}

impl Parse for UsageData {
    fn parse(input: ParseStream) -> Result<Self> {
        let par_content;
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            fn_token: input.parse()?,
            ident: input.parse()?,
            par_token: parenthesized!(par_content in input),
            args: par_content.parse_terminated(Field::parse_named, Token![,])?,
            rets: if input.peek(Token![->]) { Some(input.parse()?) } else { None },
            fat_arrow: input.parse()?,
            definition: input.parse()?,
            semi: input.parse()?,
        })
    }
}

#[derive(Debug, Default)]
struct Return {
    arrow: Token![->],
    paren_token: token::Paren,
    rets: Punctuated<Field, Token![,]>,
}

impl Parse for Return {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            arrow: input.parse()?,
            paren_token: parenthesized!(content in input),
            rets: content.parse_terminated(Field::parse_named, Token![,])?,
        })
    }
}
