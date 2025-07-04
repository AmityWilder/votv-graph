#![allow(unused)]

use proc_macro::TokenStream;
use quote::quote;
use syn::{braced, bracketed, parenthesized, parse::{Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, spanned::Spanned, token, Attribute, Error, Expr, ExprBlock, Field, Ident, Item, Lit, LitStr, MetaNameValue, Result, Token};

#[proc_macro]
pub fn cmd(input: TokenStream) -> TokenStream {
    let cmd_data = parse_macro_input!(input as CmdEnumData);
    let ident = &cmd_data.ident;

    let inputs = cmd_data.commands.iter()
        .map(|cmd|
            cmd.name.iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<_>>()
                .join(".")
        )
        .collect::<Vec<_>>();

    let variants = cmd_data.commands.iter()
        .map(|cmd|
            Ident::new(
                &cmd.name.iter()
                    .map(|ident| {
                        let mut s = ident.to_string();
                        if let Some(ch) = s.chars().next() {
                            s.replace_range(
                                0..ch.len_utf8(),
                                &ch.to_uppercase().collect::<String>(),
                            );
                        }
                        s
                    })
                    .collect::<Vec<_>>()
                    .concat(),
                cmd.name.span(),
            )
        )
        .collect::<Vec<_>>();

    quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        enum #ident {
            #(#variants),*
        }
        impl #ident {
            pub const fn input(&self) -> &'static str {
                match self {
                    #(Self::#variants => #inputs),*
                }
            }
        }
        impl FromStr for #ident {
            type Err = CmdError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #(#inputs => Ok(Self::#variants),)*
                    _ => Err(CmdError::NoSuchCmd(s.to_string())),
                }
            }
        }
        impl std::fmt::Display for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.input().fmt(f)
            }
        }
    }.into()
}

/// ```ignore
/// cmd!{
///     enum Foo {
///         // commands
///     }
/// }
/// ```
#[derive(Debug)]
struct CmdEnumData {
    enum_token: Token![enum],
    ident: Ident,
    brace_token: token::Brace,
    commands: Vec<CmdData>,
}

impl Parse for CmdEnumData {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
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

/// ```ignore
/// sv.mango {
///     // overloads
/// }
/// ```
#[derive(Debug)]
struct CmdData {
    help: Vec<LitStr>,
    name: CmdName,
    brace_token: token::Brace,
    overloads: Vec<UsageData>,
}

impl Parse for CmdData {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            help: {
                let attrs = Attribute::parse_outer(input)?;
                attrs.into_iter()
                    .map(|attr| {
                        let name_value = attr.meta.require_name_value()?;
                        if name_value.path.is_ident("doc") {
                            if let Expr::Lit(syn::ExprLit { lit: Lit::Str(doc), .. }) = &name_value.value {
                                Ok(doc.clone())
                            } else {
                                Err(Error::new_spanned(&name_value.value, "doc comment must be string literal"))
                            }
                        } else {
                            Err(Error::new_spanned(attr, "must be doc comment"))
                        }
                    })
                    .collect::<Result<_>>()?
            },
            name: CmdName::parse_separated_nonempty(input)?,
            brace_token: braced!(content in input),
            overloads: {
                let mut overloads = Vec::new();
                while content.peek(token::Paren) {
                    overloads.push(content.parse()?);
                }
                overloads
            },
        })
    }
}

/// ```ignore
/// (/* comma-terminated fields */) => {
///     // expr
/// }
/// ```
#[derive(Debug)]
struct UsageData {
    par_token: token::Paren,
    fields: Punctuated<Field, Token![,]>,
    fat_arrow: Token![=>],
    definition: ExprBlock,
    semi: Token![;],
}

impl Parse for UsageData {
    fn parse(input: ParseStream) -> Result<Self> {
        let par_content;
        Ok(Self {
            par_token: parenthesized!(par_content in input),
            fields: par_content.parse_terminated(Field::parse_named, Token![,])?,
            fat_arrow: input.parse()?,
            definition: input.parse()?,
            semi: input.parse()?,
        })
    }
}
