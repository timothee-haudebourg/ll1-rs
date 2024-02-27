use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{format_ident, quote};
use std::collections::HashMap;
use syn::spanned::Spanned;

use crate::r#type::Type;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot derive parser for `union` type")]
    Union(Span),

    #[error("invalid `parse` attribute")]
    InvalidAttribute(Span),

    #[error(transparent)]
    Syntax(#[from] syn::Error),

    #[error("ambiguous rule")]
    AmbiguousRule(Span),

    #[error(transparent)]
    UnsupportedType(#[from] crate::r#type::Unsupported),
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Self::Union(s) => *s,
            Self::InvalidAttribute(s) => *s,
            Self::Syntax(s) => s.span(),
            Self::AmbiguousRule(s) => *s,
            Self::UnsupportedType(e) => e.0,
        }
    }
}

pub fn derive(input: syn::DeriveInput) -> Result<TokenStream, Error> {
    let (attrs, _) = parse_attributes(input.attrs)?;
    let ident = &input.ident;

    let get_start;
    let get_end;
    let rules = match input.data {
        syn::Data::Enum(e) => {
            let mut rules = Vec::with_capacity(e.variants.len());

            for v in e.variants {
                rules.push(Rule {
                    ident: v.ident.clone(),
                    variant_name: Some(v.ident),
                    args: v.fields.try_into()?,
                })
            }

            let start_cases = rules.iter().map(|r| {
                let (args, to_start) = r.get_start();
                let ident = &r.ident;

                quote!(Self::#ident #args => { #to_start })
            });

            get_start = quote! {
                match self {
                    #(#start_cases),*
                }
            };

            let end_cases = rules.iter().map(|r| {
                let (args, to_end) = r.get_end();
                let ident = &r.ident;

                quote!(Self::#ident #args => { #to_end })
            });

            get_end = quote! {
                match self {
                    #(#end_cases),*
                }
            };

            rules
        }
        syn::Data::Struct(s) => {
            let rule = Rule {
                ident: ident.clone(),
                variant_name: None,
                args: s.fields.try_into()?,
            };

            let (args, to_start) = rule.get_start();
            get_start = quote! {
                let Self #args = self;
                #to_start
            };

            let (args, to_end) = rule.get_end();
            get_end = quote! {
                let Self #args = self;
                #to_end
            };

            vec![rule]
        }
        syn::Data::Union(_) => return Err(Error::Union(ident.span())),
    };

    let grammar = Grammar {
        token_type: attrs.token_type,
        rules,
    };

    let token_type = &grammar.token_type;

    let ty_gens = input.generics.split_for_impl().1;

    let parse_generics = parse_generics(&input.generics, token_type);
    let (parse_impl_gens, _, parse_where) = parse_generics.split_for_impl();

    let context_generics = context_generics(&input.generics, token_type);
    let (context_impl_gens, _, context_where) = context_generics.split_for_impl();

    let span_generics = span_generics(&input.generics);
    let (span_impl_gens, _, span_where) = span_generics.split_for_impl();

    let root = Node::new(
        &grammar,
        ident,
        &context_generics,
        (0..grammar.rules.len()).collect(),
        0,
    )?;
    let contexts = root.generate_contexts(&grammar);

    let parse_body = root.generate(&grammar, 0);
    let root_context = &root.context_ident;

    Ok(quote! {
        #contexts

        impl #parse_impl_gens ::ll1::Parse for #ident #ty_gens #parse_where {
            fn parse_in<_I: Iterator<Item = Result<::ll1::DecodedChar, _E>>, _E>(
                context: &::ll1::Context<#token_type>,
                tokens: &mut ::ll1::Lexer<#token_type, _I>
            ) -> Result<Self, ::ll1::ParseError<#token_type, _E>> {
                #parse_body
            }
        }

        impl #context_impl_gens ::ll1::LocalContext for #ident #ty_gens #context_where {
            type Token = #token_type;

            fn accepts(context: &::ll1::Context<#token_type>, token: Option<&#token_type>) -> bool {
                <#root_context #ty_gens as ::ll1::LocalContext>::accepts(context, token)
            }
        }

        impl #span_impl_gens ::ll1::Spanned for #ident #ty_gens #span_where {
            fn start(&self) -> Option<usize> {
                #get_start
            }

            fn end(&self) -> Option<usize> {
                #get_end
            }
        }
    })
}

fn parse_generics(generics: &syn::Generics, token_type: &syn::Type) -> syn::Generics {
    let mut result = generics.clone();

    for p in &mut result.params {
        if let syn::GenericParam::Type(p) = p {
            if p.colon_token.is_none() {
                p.colon_token = Some(syn::token::Colon::default());
            }

            p.bounds
                .push(syn::parse2(quote!(::ll1::Parse<Token = #token_type>)).unwrap());
        }
    }

    result
}

fn context_generics(generics: &syn::Generics, token_type: &syn::Type) -> syn::Generics {
    let mut result = generics.clone();

    for p in &mut result.params {
        if let syn::GenericParam::Type(p) = p {
            if p.colon_token.is_none() {
                p.colon_token = Some(syn::token::Colon::default());
            }

            p.bounds
                .push(syn::parse2(quote!(::ll1::LocalContext<Token = #token_type>)).unwrap());
        }
    }

    result
}

fn span_generics(generics: &syn::Generics) -> syn::Generics {
    let mut result = generics.clone();

    for p in &mut result.params {
        if let syn::GenericParam::Type(p) = p {
            if p.colon_token.is_none() {
                p.colon_token = Some(syn::token::Colon::default());
            }

            p.bounds.push(syn::parse2(quote!(::ll1::Spanned)).unwrap());
        }
    }

    result
}

pub struct Grammar {
    token_type: syn::Type,
    rules: Vec<Rule>,
}

pub struct Rule {
    ident: Ident,
    variant_name: Option<Ident>,
    args: Args,
}

impl Rule {
    pub fn generate_construction(&self) -> TokenStream {
        let variant = match &self.variant_name {
            Some(v) => quote!(Self::#v),
            None => quote!(Self),
        };

        match &self.args {
            Args::Unit => variant,
            Args::Named(fields) => {
                let args = fields.iter().enumerate().map(|(i, (name, _))| {
                    let arg = format_ident!("arg_{i}");
                    quote!(#name: #arg)
                });

                quote!(#variant { #(#args),* })
            }
            Args::Unnamed(fields) => {
                let args = (0..fields.len()).map(|i| format_ident!("arg_{i}"));

                quote!(#variant ( #(#args),* ))
            }
        }
    }

    pub fn arg(&self, i: usize) -> Option<&Type> {
        match &self.args {
            Args::Unit => None,
            Args::Named(fields) => fields.get(i).map(|(_, ty)| ty),
            Args::Unnamed(fields) => fields.get(i),
        }
    }

    pub fn get_start(&self) -> (TokenStream, TokenStream) {
        match &self.args {
            Args::Unit => (quote!(), quote!(None)),
            Args::Named(fields) => {
                let build_start = fields.iter().map(|(id, ty)| {
                    quote! {
                        if let Some(start) = <#ty as ::ll1::Spanned>::start(#id) {
                            return Some(start)
                        }
                    }
                });

                let args = fields.iter().map(|(id, _)| quote!(#id));

                (
                    quote! {
                        { #(#args),* }
                    },
                    quote! {
                        #(#build_start)*
                        None
                    },
                )
            }
            Args::Unnamed(fields) => {
                let build_start = fields.iter().enumerate().map(|(i, ty)| {
                    let id = format_ident!("arg_{i}");
                    quote! {
                        if let Some(start) = <#ty as ::ll1::Spanned>::start(#id) {
                            return Some(start)
                        }
                    }
                });

                let args = (0..fields.len()).map(|i| {
                    let id = format_ident!("arg_{i}");
                    quote!(#id)
                });

                (
                    quote! {
                        ( #(#args),* )
                    },
                    quote! {
                        #(#build_start)*
                        None
                    },
                )
            }
        }
    }

    pub fn get_end(&self) -> (TokenStream, TokenStream) {
        match &self.args {
            Args::Unit => (quote!(), quote!(None)),
            Args::Named(fields) => {
                let build_start = fields.iter().rev().map(|(id, ty)| {
                    quote! {
                        if let Some(start) = <#ty as ::ll1::Spanned>::start(#id) {
                            return Some(start)
                        }
                    }
                });

                let args = fields.iter().map(|(id, _)| quote!(#id));

                (
                    quote! {
                        { #(#args),* }
                    },
                    quote! {
                        #(#build_start)*
                        None
                    },
                )
            }
            Args::Unnamed(fields) => {
                let build_start = fields.iter().enumerate().rev().map(|(i, ty)| {
                    let id = format_ident!("arg_{i}");
                    quote! {
                        if let Some(start) = <#ty as ::ll1::Spanned>::start(#id) {
                            return Some(start)
                        }
                    }
                });

                let args = (0..fields.len()).map(|i| {
                    let id = format_ident!("arg_{i}");
                    quote!(#id)
                });

                (
                    quote! {
                        ( #(#args),* )
                    },
                    quote! {
                        #(#build_start)*
                        None
                    },
                )
            }
        }
    }
}

pub enum Args {
    Unit,
    Named(Vec<(Ident, Type)>),
    Unnamed(Vec<Type>),
}

impl TryFrom<syn::Fields> for Args {
    type Error = crate::r#type::Unsupported;

    fn try_from(value: syn::Fields) -> Result<Self, Self::Error> {
        match value {
            syn::Fields::Unit => Ok(Self::Unit),
            syn::Fields::Named(f) => Ok(Self::Named(
                f.named
                    .into_iter()
                    .map(|f| Ok((f.ident.unwrap(), f.ty.try_into()?)))
                    .collect::<Result<_, _>>()?,
            )),
            syn::Fields::Unnamed(f) => Ok(Self::Unnamed(
                f.unnamed
                    .into_iter()
                    .map(|f| f.ty.try_into())
                    .collect::<Result<_, _>>()?,
            )),
        }
    }
}

pub struct Node {
    context_ident: Ident,
    context_generics: syn::Generics,
    children: HashMap<Type, Self>,
    default_rule: Option<usize>,
}

impl Node {
    pub fn new(
        grammar: &Grammar,
        base_ident: &Ident,
        context_generics: &syn::Generics,
        rules: Vec<usize>,
        position: usize,
    ) -> Result<Self, Error> {
        let mut map: HashMap<Type, Vec<usize>> = HashMap::new();
        let mut default_rule = None;

        let repr = rules.get(0).copied();
        for r in rules {
            let rule = &grammar.rules[r];
            match rule.arg(position) {
                Some(ty) => {
                    map.entry(ty.clone()).or_default().push(r);
                }
                None => {
                    if default_rule.replace(r).is_some() {
                        return Err(Error::AmbiguousRule(rule.ident.span()));
                    }
                }
            }
        }

        let mut children = HashMap::new();
        for (ty, rules) in map {
            children.insert(
                ty,
                Self::new(grammar, base_ident, context_generics, rules, position + 1)?,
            );
        }

        Ok(Self {
            context_ident: match repr {
                Some(repr) => format_ident!("_{base_ident}_{repr}_{position}"),
                None => format_ident!("_{base_ident}_{position}")
            },
            context_generics: context_generics.clone(),
            children,
            default_rule,
        })
    }

    pub fn generate(&self, grammar: &Grammar, depth: u32) -> TokenStream {
        let arg = format_ident!("arg_{depth}");
        let cases = self.children.iter().map(|(ty, n)| {
			let rest = n.generate(grammar, depth + 1);
			let node_context = &n.context_ident;
			let context_ty_generics = n.context_generics.split_for_impl().1;

			quote! {
				let rule_context = context.with::<#node_context #context_ty_generics>();
				if <#ty as ::ll1::LocalContext>::accepts(&rule_context, next_token) {
					let #arg = <#ty as ::ll1::Parse>::parse_in(&rule_context, tokens)?;
					#rest
				}
			}
		});

        let node_context = &self.context_ident;
		let context_ty_generics = self.context_generics.split_for_impl().1;

        let finalize = match self.default_rule {
            Some(i) => {
                let construction = grammar.rules[i].generate_construction();
                quote!(return Ok(#construction))
            }
            None => {
                quote! {
                    let (offset, token) = tokens.next_token().map_err(::ll1::ParseError::Lexing)?;
                    return Err(::ll1::ParseError::UnexpectedToken(offset, token))
                }
            }
        };

        quote! {
            let next_context = context.with::<#node_context #context_ty_generics>();
            let next_token = tokens.peek_in(&next_context).map_err(::ll1::ParseError::Lexing)?;

            #(#cases)*
            #finalize
        }
    }

    fn generate_contexts(&self, grammar: &Grammar) -> TokenStream {
        let mut tokens = TokenStream::new();

        tokens.extend(self.generate_context(grammar));
        for n in self.children.values() {
            tokens.extend(n.generate_contexts(grammar));
        }

        tokens
    }

    fn generate_context(&self, grammar: &Grammar) -> TokenStream {
        let ident = &self.context_ident;

        let generics = &self.context_generics;
        let (impl_gens, ty_gens, where_clauses) = self.context_generics.split_for_impl();

        let cases = self.children.iter().map(|(ty, n)| {
            let node_context = &n.context_ident;

            quote! {
                let sub_context = context.with::<#node_context #ty_gens>();
                if <#ty as ::ll1::LocalContext>::accepts(&sub_context, token) {
                    return true
                }
            }
        });

        // let bounds = self.generate_context_bounds();

        let token_type = &grammar.token_type;

        let mut ty_args = Vec::new();
        for p in &generics.params {
            if let syn::GenericParam::Type(p) = p {
                let id = &p.ident;
                ty_args.push(quote!(::core::marker::PhantomData<#id>))
            }
        }

        let args = if ty_args.is_empty() {
            None
        } else {
            Some(quote!(( #(#ty_args),* )))
        };

        quote! {
            struct #ident #generics #args;

            impl #impl_gens ::ll1::LocalContext for #ident #ty_gens #where_clauses {
                type Token = #token_type;

                fn accepts(context: &::ll1::Context<#token_type>, token: Option<&#token_type>) -> bool {
                    #(#cases)*
                    context.accepts(token)
                }
            }
        }
    }

    // fn collect_dependencies<'a>(&'a self, result: &mut Vec<&'a Type>) {
    // 	for (ty, n) in &self.children {
    // 		result.push(ty);
    // 		n.collect_dependencies(result)
    // 	}
    // }

    // fn generate_context_bounds(&self) -> Vec<TokenStream> {
    // 	let mut deps = Vec::new();
    // 	self.collect_dependencies(&mut deps);
    // 	deps.sort_unstable();
    // 	deps.dedup();

    // 	deps.into_iter().map(|d| {
    // 		quote!(#d: ::ll1::LocalContext<_Token>)
    // 	}).collect()
    // }

    // fn generate_parse_bounds(&self) -> Vec<TokenStream> {
    // 	let mut deps = Vec::new();
    // 	self.collect_dependencies(&mut deps);
    // 	deps.sort_unstable();
    // 	deps.dedup();

    // 	deps.into_iter().map(|d| {
    // 		quote!(#d: ::ll1::Parse<_Token>)
    // 	}).collect()
    // }
}

pub struct GrammarAttributes {
    token_type: syn::Type,
}

fn parse_attributes(
    attrs: Vec<syn::Attribute>,
) -> Result<(GrammarAttributes, Vec<syn::Attribute>), Error> {
    let mut rest = Vec::with_capacity(attrs.len());
    let mut token_type: syn::Type = syn::parse2(quote!(Token)).unwrap();

    for attr in attrs {
        if attr.path().is_ident("parse") {
            let span = attr.span();
            match attr.meta {
                syn::Meta::List(list) => {
                    let mut tokens = list.tokens.into_iter();
                    while let Some(token) = tokens.next() {
                        let span = token.span();
                        match token {
                            TokenTree::Ident(ident) if ident == "token" => match tokens.next() {
                                Some(TokenTree::Group(group)) => {
                                    token_type = syn::parse2(group.stream())?;
                                }
                                _ => return Err(Error::InvalidAttribute(span)),
                            },
                            TokenTree::Punct(p) if p.as_char() == ',' => (),
                            _ => return Err(Error::InvalidAttribute(span)),
                        }
                    }
                }
                _ => return Err(Error::InvalidAttribute(span)),
            }
        } else {
            rest.push(attr)
        }
    }

    Ok((GrammarAttributes { token_type }, rest))
}
