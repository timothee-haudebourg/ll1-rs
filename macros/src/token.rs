use btree_range_map::{AnyRange, RangeSet};
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use std::{collections::HashMap, str::FromStr};
use syn::spanned::Spanned;

const IDENT_REGEXP: &str = "[a-zA-Z_][a-zA-Z0-9_]*";
const WHITESPACE_REGEXP: &str = "[\\s\\t\\r\\n]+";

#[derive(Debug, thiserror::Error)]
pub enum TokenDeriveError {
	#[error("expected enum type")]
	ExpectedEnum(Span),

	#[error("missing token value")]
	MissingValue(Span),

	#[error("invalid token attribute")]
	InvalidAttribute(Span),

	#[error("ambiguous with the `{1}` token")]
	Ambiguity(Span, Ident),

	#[error("invalid regular expression: {1}")]
	RegExp(Span, iregex_syntax::Error),

	#[error("cannot skip empty token")]
	SkipEmpty(Span),

	#[error(transparent)]
	Parse(#[from] syn::Error),
}

impl TokenDeriveError {
	pub fn span(&self) -> Span {
		match self {
			Self::ExpectedEnum(span) => *span,
			Self::MissingValue(span) => *span,
			Self::InvalidAttribute(span) => *span,
			Self::Ambiguity(span, _) => *span,
			Self::RegExp(span, _) => *span,
			Self::SkipEmpty(span) => *span,
			Self::Parse(e) => e.span(),
		}
	}
}

pub enum Value {
	Whitespace,
	Ident,
	Const(String),
	RegExp(iregex_syntax::Disjunction),
}

impl Value {
	/// Builds the automaton represented by this value.
	pub fn build_automaton(&self) -> iregex::automata::NFA {
		let mut state_builder = iregex::automata::nfa::U32StateBuilder::new();
		iregex::IRegEx::anchored(self.build())
			.compile(&mut state_builder)
			.unwrap()
			.root
			.unwrap()
			.unwrap()
			.untagged
	}

	/// Builds the automaton represented by this value.
	pub fn build(&self) -> iregex::Alternation {
		match self {
			Self::Whitespace => iregex_syntax::Disjunction::from_str(WHITESPACE_REGEXP)
				.unwrap()
				.build(),
			Self::Ident => iregex_syntax::Disjunction::from_str(IDENT_REGEXP)
				.unwrap()
				.build(),
			Self::Const(s) => iregex_syntax::Sequence::from_iter(s.chars().map(|c| {
				let mut set = RangeSet::new();
				set.insert(c);
				iregex_syntax::Atom::Set(set.into())
			}))
			.into_disjunction()
			.build(),
			Self::RegExp(e) => e.build(),
		}
	}
}

pub struct TokenAttributes {
	value: Value,
	skip: bool,
}

fn parse_attributes(
	attrs: Vec<syn::Attribute>,
	span: Span,
) -> Result<(TokenAttributes, Vec<syn::Attribute>), TokenDeriveError> {
	let mut rest = Vec::with_capacity(attrs.len());
	let mut value = None;
	let mut skip = false;

	for attr in attrs {
		if attr.path().is_ident("token") {
			let span = attr.span();
			match attr.meta {
				syn::Meta::List(list) => {
					let mut tokens = list.tokens.into_iter();
					while let Some(token) = tokens.next() {
						let span = token.span();
						match token {
							TokenTree::Ident(ident) if ident == "skip" => skip = true,
							TokenTree::Ident(ident) if ident == "regexp" => match tokens.next() {
								Some(TokenTree::Group(group)) => {
									let span = group.span();
									let s: syn::LitStr = syn::parse2(group.stream())?;
									value = Some(Value::RegExp(
										s.value()
											.parse()
											.map_err(|e| TokenDeriveError::RegExp(span, e))?,
									))
								}
								_ => return Err(TokenDeriveError::InvalidAttribute(span)),
							},
							TokenTree::Ident(ident) if ident == "whitespace" => {
								value = Some(Value::Whitespace)
							}
							TokenTree::Ident(ident) if ident == "ident" => {
								value = Some(Value::Ident)
							}
							TokenTree::Literal(l) => {
								let s: syn::LitStr = syn::parse2(l.into_token_stream())?;
								value = Some(Value::Const(s.value()))
							}
							TokenTree::Punct(p) if p.as_char() == ',' => (),
							_ => return Err(TokenDeriveError::InvalidAttribute(span)),
						}
					}
				}
				_ => return Err(TokenDeriveError::InvalidAttribute(span)),
			}
		} else {
			rest.push(attr)
		}
	}

	match value {
		Some(value) => Ok((TokenAttributes { value, skip }, rest)),
		None => Err(TokenDeriveError::MissingValue(span)),
	}
}

pub fn derive(input: syn::DeriveInput) -> Result<TokenStream, TokenDeriveError> {
	let kind_ident = &input.ident;
	let ident = Ident::new("Token", kind_ident.span());
	let vis = &input.vis;

	match input.data {
		syn::Data::Enum(e) => {
			let mut output = TokenStream::new();
			let mut automaton: iregex::automata::NFA<TokenState> = iregex::automata::NFA::new();
			let mut tokens = Vec::with_capacity(e.variants.len());
			let mut capture = false;

			for v in e.variants {
				let span = v.span();
				let (token_attrs, attrs) = parse_attributes(v.attrs, span)?;
				let token_automaton = token_attrs.value.build_automaton();

				if token_attrs.skip && token_automaton.recognizes_empty() {
					return Err(TokenDeriveError::SkipEmpty(span));
				}

				let const_ = token_automaton.to_singleton().map(String::from_iter);
				let is_const = const_.is_some();

				let token = Token {
					kind_ident: v.ident.clone(),
					ident: if token_attrs.skip {
						None
					} else {
						Some(v.ident.clone())
					},
					const_,
					capture: !token_attrs.skip && !is_const,
				};

				capture |= token.capture;

				let kind_ident = &token.kind_ident;

				let as_str = match &token.const_ {
					Some(c) => quote!(#c),
					None => quote!(self.1.as_str()),
				};

				let into_str = match &token.const_ {
					Some(c) => quote!(#c.to_owned()),
					None => quote!(self.1),
				};

				if let Some(token_ident) = &token.ident {
					let arg_types = token.arg_types();
					let arg_wildcards = token.arg_wildcards();
					let args = token.args();

					output.extend(quote! {
						#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
						#(#attrs)*
						pub struct #token_ident #arg_types ;

						impl #token_ident {
							pub fn as_str(&self) -> &str {
								#as_str
							}

							pub fn into_string(self) -> std::string::String {
								#into_str
							}
						}

						impl AsRef<str> for #token_ident {
							fn as_ref(&self) -> &str {
								self.as_str()
							}
						}

						impl ::core::borrow::Borrow<str> for #token_ident {
							fn borrow(&self) -> &str {
								self.as_str()
							}
						}

						impl ::core::fmt::Display for #token_ident {
							fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
								self.as_str().fmt(f)
							}
						}
					});

					output.extend(quote! {
                        impl ::ll1::LocalContext for #token_ident {
                            type Token = #ident;

                            fn accepts(context: &::ll1::Context<#ident>, token: Option<&#ident>) -> bool {
                                matches!(token, Some(#ident::#kind_ident #arg_wildcards))
                            }
                        }

                        impl ::ll1::Parse for #token_ident {
                            fn parse_in<_I: Iterator<Item = Result<::ll1::DecodedChar, _E>>, _E>(context: &::ll1::Context<#ident>, tokens: &mut ::ll1::Lexer<#ident, _I>) -> Result<Self, ::ll1::ParseError<#ident, _E>> {
                                let context = context.with::<Self>();
                                match tokens.next_token_in(&context).map_err(::ll1::ParseError::Lexing)? {
                                    (_, Some(#ident::#kind_ident #args)) => Ok(Self #args),
                                    (offset, Some(token)) => Err(::ll1::ParseError::UnexpectedToken(offset, Some(token))),
                                    (offset, None) => Err(::ll1::ParseError::UnexpectedToken(offset, None))
                                }
                            }
                        }

                        impl ::ll1::Spanned for #token_ident {
                            fn start(&self) -> Option<usize> {
                                Some(self.0.start())
                            }

                            fn end(&self) -> Option<usize> {
                                Some(self.0.end())
                            }

                            fn span(&self) -> ::ll1::Span {
                                self.0
                            }
                        }
                    });
				}

				automaton.mapped_union(token_automaton, |q| TokenState {
					token: tokens.len(),
					state: q,
				});

				tokens.push(token);
			}

			let mut det_states = Vec::new();
			let mut det_map = HashMap::new();
			let det_automaton = automaton.determinize(|states| {
				*det_map.entry(states.clone()).or_insert_with(|| {
					let i = det_states.len() as u32;
					det_states.push(states.clone());
					i
				})
			});

			let initial_state = *det_automaton.initial_state();

			let capture_char = if capture {
				Some(quote!(buffer.push(next_char.unwrap());))
			} else {
				None
			};

			let clear_buffer = if capture {
				Some(quote!(buffer.clear();))
			} else {
				None
			};

			let mut cases = Vec::with_capacity(det_states.len());
			for (i, states) in det_states.iter().enumerate() {
				let i = i as u32;
				let mut final_state = None;
				for &&q in states {
					if automaton.is_final_state(&q) {
						if let Some(r) = final_state.replace(q) {
							let q_token = &tokens[q.token];
							let r_token = &tokens[r.token];

							if q_token.capture == r_token.capture {
								return Err(TokenDeriveError::Ambiguity(
									tokens[q.token].ident.span(),
									tokens[r.token].kind_ident.clone(),
								));
							} else if !r_token.capture {
								final_state = Some(r) // the `r` token is more precise.
							}
						}
					}
				}

				let transition_cases: Vec<_> = det_automaton
					.transitions_from(&i)
					.map(|(range, r)| {
						let rust_range = rust_range(*range);
						quote! {
							Some(#rust_range) => {
								#capture_char
								end += chars.next().unwrap().ok().unwrap().len();
								#r
							}
						}
					})
					.collect();

				let finalize = match final_state {
					Some(q) => {
						let token = &tokens[q.token];

						match &token.ident {
							Some(token_ident) => {
								if token.capture {
									quote! {
										break Ok(Some(Self::#token_ident(::ll1::locspan::Span::new(offset, end), ::core::mem::take(&mut buffer))))
									}
								} else {
									quote! {
										break Ok(Some(Self::#token_ident(::ll1::locspan::Span::new(offset, end))))
									}
								}
							}
							None => {
								quote! {
									offset = end;
									#clear_buffer
									#initial_state
								}
							}
						}
					}
					None => {
						if initial_state == i {
							quote! {
								break Ok(None)
							}
						} else {
							quote! {
								break Err(::ll1::LexError::Unexpected(end, next_char))
							}
						}
					}
				};

				let case = if transition_cases.is_empty() {
					finalize
				} else {
					quote! {
						match next_char {
							#(#transition_cases,)*
							_ => { #finalize }
						}
					}
				};

				cases.push(quote! {
					#i => #case
				})
			}

			let kind_variants = tokens.iter().filter_map(|t| {
				t.ident.as_ref().map(|token_ident| {
					if t.capture {
						quote! {
							Self::#token_ident(_, _) => #kind_ident::#token_ident
						}
					} else {
						quote! {
							Self::#token_ident(_) => #kind_ident::#token_ident
						}
					}
				})
			});

			let span_variants = tokens.iter().filter_map(|t| {
				t.ident.as_ref().map(|token_ident| {
					if t.capture {
						quote! {
							Self::#token_ident(span, _) => *span
						}
					} else {
						quote! {
							Self::#token_ident(span) => *span
						}
					}
				})
			});

			let start_variants = tokens.iter().filter_map(|t| {
				t.ident.as_ref().map(|token_ident| {
					if t.capture {
						quote! {
							Self::#token_ident(span, _) => Some(span.start())
						}
					} else {
						quote! {
							Self::#token_ident(span) => Some(span.start())
						}
					}
				})
			});

			let end_variants = tokens.iter().filter_map(|t| {
				t.ident.as_ref().map(|token_ident| {
					if t.capture {
						quote! {
							Self::#token_ident(span, _) => Some(span.end())
						}
					} else {
						quote! {
							Self::#token_ident(span) => Some(span.end())
						}
					}
				})
			});

			let variants = tokens.iter().filter_map(|t| {
				t.ident.as_ref().map(|token_ident| {
					if t.capture {
						quote! {
							#token_ident(::ll1::locspan::Span, ::std::string::String)
						}
					} else {
						quote! {
							#token_ident(::ll1::locspan::Span)
						}
					}
				})
			});

			let as_str_cases = tokens.iter().filter_map(|t| {
				t.ident.as_ref().map(|token_ident| match &t.const_ {
					Some(c) => quote! {
						Self::#token_ident(_) => #c
					},
					None => quote! {
						Self::#token_ident(_, buffer) => buffer.as_str()
					},
				})
			});

			let into_str_cases = tokens.iter().filter_map(|t| {
				t.ident.as_ref().map(|token_ident| match &t.const_ {
					Some(c) => quote! {
						Self::#token_ident(_) => #c.to_owned()
					},
					None => quote! {
						Self::#token_ident(_, buffer) => buffer
					},
				})
			});

			let init_buffer = if capture {
				Some(quote!(let mut buffer = ::std::string::String::new();))
			} else {
				None
			};

			output.extend(quote! {
                #[derive(Debug)]
                #vis enum #ident {
                    #(#variants),*
                }

                impl #ident {
                    pub fn kind(&self) -> #kind_ident {
                        match self {
                            #(#kind_variants,)*
                        }
                    }

                    pub fn into_kind(self) -> #kind_ident {
                        self.kind()
                    }

                    pub fn as_str(&self) -> &str {
                        match self {
                            #(#as_str_cases,)*
                        }
                    }

                    pub fn into_string(self) -> ::std::string::String {
                        match self {
                            #(#into_str_cases,)*
                        }
                    }
                }

                impl ::ll1::Spanned for #ident {
                    fn start(&self) -> Option<usize> {
                        match self {
                            #(#start_variants,)*
                        }
                    }

                    fn end(&self) -> Option<usize> {
                        match self {
                            #(#end_variants,)*
                        }
                    }

                    fn span(&self) -> ::ll1::Span {
                        match self {
                            #(#span_variants,)*
                        }
                    }
                }

                impl ::ll1::Token for #ident {
                    fn parse_from<E>(mut offset: usize, chars: &mut ::std::iter::Peekable<impl Iterator<Item = Result<::ll1::decoded_char::DecodedChar, E>>>) -> Result<Option<Self>, ::ll1::LexError<E>> {
                        let mut state = #initial_state;
                        let mut end = offset;
                        #init_buffer

                        loop {
                            let next_char = match chars.peek() {
                                Some(Ok(c)) => Some(c.chr()),
                                Some(Err(_)) => break Err(::ll1::LexError::Stream(end, chars.next().unwrap().unwrap_err())),
                                None => None
                            };

                            state = match state {
                                #(#cases,)*
                                _ => unreachable!()
                            };
                        }
                    }
                }

                impl AsRef<str> for #ident {
                    fn as_ref(&self) -> &str {
                        self.as_str()
                    }
                }

                impl ::core::borrow::Borrow<str> for #ident {
                    fn borrow(&self) -> &str {
                        self.as_str()
                    }
                }

                impl ::core::fmt::Display for #ident {
                    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                        self.as_str().fmt(f)
                    }
                }
            });

			Ok(output)
		}
		_ => Err(TokenDeriveError::ExpectedEnum(input.span())),
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TokenState {
	token: usize,
	state: u32,
}

struct Token {
	kind_ident: Ident,
	ident: Option<Ident>,
	const_: Option<String>,
	capture: bool,
}

impl Token {
	fn arg_types(&self) -> TokenStream {
		if self.capture {
			quote!((::ll1::locspan::Span, ::std::string::String))
		} else {
			quote!((::ll1::locspan::Span))
		}
	}

	fn arg_wildcards(&self) -> TokenStream {
		if self.capture {
			quote!((_, _))
		} else {
			quote!((_))
		}
	}

	fn args(&self) -> TokenStream {
		if self.capture {
			quote!((span, buffer))
		} else {
			quote!((span))
		}
	}
}

fn rust_range(range: AnyRange<char>) -> TokenStream {
	if range.len() == 1 {
		let c = range.first().unwrap();
		quote! { #c }
	} else {
		let first = range.first().unwrap();
		let last = range.last().unwrap();
		quote! { #first..=#last }
	}
}
