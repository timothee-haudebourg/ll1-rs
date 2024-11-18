use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use syn::parse_macro_input;
mod grammar;
mod token;
mod r#type;

#[proc_macro_derive(Parse)]
#[proc_macro_error]
pub fn derive_non_terminal(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as syn::DeriveInput);

	match grammar::derive(input) {
		Ok(output) => {
			// eprintln!("tokens: {output}");
			output.into()
		}
		Err(e) => {
			let span = e.span();
			abort!(span, e)
		}
	}
}

#[proc_macro_derive(Token, attributes(token))]
#[proc_macro_error]
pub fn token(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as syn::DeriveInput);

	match token::derive(input) {
		Ok(output) => output.into(),
		Err(e) => {
			let span = e.span();
			abort!(span, e)
		}
	}
}
