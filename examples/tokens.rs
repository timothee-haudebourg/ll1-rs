#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ll1::Token)]
pub enum TokenKind {
	#[token(skip, whitespace)]
	Whitespace,

	#[token(skip, regexp("#[^\n]*"))]
	Comment,

	#[token("foo")]
	Foo,

	#[token("bar")]
	Bar,
}

fn main() {
	let tokens: Vec<_> = ll1::Lexer::for_str("foo bar #comment")
		.map(|r| r.map(Token::into_kind))
		.collect::<Result<_, _>>()
		.unwrap();
	assert_eq!(tokens, [TokenKind::Foo, TokenKind::Bar]);
}
