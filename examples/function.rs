#[derive(Debug, ll1::Token)]
pub enum TokenKind {
	#[token(skip, whitespace)]
	Whitespace,

	#[token(skip, regexp("#([^\n#][^\n]*)?"))]
	Comment,

	#[token(regexp("##[^\n]*"))]
	DocComment,

	#[token("a")]
	A,
}

#[derive(Debug, ll1::Parse)]
struct Test {
	pub doc: DocComment,
	pub a: A,
}

fn main() {
	use ll1::Parse;
	let expr = Test::parse_str("#foo\na").unwrap();
	eprintln!("{expr:#?}")
}
