#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ll1::Token)]
pub enum TokenKind {
	#[token(skip, whitespace)]
	Whitespace,

	#[token(ident)]
	Ident,

	#[token("+")]
	Plus,

	#[token("-")]
	Minus,

	#[token("*")]
	Mul,

	#[token("/")]
	Div,
}

#[derive(Debug, ll1::Parse)]
pub enum Expr {
	Add(Term, Plus, Box<Expr>),
	Sub(Term, Minus, Box<Expr>),
	Term(Term),
}

#[derive(Debug, ll1::Parse)]
pub enum Term {
	Mul(Factor, Mul, Box<Term>),
	Div(Factor, Div, Box<Term>),
	Factor(Factor),
}

#[derive(Debug, ll1::Parse)]
pub enum Factor {
	Ident(Ident),
}

#[derive(Debug, ll1::Parse)]
pub enum List<T> {
	Empty,
	NonEmpty(T, Mul, Box<Self>),
}

fn main() {
	use ll1::Parse;
	let expr = Expr::parse_str("a * b + c").unwrap();
	eprintln!("{expr:#?}")
}
