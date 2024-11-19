# LL(1) parsers made simple

<!-- cargo-rdme start -->

This crate provides the `Parse` macro automatically deriving a parser for
datatypes representing the abstract syntax tree of an [LL(1)][ll1] grammar.

[ll1]: <https://en.wikipedia.org/wiki/LL_grammar>

## Usage

First define all the tokens in your grammar by creating a `TokenKind` type
implementing deriving [`Token`], then use the tokens to define the
grammar using data-types deriving [`Parse`].

```rust
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

use ll1::Parse;
let expr = Expr::parse_str("a * b + c").unwrap();
eprintln!("{expr:#?}")
```

The drawback to this method is that it is impossible for `ll1` to warn you
if the grammar is not actually an LL(1) grammar. If the grammar is not LL(1)
the generated parser may fail, or hang.

<!-- cargo-rdme end -->
