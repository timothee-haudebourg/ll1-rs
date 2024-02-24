use core::fmt;
use std::{convert::Infallible, iter::Peekable};

pub use ll1_macros::{Parse, Token};

pub use decoded_char;
pub use locspan;

pub use decoded_char::DecodedChar;
pub use locspan::Span;

#[derive(Debug)]
pub enum LexError<E> {
    Stream(usize, E),
    Unexpected(usize, Option<char>),
}

impl<E: fmt::Display> fmt::Display for LexError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Stream(_, e) => e.fmt(f),
            Self::Unexpected(_, Some(c)) => write!(f, "unexpected character `{c}`"),
            Self::Unexpected(_, None) => write!(f, "unexpected end of stream"),
        }
    }
}

#[derive(Debug)]
pub enum ParseError<T, E = std::convert::Infallible> {
    Lexing(LexError<E>),
    UnexpectedToken(usize, Option<T>),
}

impl<T: fmt::Display, E: fmt::Display> fmt::Display for ParseError<T, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lexing(e) => e.fmt(f),
            Self::UnexpectedToken(_, Some(t)) => write!(f, "unexpected token `{t}`"),
            Self::UnexpectedToken(_, None) => write!(f, "unexpected end of stream"),
        }
    }
}

pub trait Spanned {
    fn start(&self) -> Option<usize>;

    fn end(&self) -> Option<usize>;

    fn span(&self) -> Span {
        Span::new(
            self.start().unwrap_or_default(),
            self.end().unwrap_or_default(),
        )
    }
}

impl<T: Spanned> Spanned for Box<T> {
    fn start(&self) -> Option<usize> {
        T::start(self)
    }

    fn end(&self) -> Option<usize> {
        T::end(self)
    }

    fn span(&self) -> Span {
        T::span(self)
    }
}

pub trait Token: Sized + Spanned {
    fn parse_from<E>(
        offset: usize,
        chars: &mut Peekable<impl Iterator<Item = Result<DecodedChar, E>>>,
    ) -> Result<Option<Self>, LexError<E>>;
}

pub struct InfallibleIter<I>(pub I);

impl<I: Iterator> Iterator for InfallibleIter<I> {
    type Item = Result<I::Item, Infallible>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Ok)
    }
}

pub struct Lexer<T, I: Iterator> {
    offset: usize,
    chars: Peekable<I>,
    next_token: Option<(usize, T)>,
}

impl<T, I: Iterator> Lexer<T, I> {
    pub fn new(chars: I) -> Self {
        Self {
            offset: 0,
            chars: chars.peekable(),
            next_token: None,
        }
    }
}

impl<T, I, E> Lexer<T, I>
where
    I: Iterator<Item = Result<DecodedChar, E>>,
    T: Token,
{
    fn pull_token(&mut self) -> Result<(usize, Option<T>), LexError<E>> {
        match T::parse_from(self.offset, &mut self.chars) {
            Ok(Some(token)) => {
                self.offset = token.span().end();
                Ok((token.span().start(), Some(token)))
            }
            Ok(None) => Ok((self.offset, None)),
            Err(e) => Err(e),
        }
    }

    pub fn peek_with_offset(&mut self) -> Result<(usize, Option<&T>), LexError<E>> {
        if self.next_token.is_none() {
            let (offset, token) = self.pull_token()?;
            self.next_token = token.map(|t| (offset, t))
        }

        match &self.next_token {
            Some((offset, token)) => Ok((*offset, Some(token))),
            None => Ok((self.offset, None)),
        }
    }

    pub fn peek(&mut self) -> Result<Option<&T>, LexError<E>> {
        self.peek_with_offset().map(|(_, t)| t)
    }

    pub fn next_token(&mut self) -> Result<(usize, Option<T>), LexError<E>> {
        match self.next_token.take() {
            Some((offset, token)) => Ok((offset, Some(token))),
            None => self.pull_token(),
        }
    }
}

impl<'a, T> Lexer<T, InfallibleIter<decoded_char::Utf8Decoded<std::str::Chars<'a>>>> {
    pub fn for_str(input: &'a str) -> Self {
        Self::new(InfallibleIter(decoded_char::Utf8Decoded(input.chars())))
    }
}

impl<I, T, E> Iterator for Lexer<T, I>
where
    I: Iterator<Item = Result<DecodedChar, E>>,
    T: Spanned + Token,
{
    type Item = Result<T, LexError<E>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok((_, token)) => token.map(Ok),
            Err(e) => Some(Err(e)),
        }
    }
}

pub trait Parse: Sized + LocalContext {
    fn parse_str(string: &str) -> Result<Self, ParseError<Self::Token>> {
        Self::parse_utf8(string.chars())
    }

    fn parse_utf8(chars: impl Iterator<Item = char>) -> Result<Self, ParseError<Self::Token>> {
        Self::parse_chars_infallible(chars.map(DecodedChar::from_utf8))
    }

    fn parse_utf16(chars: impl Iterator<Item = char>) -> Result<Self, ParseError<Self::Token>> {
        Self::parse_chars_infallible(chars.map(DecodedChar::from_utf16))
    }

    fn parse_chars_infallible(
        chars: impl Iterator<Item = DecodedChar>,
    ) -> Result<Self, ParseError<Self::Token>> {
        Self::parse_chars(chars.map(Ok))
    }

    fn parse_chars<E>(
        chars: impl Iterator<Item = Result<DecodedChar, E>>,
    ) -> Result<Self, ParseError<Self::Token, E>> {
        Self::parse(Lexer::new(chars))
    }

    fn parse<I: Iterator<Item = Result<DecodedChar, E>>, E>(
        mut tokens: Lexer<Self::Token, I>,
    ) -> Result<Self, ParseError<Self::Token, E>> {
        Self::parse_in(&Context::Empty, &mut tokens)
    }

    fn parse_in<I: Iterator<Item = Result<DecodedChar, E>>, E>(
        context: &Context<Self::Token>,
        tokens: &mut Lexer<Self::Token, I>,
    ) -> Result<Self, ParseError<Self::Token, E>>;
}

impl<U: Parse> Parse for Box<U> {
    fn parse_in<I: Iterator<Item = Result<DecodedChar, E>>, E>(
        context: &Context<Self::Token>,
        tokens: &mut Lexer<Self::Token, I>,
    ) -> Result<Self, ParseError<Self::Token, E>> {
        U::parse_in(context, tokens).map(Box::new)
    }
}

pub trait LocalContext {
    type Token;

    fn accepts(context: &Context<Self::Token>, token: Option<&Self::Token>) -> bool;
}

impl<U: LocalContext> LocalContext for Box<U> {
    type Token = U::Token;

    fn accepts(context: &Context<Self::Token>, token: Option<&Self::Token>) -> bool {
        U::accepts(context, token)
    }
}

pub type ContextAcceptsFn<T> = fn(&Context<T>, Option<&T>) -> bool;

pub enum Context<'a, T> {
    Empty,
    NonEmpty {
        parent: &'a Context<'a, T>,
        local: Box<ContextAcceptsFn<T>>,
    },
}

impl<'a, T> Context<'a, T> {
    pub fn with<L: LocalContext<Token = T>>(&self) -> Context<T> {
        Context::NonEmpty {
            parent: self,
            local: Box::new(L::accepts),
        }
    }

    pub fn accepts(&self, token: Option<&T>) -> bool {
        match self {
            Self::Empty => token.is_none(),
            Self::NonEmpty { parent, local } => local(parent, token),
        }
    }
}
