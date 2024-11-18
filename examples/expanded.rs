#[derive(Debug)]
pub enum TokenKind {
	Whitespace,
	Comment,
	DocComment,
	A,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DocComment(::ll1::locspan::Span, ::std::string::String);

impl DocComment {
	pub fn as_str(&self) -> &str {
		self.1.as_str()
	}
	pub fn into_string(self) -> std::string::String {
		self.1
	}
}
impl AsRef<str> for DocComment {
	fn as_ref(&self) -> &str {
		self.as_str()
	}
}
impl ::core::borrow::Borrow<str> for DocComment {
	fn borrow(&self) -> &str {
		self.as_str()
	}
}
impl ::core::fmt::Display for DocComment {
	fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
		self.as_str().fmt(f)
	}
}
impl ::ll1::LocalContext for DocComment {
	type Token = Token;
	fn accepts(context: &::ll1::Context<Token>, token: Option<&Token>) -> bool {
		match token {
			Some(Token::DocComment(_, _)) => true,
			_ => false,
		}
	}
}
impl ::ll1::Parse for DocComment {
	fn parse_in<_I: Iterator<Item = Result<::ll1::DecodedChar, _E>>, _E>(
		context: &::ll1::Context<Token>,
		tokens: &mut ::ll1::Lexer<Token, _I>,
	) -> Result<Self, ::ll1::ParseError<Token, _E>> {
		let context = context.with::<Self>();
		match tokens
			.next_token_in(&context)
			.map_err(::ll1::ParseError::Lexing)?
		{
			(_, Some(Token::DocComment(span, buffer))) => Ok(Self(span, buffer)),
			(offset, Some(token)) => Err(::ll1::ParseError::UnexpectedToken(offset, Some(token))),
			(offset, None) => Err(::ll1::ParseError::UnexpectedToken(offset, None)),
		}
	}
}
impl ::ll1::Spanned for DocComment {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct A(::ll1::locspan::Span);

impl A {
	pub fn as_str(&self) -> &str {
		"a"
	}
	pub fn into_string(self) -> std::string::String {
		"a".to_owned()
	}
}
impl AsRef<str> for A {
	fn as_ref(&self) -> &str {
		self.as_str()
	}
}
impl ::core::borrow::Borrow<str> for A {
	fn borrow(&self) -> &str {
		self.as_str()
	}
}
impl ::core::fmt::Display for A {
	fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
		self.as_str().fmt(f)
	}
}
impl ::ll1::LocalContext for A {
	type Token = Token;
	fn accepts(context: &::ll1::Context<Token>, token: Option<&Token>) -> bool {
		match token {
			Some(Token::A(_)) => true,
			_ => false,
		}
	}
}
impl ::ll1::Parse for A {
	fn parse_in<_I: Iterator<Item = Result<::ll1::DecodedChar, _E>>, _E>(
		context: &::ll1::Context<Token>,
		tokens: &mut ::ll1::Lexer<Token, _I>,
	) -> Result<Self, ::ll1::ParseError<Token, _E>> {
		let context = context.with::<Self>();
		match tokens
			.next_token_in(&context)
			.map_err(::ll1::ParseError::Lexing)?
		{
			(_, Some(Token::A(span))) => Ok(Self(span)),
			(offset, Some(token)) => Err(::ll1::ParseError::UnexpectedToken(offset, Some(token))),
			(offset, None) => Err(::ll1::ParseError::UnexpectedToken(offset, None)),
		}
	}
}
impl ::ll1::Spanned for A {
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

#[derive(Debug)]
pub enum Token {
	DocComment(::ll1::locspan::Span, ::std::string::String),
	A(::ll1::locspan::Span),
}

impl Token {
	pub fn kind(&self) -> TokenKind {
		match self {
			Self::DocComment(_, _) => TokenKind::DocComment,
			Self::A(_) => TokenKind::A,
		}
	}
	pub fn into_kind(self) -> TokenKind {
		self.kind()
	}
	pub fn as_str(&self) -> &str {
		match self {
			Self::DocComment(_, buffer) => buffer.as_str(),
			Self::A(_) => "a",
		}
	}
	pub fn into_string(self) -> ::std::string::String {
		match self {
			Self::DocComment(_, buffer) => buffer,
			Self::A(_) => "a".to_owned(),
		}
	}
}
impl ::ll1::Spanned for Token {
	fn start(&self) -> Option<usize> {
		match self {
			Self::DocComment(span, _) => Some(span.start()),
			Self::A(span) => Some(span.start()),
		}
	}
	fn end(&self) -> Option<usize> {
		match self {
			Self::DocComment(span, _) => Some(span.end()),
			Self::A(span) => Some(span.end()),
		}
	}
	fn span(&self) -> ::ll1::Span {
		match self {
			Self::DocComment(span, _) => *span,
			Self::A(span) => *span,
		}
	}
}
impl ::ll1::Token for Token {
	fn parse_from<E>(
		mut offset: usize,
		chars: &mut ::std::iter::Peekable<
			impl Iterator<Item = Result<::ll1::decoded_char::DecodedChar, E>>,
		>,
	) -> Result<Option<Self>, ::ll1::LexError<E>> {
		let mut state = 0u32;
		let mut end = offset;
		let mut buffer = ::std::string::String::new();
		loop {
			let next_char = match chars.peek() {
				Some(Ok(c)) => Some(c.chr()),
				Some(Err(_)) => {
					break Err(::ll1::LexError::Stream(
						end,
						chars.next().unwrap().unwrap_err(),
					));
				}
				None => None,
			};
			state = match state {
				0u32 => match next_char {
					Some('\t'..='\n') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						1u32
					}
					Some('\r') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						1u32
					}
					Some(' ') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						1u32
					}
					Some('#') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						2u32
					}
					Some('a') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						3u32
					}
					_ => break Ok(None),
				},
				1u32 => match next_char {
					Some('\t'..='\n') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						6u32
					}
					Some('\r') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						6u32
					}
					Some(' ') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						6u32
					}
					_ => {
						offset = end;
						buffer.clear();
						0u32
					}
				},
				2u32 => match next_char {
					Some('#') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						4u32
					}
					_ => {
						offset = end;
						buffer.clear();
						0u32
					}
				},
				3u32 => break Ok(Some(Self::A(::ll1::locspan::Span::new(offset, end)))),
				4u32 => match next_char {
					Some('\0'..='\t') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						5u32
					}
					Some('\u{b}'..='\u{10ffff}') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						5u32
					}
					_ => {
						break Ok(Some(Self::DocComment(
							::ll1::locspan::Span::new(offset, end),
							::core::mem::take(&mut buffer),
						)));
					}
				},
				5u32 => match next_char {
					Some('\0'..='\t') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						5u32
					}
					Some('\u{b}'..='\u{10ffff}') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						5u32
					}
					_ => {
						break Ok(Some(Self::DocComment(
							::ll1::locspan::Span::new(offset, end),
							::core::mem::take(&mut buffer),
						)));
					}
				},
				6u32 => match next_char {
					Some('\t'..='\n') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						6u32
					}
					Some('\r') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						6u32
					}
					Some(' ') => {
						buffer.push(next_char.unwrap());
						end += chars.next().unwrap().ok().unwrap().len();
						6u32
					}
					_ => {
						offset = end;
						buffer.clear();
						0u32
					}
				},
				_ => panic!("internal error: entered unreachable code"),
			};
		}
	}
}
impl AsRef<str> for Token {
	fn as_ref(&self) -> &str {
		self.as_str()
	}
}
impl ::core::borrow::Borrow<str> for Token {
	fn borrow(&self) -> &str {
		self.as_str()
	}
}
impl ::core::fmt::Display for Token {
	fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
		self.as_str().fmt(f)
	}
}

#[derive(Debug)]
struct Test {
	pub doc: DocComment,
	pub a: A,
}

#[allow(non_camel_case_types)]
struct _Test_0_0;
impl ::ll1::LocalContext for _Test_0_0 {
	type Token = Token;
	fn accepts(context: &::ll1::Context<Token>, token: Option<&Token>) -> bool {
		let sub_context = context.with::<_Test_0_1>();
		if <DocComment as ::ll1::LocalContext>::accepts(&sub_context, token) {
			return true;
		}
		false
	}
}
#[allow(non_camel_case_types)]
struct _Test_0_1;
impl ::ll1::LocalContext for _Test_0_1 {
	type Token = Token;
	fn accepts(context: &::ll1::Context<Token>, token: Option<&Token>) -> bool {
		let sub_context = context.with::<_Test_0_2>();
		if <A as ::ll1::LocalContext>::accepts(&sub_context, token) {
			return true;
		}
		false
	}
}
#[allow(non_camel_case_types)]
struct _Test_0_2;
impl ::ll1::LocalContext for _Test_0_2 {
	type Token = Token;
	fn accepts(context: &::ll1::Context<Token>, token: Option<&Token>) -> bool {
		context.accepts(token)
	}
}
impl ::ll1::Parse for Test {
	fn parse_in<_I: Iterator<Item = Result<::ll1::DecodedChar, _E>>, _E>(
		context: &::ll1::Context<Token>,
		tokens: &mut ::ll1::Lexer<Token, _I>,
	) -> Result<Self, ::ll1::ParseError<Token, _E>> {
		let next_context = context.with::<_Test_0_0>();
		let next_token = tokens
			.peek_in(&next_context)
			.map_err(::ll1::ParseError::Lexing)?;
		let rule_context = context.with::<_Test_0_1>();
		if <DocComment as ::ll1::LocalContext>::accepts(&rule_context, next_token) {
			let arg_0 = <DocComment as ::ll1::Parse>::parse_in(&rule_context, tokens)?;
			let next_context = context.with::<_Test_0_1>();
			let next_token = tokens
				.peek_in(&next_context)
				.map_err(::ll1::ParseError::Lexing)?;
			let rule_context = context.with::<_Test_0_2>();
			if <A as ::ll1::LocalContext>::accepts(&rule_context, next_token) {
				let arg_1 = <A as ::ll1::Parse>::parse_in(&rule_context, tokens)?;
				let next_context = context.with::<_Test_0_2>();
				let next_token = tokens
					.peek_in(&next_context)
					.map_err(::ll1::ParseError::Lexing)?;
				return Ok(Self {
					doc: arg_0,
					a: arg_1,
				});
			}
			let (offset, token) = tokens.next_token().map_err(::ll1::ParseError::Lexing)?;
			return Err(::ll1::ParseError::UnexpectedToken(offset, token));
		}
		let (offset, token) = tokens.next_token().map_err(::ll1::ParseError::Lexing)?;
		return Err(::ll1::ParseError::UnexpectedToken(offset, token));
	}
}
impl ::ll1::LocalContext for Test {
	type Token = Token;
	fn accepts(context: &::ll1::Context<Token>, token: Option<&Token>) -> bool {
		<_Test_0_0 as ::ll1::LocalContext>::accepts(context, token)
	}
}
impl ::ll1::Spanned for Test {
	fn start(&self) -> Option<usize> {
		let Self { doc, a } = self;
		if let Some(start) = <DocComment as ::ll1::Spanned>::start(doc) {
			return Some(start);
		}
		if let Some(start) = <A as ::ll1::Spanned>::start(a) {
			return Some(start);
		}
		None
	}
	fn end(&self) -> Option<usize> {
		let Self { doc, a } = self;
		if let Some(start) = <A as ::ll1::Spanned>::start(a) {
			return Some(start);
		}
		if let Some(start) = <DocComment as ::ll1::Spanned>::start(doc) {
			return Some(start);
		}
		None
	}
}
fn main() {
	use ll1::Parse;
	let expr = Test::parse_str("#foo\na").unwrap();
	eprintln!("{0:#?}\n", expr)
}
