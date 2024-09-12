//! todo

use bstr::BStr;

use std::{
    fmt::{self, Display},
    ops::Range,
};

use super::{err::*, pos::*};

/* -------------------- *
 *    TOKEN METADATA    *
 * -------------------- */
/// Represents the different types of tokens created by the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // termimal token types
    Raw,
    Atom,
    Special,
    Whitespace,
    Newline,
    Control,
    Comment,

    // compound token types
    Escape,
    String,
    Identifier,
    IdentifierName,
    IdentifierArgument,
    DecoratorArgument,
    RequestURI,
    RequestVersion,
}

/// Represents the names for special tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenName {
    Backslash,
    SingleQuote,
    DoubleQuote,
    Equal,
    Colon,
    SemiColon,
    Dollar,
    AtSign,
    Hash,
    OpenBrace,
    CloseBrace,
    None,
}

impl From<u8> for TokenName {
    fn from(byte: u8) -> Self {
        match byte {
            b'\\' => Self::Backslash,
            b'\'' => Self::SingleQuote,
            b'"'  => Self::DoubleQuote,
            b'='  => Self::Equal,
            b':'  => Self::Colon,
            b';'  => Self::SemiColon,
            b'$'  => Self::Dollar,
            b'@'  => Self::AtSign,
            b'#'  => Self::Hash,
            b'{'  => Self::OpenBrace,
            b'}'  => Self::CloseBrace,
            _     => Self::None,
        }
    }
}


/* -------------------- *
 *    TERMINAL TOKEN    *
 * -------------------- */
/// Represents a terminal token created by the lexer.
#[derive(Debug, Clone)]
pub struct Terminal {
    /// The type of the terminal token.
    pub kind: TokenKind,
    /// The name of the terminal token.
    pub name: TokenName,
    /// The raw string value associated with the token.
    pub value: Box<BStr>,
    /// The position in the source code where this token was created.
    pub position: Position,
}

impl Terminal {
    /// Creates a new [`Terminal`] token.
    #[inline]
    #[must_use]
    pub fn new(kind: TokenKind, name: TokenName, value: Box<BStr>, position: Position) -> Self {
        Self { kind, name, value, position }
    }
}

impl GetPosition for Terminal {
    #[inline]
    fn byte_start(&self) -> usize {
        self.position.byte_start
    }

    #[inline]
    fn byte_end(&self) -> usize {
        self.position.byte_end
    }

    #[inline]
    fn line_start(&self) -> u32 {
        self.position.line_start
    }

    #[inline]
    fn line_end(&self) -> u32 {
        self.position.line_end
    }

    #[inline]
    fn col_start(&self) -> u32 {
        self.position.col_start
    }

    #[inline]
    fn col_end(&self) -> u32 {
        self.position.col_end
    }

    #[inline]
    fn position(&self) -> Position {
        self.position
    }
}

impl UpdatePosition for Terminal {
    #[inline]
    fn update_position(&mut self, delta: PositionDelta) {
        self.position.update_position(delta)
    }
}

impl Display for Terminal {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}


/* -------------------- *
 *    COMPOUND TOKEN    *
 * -------------------- */
/// Represents a compound token created by the lexer by collecting multiple tokens.
#[derive(Debug, Clone)]
pub struct Compound {
    /// The type of the compound token.
    pub kind: TokenKind,
    /// The body of the compound token, consisting of child tokens.
    pub body: Vec<Token>,
    /// An optional error associated with the compound token.
    pub error: Option<NodeError>,
}

impl Compound {
    /// Creates a new [`Compound`] token.
    #[inline]
    #[must_use]
    pub fn new(kind: TokenKind, body: Vec<Token>, error: Option<NodeError>) -> Self {
        Self { kind, body, error }
    }
}

impl GetPosition for Compound {
    #[inline]
    fn byte_start(&self) -> usize {
        self.body.byte_start()
    }

    #[inline]
    fn byte_end(&self) -> usize {
        self.body.byte_end()
    }

    #[inline]
    fn line_start(&self) -> u32 {
        self.body.line_start()
    }

    #[inline]
    fn line_end(&self) -> u32 {
        self.body.line_end()
    }

    #[inline]
    fn col_start(&self) -> u32 {
        self.body.col_start()
    }

    #[inline]
    fn col_end(&self) -> u32 {
        self.body.col_end()
    }
}

impl UpdatePosition for Compound {
    #[inline]
    fn update_position(&mut self, delta: PositionDelta) {
        self.body.update_position(delta);
    }
}

impl HasError for Compound {
    #[inline]
    fn has_error(&self) -> bool {
        self.error.is_some() || self.body.has_error()
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        self.error.as_ref().is_some_and(|x| !x.seen()) || self.body.has_unseen_error()
    }

    #[inline]
    fn has_eob_error(&self) -> bool {
        self.error.as_ref().is_some_and(|x| TOKEN_EOB_ERROR_CODES.contains(&x.code()))
    }
}

impl Display for Compound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for token in &self.body {
            token.fmt(f)?;
        }

        Ok(())
    }
}


/* -------------------- *
 *         TOKEN        *
 * -------------------- */
/// Represents a token created by the lexer, which can be either a terminal token or a compound token.
#[derive(Debug, Clone)]
pub enum Token {
    /// A terminal token created by the lexer.
    Terminal(Terminal),
    /// A compound token created by the lexer by collecting multiple tokens.
    Compound(Compound),
}

impl Token {
    /// Creates a new terminal [`Token`].
    #[inline]
    #[must_use]
    pub fn new_terminal(kind: TokenKind, name: TokenName, value: Box<BStr>, position: Position) -> Self {
        Self::Terminal(Terminal::new(kind, name, value, position))
    }

    /// Creates a new compound [`Token`].
    #[inline]
    #[must_use]
    pub fn new_compound(kind: TokenKind, body: Vec<Token>, error: Option<NodeError>) -> Self {
        Self::Compound(Compound::new(kind, body, error))
    }

    /// Gets the type of the token.
    #[inline]
    #[must_use]
    pub fn kind(&self) -> TokenKind {
        match self {
            Token::Terminal(x) => x.kind.clone(),
            Token::Compound(x) => x.kind.clone(),
        }
    }

    /// Returns true if the token is a delimiter.
    #[inline]
    #[must_use]
    pub fn is_delimiter(&self) -> bool {
        matches!(self.kind(), TokenKind::Comment | TokenKind::Whitespace | TokenKind::Newline)
    }

    /// Gets the name of the token.
    #[inline]
    #[must_use]
    pub fn name(&self) -> TokenName {
        match self {
            Token::Terminal(x) => x.name.clone(),
            Token::Compound(_) => TokenName::None,
        }
    }

    /// Gets the raw string value associated with the terminal token.
    ///
    /// # Panics
    ///
    /// Panics if called on a compound token.
    #[inline]
    #[must_use]
    pub fn value(&self) -> &BStr {
        match self {
            Token::Terminal(x) => &x.value,
            Token::Compound(_) => panic!("Unable to get string value from a compound token."),
        }
    }

    /// Gets the body of a compound token.
    ///
    /// # Panics
    ///
    /// Panics if called on a terminal token.
    #[inline]
    #[must_use]
    pub fn body(&self) -> &[Token] {
        match self {
            Token::Terminal(_) => panic!("Unable to get body from a terminal token."),
            Token::Compound(x) => &x.body,
        }
    }

    /// Gets a mutable reference to the body of a compound token.
    ///
    /// # Panics
    ///
    /// Panics if called on a terminal token.
    #[inline]
    #[must_use]
    pub fn body_mut(&mut self) -> &mut [Token] {
        match self {
            Token::Terminal(_) => panic!("Unable to get body from a terminal token."),
            Token::Compound(x) => &mut x.body,
        }
    }

    /// Gets an optional reference to the error associated with a compound token.
    #[inline]
    #[must_use]
    pub fn error(&self) -> Option<&NodeError> {
        match self {
            Token::Terminal(_) => None,
            Token::Compound(x) => x.error.as_ref(),
        }
    }

    /// Gets an optional mutable reference to the error associated with a compound token.
    #[inline]
    #[must_use]
    pub fn error_mut(&mut self) -> Option<&mut NodeError> {
        match self {
            Token::Terminal(_) => None,
            Token::Compound(x) => x.error.as_mut(),
        }
    }

    /// Sets the error associated with a compound token.
    ///
    /// # Panics
    ///
    /// Panics if called on a terminal token.
    #[inline]
    pub fn set_error(&mut self, error: NodeError) {
        match self {
            Token::Terminal(_) => panic!("Unable to set error on a terminal token."),
            Token::Compound(x) => x.error = Some(error),
        }
    }
}

impl GetPosition for Token {
    #[inline]
    fn byte_start(&self) -> usize {
        match self {
            Token::Terminal(x) => x.byte_start(),
            Token::Compound(x) => x.byte_start(),
        }
    }

    #[inline]
    fn byte_end(&self) -> usize {
        match self {
            Token::Terminal(x) => x.byte_end(),
            Token::Compound(x) => x.byte_end(),
        }
    }

    #[inline]
    fn line_start(&self) -> u32 {
        match self {
            Token::Terminal(x) => x.line_start(),
            Token::Compound(x) => x.line_start(),
        }
    }

    #[inline]
    fn line_end(&self) -> u32 {
        match self {
            Token::Terminal(x) => x.line_end(),
            Token::Compound(x) => x.line_end(),
        }
    }

    #[inline]
    fn col_start(&self) -> u32 {
        match self {
            Token::Terminal(x) => x.col_start(),
            Token::Compound(x) => x.col_start(),
        }
    }

    #[inline]
    fn col_end(&self) -> u32 {
        match self {
            Token::Terminal(x) => x.col_end(),
            Token::Compound(x) => x.col_end(),
        }
    }
}

impl UpdatePosition for Token {
    #[inline]
    fn update_position(&mut self, delta: PositionDelta) {
        match self {
            Token::Terminal(x) => x.update_position(delta),
            Token::Compound(x) => x.update_position(delta),
        }
    }
}

impl HasError for Token {
    #[inline]
    fn has_error(&self) -> bool {
        match self {
            Token::Terminal(_) => false,
            Token::Compound(x) => x.has_error(),
        }
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        match self {
            Token::Terminal(_) => false,
            Token::Compound(x) => x.has_unseen_error(),
        }
    }

    #[inline]
    fn has_eob_error(&self) -> bool {
        match self {
            Token::Terminal(_) => false,
            Token::Compound(x) => x.has_eob_error(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Terminal(x) => x.fmt(f),
            Token::Compound(x) => x.fmt(f),
        }
    }
}


/* -------------------- *
 *        TRAITS        *
 * -------------------- */
/// Provides methods to get the inner bounds of the implementing object.
pub trait InnerBounds {
    /// Returns the starting inner bound within the implementing object.
    #[must_use]
    fn inner_bound_start(&self) -> usize;

    /// Returns the starting inner bound after idx within the implementing object.
    #[must_use]
    fn inner_bound_start_after(&self, idx: usize) -> usize;

    /// Returns the ending inner bound within the implementing object.
    #[must_use]
    fn inner_bound_end(&self) -> usize;

    /// Returns the inner bounds of the implementing object.
    #[inline]
    #[must_use]
    fn inner_bounds(&self) -> Range<usize> {
        Range {
            start: self.inner_bound_start(),
            end: self.inner_bound_end(),
        }
    }

    /// Returns the inner bounds after idx from the implementing object.
    #[inline]
    #[must_use]
    fn inner_bounds_after(&self, idx: usize) -> Range<usize> {
        Range {
            start: self.inner_bound_start_after(idx),
            end: self.inner_bound_end(),
        }
    }
}

impl InnerBounds for [Token] {
    #[inline]
    fn inner_bound_start(&self) -> usize {
        self.iter().take_while(|x| x.is_delimiter()).count()
    }

    #[inline]
    fn inner_bound_start_after(&self, n: usize) -> usize {
        n + self[n..].inner_bound_start()
    }

    #[inline]
    fn inner_bound_end(&self) -> usize {
        self.len() - self.iter().rev().take_while(|x| x.is_delimiter()).count()
    }
}
