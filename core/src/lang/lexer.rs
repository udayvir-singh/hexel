//! Provides a lexer for tokenizing source code.
//!
//! # Examples
//!
//! ```
//! let mut lexer = Lexer::new(b"hello ${world}", "/path/example.hxl".to_string(), None, None);
//! let mut tokens = Vec::new();
//!
//! while lexer.active() {
//!     tokens.push(lexer.next_token(true, true));
//! }
//!
//! assert_eq!(tokens.len(), 3);
//!
//! assert_eq!(tokens[0].value(), "hello");
//! assert_eq!(tokens[0].kind(), TokenKind::Atom);
//!
//! assert_eq!(tokens[1].value(), " ");
//! assert_eq!(tokens[1].kind(), TokenKind::Whitespace);
//!
//! assert_eq!(tokens[2].to_string(), "${world}");
//! assert_eq!(tokens[2].kind(), TokenKind::Identifier);
//! ```

use crate::utils::{error::*, position::*, token::*};

/* -------------------- *
 *         LEXER        *
 * -------------------- */
/// Represents a lexer used for tokenizing source code.
pub struct Lexer<'a> {
    source: &'a [u8],
    filepath: &'a str,
    byte: usize,
    line: u32,
    col: u32,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    /* -------------------- *
     *        PUBLIC        *
     * -------------------- */
    /// Creates a new instance of [`Lexer`].
    #[inline]
    #[must_use]
    pub fn new(
        source: &'a [u8],
        filepath: &'a str,
        byte_offset: Option<usize>,
        line_offset: Option<u32>,
    ) -> Self {
        Lexer {
            source,
            filepath,
            byte: byte_offset.unwrap_or(0),
            line: line_offset.unwrap_or(1),
            col: 0,
            cursor: 0,
        }
    }

    /// Checks if there are more bytes to process in the source code.
    #[inline]
    #[must_use]
    pub fn active(&self) -> bool {
        self.cursor < self.source.len()
    }

    /// Check if the next byte is a newline.
    ///
    /// # Panics
    ///
    /// This function will panic if called when there are no more bytes to read.
    ///
    /// It is important to check with [`active`](Self::active) function before calling to prevent panics.
    #[inline]
    #[must_use]
    pub fn is_newline_next(&self) -> bool {
        self.source[self.cursor] == b'\n'
            || (self.cursor + 1 < self.source.len()
                && self.source[self.cursor] == b'\r'
                && self.source[self.cursor + 1] == b'\n')
    }

    /// Check if the next byte is a delimiter.
    ///
    /// # Panics
    ///
    /// This function will panic if called when there are no more bytes to read.
    ///
    /// It is important to check with [`active`](Self::active) function before calling to prevent panics.
    #[inline]
    #[must_use]
    pub fn is_delimiter_next(&self) -> bool {
        matches!(self.peek_byte(), b';' | b' ' | b'\t') || self.is_newline_next()
    }

    /// Peeks at the next byte in the source code.
    ///
    /// # Panics
    ///
    /// This function will panic if called when there are no more bytes to read.
    ///
    /// It is important to check with [`active`](Self::active) function before calling to prevent panics.
    #[inline]
    #[must_use]
    pub fn peek_byte(&self) -> u8 {
        self.source[self.cursor]
    }

    /// Peeks at the next whitespace delimited word in the source code.
    #[must_use]
    pub fn peek_word(&self) -> &[u8] {
        // skip leading whitespace
        let mut cursor_start = self.cursor;

        while cursor_start < self.source.len()
            && matches!(self.source[cursor_start], b' ' | b'\t')
        {
            cursor_start += 1;
        }

        // collect till whitespace
        let mut cursor_end = cursor_start;

        while cursor_end < self.source.len()
            && !matches!(self.source[cursor_end], b' ' | b'\t' | b'\r' | b'\n')
        {
            cursor_end += 1;
        }

        &self.source[cursor_start..cursor_end]
    }

    /// Returns the next terminal token in the source code.
    ///
    /// # Arguments
    ///
    /// * `lex_comment` - Indicates whether to process comments.
    ///
    /// # Panics
    ///
    /// This function will panic if called when there are no more bytes to read.
    ///
    /// It is important to check with [`active`](Self::active) function before calling to prevent panics.
    #[must_use]
    pub fn next_terminal(&mut self, lex_comment: bool) -> Token {
        let cursor_start = self.cursor;

        match self.next_byte() {
            b'\n' => {
                self.lex_newline(cursor_start)
            }
            b'\r' if self.active() && self.peek_byte() == b'\n' => {
                self.lex_carriage_newline(cursor_start)
            }
            b' ' | b'\t' => {
                self.lex_whitespace(cursor_start)
            }
            b'\0'..=b'\x1F' | b'\x7F' => {
                self.lex_control(cursor_start)
            }
            b';' if lex_comment => {
                self.lex_comment(cursor_start)
            }
            b'\\' | b'\'' | b'"' | b'=' | b':' | b';' | b'$' | b'@' | b'#' | b'{' | b'}' => {
                self.lex_special(cursor_start)
            }
            _ => {
                self.lex_atom(cursor_start)
            }
        }
    }

    /// Returns the next token in the source code.
    ///
    /// # Arguments
    ///
    /// * `lex_comment` - Indicates whether to process comments.
    /// * `lex_string` - Indicates whether to process strings.
    ///
    /// # Panics
    ///
    /// This function will panic if called when there are no more bytes to read.
    ///
    /// It is important to check with [`active()`](Self::active) before calling to prevent panics.
    #[must_use]
    pub fn next_token(&mut self, lex_comment: bool, lex_string: bool) -> Token {
        let token = self.next_terminal(lex_comment);

        match token.name() {
            TokenName::Backslash => {
                self.lex_escape(token)
            }
            TokenName::Dollar if self.peek_byte() == b'{' => {
                self.lex_identifier(token)
            }
            TokenName::SingleQuote | TokenName::DoubleQuote if lex_string => {
                self.lex_string(token)
            }
            _ => token
        }
    }

    /// Returns a raw token from the next bytes in the source code that satisfy the given condition.
    ///
    /// # Panics
    ///
    /// This function will panic if called when there are no more bytes to read
    /// or if the given condition doesn't satisfy at least 1 byte.
    ///
    /// It is important to check with [`active()`](Self::active) before calling to prevent panics.
    #[must_use]
    pub fn next_raw(&mut self, cond: fn(&Self) -> bool) -> Token {
        assert!(self.active() && cond(self));

        let cursor_start = self.cursor;
        let byte_start = self.byte;
        let line_start = self.line;
        let col_start = self.col;

        self.inc_cursor_while(cond);

        Token::new_terminal(
            TokenKind::Raw,
            TokenName::None,
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                byte_start, self.byte,
                line_start, self.line,
                col_start,  self.col,
            ),
        )
    }


    /* -------------------- *
     *       TERMINAL       *
     * -------------------- */
    fn lex_newline(&self, cursor_start: usize) -> Token {
        Token::new_terminal(
            TokenKind::Newline,
            TokenName::None,
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                self.byte, self.byte,
                self.line, self.line,
                0, 0,
            ),
        )
    }

    fn lex_carriage_newline(&mut self, cursor_start: usize) -> Token {
        let byte_start = self.byte;

        self.inc_cursor();

        Token::new_terminal(
            TokenKind::Newline,
            TokenName::None,
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                byte_start, self.byte,
                self.line, self.line,
                0, 0,
            ),
        )
    }

    fn lex_whitespace(&mut self, cursor_start: usize) -> Token {
        let byte_start = self.byte;
        let col_start = self.col;

        self.inc_cursor_while(|l| matches!(l.peek_byte(), b' ' | b'\t'));

        Token::new_terminal(
            TokenKind::Whitespace,
            TokenName::None,
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                byte_start, self.byte,
                self.line,  self.line,
                col_start,  self.col,
            ),
        )
    }

    fn lex_control(&mut self, cursor_start: usize) -> Token {
        let byte_start = self.byte;
        let col_start = self.col;

        self.inc_cursor_while(|l| {
            !l.is_newline_next()
                && matches!(l.peek_byte(), b'\0'..=b'\x08' | b'\x0B'..=b'\x1F' | b'\x7F')
        });

        Token::new_terminal(
            TokenKind::Control,
            TokenName::None,
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                byte_start, self.byte,
                self.line,  self.line,
                col_start,  self.col,
            ),
        )
    }

    fn lex_comment(&mut self, cursor_start: usize) -> Token {
        let byte_start = self.byte;
        let col_start = self.col;

        self.inc_cursor_while(|l| !l.is_newline_next());

        Token::new_terminal(
            TokenKind::Comment,
            TokenName::None,
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                byte_start, self.byte,
                self.line,  self.line,
                col_start,  self.col,
            ),
        )
    }

    fn lex_special(&self, cursor_start: usize) -> Token {
        Token::new_terminal(
            TokenKind::Special,
            TokenName::from(self.source[cursor_start]),
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                self.byte, self.byte,
                self.line, self.line,
                self.col,  self.col,
            ),
        )
    }

    fn lex_atom(&mut self, cursor_start: usize) -> Token {
        let byte_start = self.byte;
        let col_start = self.col;

        self.inc_cursor_while(|l| {
            !matches!(
                l.peek_byte(),
                b'\0'..=b'\x1F' | b'\x7F' | b' ' | b'\\' | b'\'' | b'"' |
                b'=' | b':' | b';' | b'$' | b'@' | b'#' | b'{' | b'}'
            )
        });

        Token::new_terminal(
            TokenKind::Atom,
            TokenName::None,
            self.slice_source(cursor_start, self.cursor),
            Position::new(
                byte_start, self.byte,
                self.line,  self.line,
                col_start,  self.col,
            ),
        )
    }


    /* -------------------- *
     *       COMPOUND       *
     * -------------------- */
    fn lex_braced_escape(&mut self, mut body: Vec<Token>, decimal: bool) -> Token {
        let mut open = true;
        let mut valid = true;
        let mut code_idx = 0;
        let mut body_len = 0;

        // collect codepoint with braces
        while self.active() && !self.is_newline_next() {
            let token = self.next_terminal(false);

            if token.name() == TokenName::CloseBrace {
                body.push(token);
                open = false;
                break;
            }

            if !token.is_delimiter() {
                if token.kind() != TokenKind::Atom {
                    valid = false;
                }

                if code_idx == 0 {
                    code_idx = body.len();
                }

                body_len += 1;
            }


            body.push(token);
        }

        // validate escape sequence
        let error = {
            let err = |code, bounds| {
                Some(NodeError::new(code, bounds, None, self.filepath.to_string()))
            };

            if open {
                err(ErrorCode::LC04, None)
            } else if body_len == 0 {
                err(ErrorCode::LC05, None)
            } else if body_len > 1 {
                err(ErrorCode::LC06, Some(code_idx + 1..body.len()))
            } else if !valid {
                err(ErrorCode::LC07, None)
            } else {
                let codepoint = body[code_idx].value();

                match decimal {
                    true if !Self::is_valid_ascii_codepoint(codepoint) => {
                        err(ErrorCode::LC08, Some(code_idx..code_idx + 1))
                    }
                    false if !Self::is_valid_unicode_codepoint(codepoint) => {
                        err(ErrorCode::LC09, Some(code_idx..code_idx + 1))
                    }
                    _ => None
                }
            }
        };

        Token::new_compound(TokenKind::Escape, body, error)
    }

    fn lex_escape(&mut self, token: Token) -> Token {
        // early return with an error if EOF or EOL is reached
        if !self.active() || self.peek_byte() == b'\n' {
            return Token::new_compound(
                TokenKind::Escape,
                vec![token],
                Some(NodeError::new(
                    ErrorCode::LC01,
                    None,
                    None,
                    self.filepath.to_string(),
                )),
            );
        }

        // collect escape sequence head
        let head_start = self.cursor;
        let head_byte = self.next_byte();
        let mut body = vec![token, self.lex_special(head_start)];

        match head_byte {
            // tokenize single character escape sequence
            b'a' | b'b' | b't' | b'n' | b'v' | b'f' | b'r' | b'e' |
            b';' | b'$' | b'@' | b'=' | b'\'' | b'"' | b'\\' |
            b'C' | b'D' | b'G' | b'H' | b'O' | b'P' | b'T' => {
                Token::new_compound(TokenKind::Escape, body, None)
            }
            // tokenize ASCII escape sequence
            b'{' => {
                self.lex_braced_escape(body, true)
            }
            // tokenize Unicode escape sequence
            b'u' => {
                if self.peek_byte() == b'{' {
                    body.push(self.next_terminal(false));

                    self.lex_braced_escape(body, false)
                } else {
                    Token::new_compound(
                        TokenKind::Escape,
                        body,
                        Some(NodeError::new(
                            ErrorCode::LC03,
                            None,
                            None,
                            self.filepath.to_string(),
                        )),
                    )
                }
            }
            // return with an error for unknown escape sequence
            _ => {
                Token::new_compound(
                    TokenKind::Escape,
                    body,
                    Some(NodeError::new(
                        ErrorCode::LC02,
                        Some(1..2),
                        None,
                        self.filepath.to_string(),
                    )),
                )
            }
        }
    }

    fn lex_identifier(&mut self, head: Token) -> Token {
        // collect identifier start
        let mut body = vec![head, self.next_terminal(false)];

        // collect leading delimiters before identifier name
        while self.active() && self.is_delimiter_next() {
            body.push(self.next_terminal(true));
        }

        // collect identifier name
        let mut name = Vec::new();
        let mut name_valid = true;
        let mut name_empty = true;

        while self.active() && !self.is_delimiter_next() && self.peek_byte() != b'}' {
            let token = self.next_terminal(false);

            if name_valid {
                match name.len() & 1 {
                    0 if token.kind() != TokenKind::Atom => name_valid = false,
                    1 if token.name() != TokenName::Colon => name_valid = false,
                    _ => (),
                }
            }

            name.push(token);
        }

        if !name.is_empty() {
            name_empty = false;

            if name[name.len() - 1].name() == TokenName::Colon {
                name_valid = false;
            }

            let name_err = if !name_valid {
                Some(NodeError::new(ErrorCode::LI03, None, None, self.filepath.to_string()))
            } else {
                None
            };

            body.push(Token::new_compound(TokenKind::IdentifierName, name, name_err));
        }

        // collect trailing delimiters after identifer name
        while self.active() && self.is_delimiter_next() {
            body.push(self.next_terminal(true));
        }

        // collect identifier arguments
        while self.active() && self.peek_byte() != b'}' {
            let mut argument = Vec::new();

            while self.active() && !self.is_delimiter_next() && self.peek_byte() != b'}' {
                argument.push(self.next_token(true, true));
            }

            if !argument.is_empty() {
                body.push(Token::new_compound(TokenKind::IdentifierArgument, argument, None));
            }

            while self.active() && self.is_delimiter_next() {
                body.push(self.next_terminal(true));
            }
        }

        // collect closing brace
        let mut open = true;

        if self.active() && self.peek_byte() == b'}' {
            body.push(self.next_terminal(false));
            open = false;
        }

        // validate identifier
        let error = {
            let err = |code, bounds| {
                Some(NodeError::new(code, bounds, None, self.filepath.to_string()))
            };

            if open {
                err(ErrorCode::LI01, None)
            } else if name_empty {
                err(ErrorCode::LI02, None)
            } else {
                None
            }
        };

        Token::new_compound(TokenKind::Identifier, body, error)
    }

    fn lex_string(&mut self, head: Token) -> Token {
        // collect string
        let mut body = vec![head];
        let mut open = true;

        while self.active() {
            body.push(self.next_token(false, false));

            if body[body.len() - 1].name() == body[0].name() {
                open = false;
                break;
            }
        }

        // validate string
        let error = {
            if open {
                Some(NodeError::new(ErrorCode::LS01, None, None, self.filepath.to_string()))
            } else {
                None
            }
        };

        Token::new_compound(TokenKind::String, body, error)
    }


    /* -------------------- *
     *         UTILS        *
     * -------------------- */
    fn inc_cursor(&mut self) {
        if self.cursor != 0 {
            self.byte += 1;
        }

        if self.peek_byte() == b'\n' {
            self.col = 0;
            self.line += 1;
        } else {
            self.col += 1;
        }

        self.cursor += 1;
    }

    fn inc_cursor_while(&mut self, cond: fn(&Self) -> bool) {
        while self.active() && cond(self) {
            self.inc_cursor()
        }
    }

    fn next_byte(&mut self) -> u8 {
        let byte = self.peek_byte();
        self.inc_cursor();
        byte
    }

    fn slice_source(&self, start: usize, end: usize) -> Box<str> {
        let bytes = self.source[start..end].to_vec();

        unsafe {
            String::from_utf8_unchecked(bytes).into_boxed_str()
        }
    }

    fn is_valid_ascii_codepoint(codepoint: &str) -> bool {
        u8::from_str_radix(codepoint, 10).is_ok_and(|x| x < 0x80)
    }

    fn is_valid_unicode_codepoint(codepoint: &str) -> bool {
        u32::from_str_radix(codepoint, 16)
            .is_ok_and(|x| (x ^ 0xD800).wrapping_sub(0x800) < 0x110000 - 0x800)
    }
}
