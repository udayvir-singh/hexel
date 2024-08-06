//! Provides parsers for converting source code into an Abstract Syntax Tree (AST).

use std::{mem, collections::VecDeque, sync::Arc};

use bstr::{BStr, ByteSlice};

use super::lexer::Lexer;
use crate::types::{ast::*, error::*, position::*, token::*};
use crate::utils::consts::*;

/* -------------------- *
 *        PARSER        *
 * -------------------- */
/// Represents a parser which converts source code into an AST.
pub struct Parser<'a> {
    iter: IterativeParser<'a>,
}

impl<'a> IntoIterator for Parser<'a> {
    type Item = Arc<AstNode>;
    type IntoIter = IterativeParser<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter
    }
}

impl<'a> Parser<'a> {
    /* -------------------- *
     *        PUBLIC        *
     * -------------------- */
    /// Creates a new instance of [`Parser`].
    #[inline]
    #[must_use]
    pub fn new(
        source: &'a [u8],
        filepath: String,
        byte_offset: Option<usize>,
        line_offset: Option<u32>,
    ) -> Self {
        Self {
            iter: IterativeParser::new(source, filepath, byte_offset, line_offset),
        }
    }

    /// Parses the source code producing an Abstract Syntax Tree (AST).
    #[must_use]
    pub fn parse(self) -> Vec<Arc<AstNode>> {
        self.iter.collect()
    }
}


/* -------------------- *
 *   ITERATIVE PARSER   *
 * -------------------- */
#[derive(PartialEq, Eq)]
enum ParserState {
    Header,
    Decorator,
    Request,
}

enum ParserStore {
    Active(Vec<AstNode>),
    Drain(VecDeque<AstNode>),
}

/// todo
pub struct IterativeParser<'a> {
    source: &'a [u8],
    filepath: String,
    byte_offset: usize,
    lexer: Lexer<'a>,
    state: ParserState,
    store: ParserStore,
}

impl<'a> Iterator for IterativeParser<'a> {
    /* -------------------- *
     *       ITERATOR       *
     * -------------------- */
    type Item = Arc<AstNode>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.next_node().map(Arc::new)
    }

}

impl<'a> IterativeParser<'a> {
    /* -------------------- *
     *        PUBLIC        *
     * -------------------- */
    /// Creates a new instance of [`IterativeParser`].
    #[inline]
    #[must_use]
    pub fn new(
        source: &'a [u8],
        filepath: String,
        byte_offset: Option<usize>,
        line_offset: Option<u32>,
    ) -> Self {
        Self {
            source,
            filepath: filepath.clone(),
            byte_offset: byte_offset.unwrap_or(0),
            lexer: Lexer::new(source, filepath, byte_offset, line_offset),
            state: ParserState::Header,
            store: ParserStore::Active(Vec::new()),
        }
    }

    /// todo
    #[inline]
    #[must_use]
    pub fn active(&self) -> bool {
        self.lexer.active() || match &self.store {
            ParserStore::Active(x) => !x.is_empty(),
            ParserStore::Drain(x) => !x.is_empty(),
        }
    }


    /* -------------------- *
     *       NEXT NODE      *
     * -------------------- */
    fn next_node(&mut self) -> Option<AstNode> {
        while self.lexer.active() {
            let (head, head_idx) = self.collect_word(Vec::new(), true);

            if head.is_empty() || head[head_idx].is_delimiter() {
                let line = Line {
                    body: self.collect_line(head),
                    error: None,
                };
                let source = self.get_source(&line);
                let node = AstNode::Line(line, source);

                if self.state == ParserState::Decorator {
                    if let ParserStore::Active(x) = &mut self.store {
                        x.push(node);
                    }
                    continue;
                } else {
                    return Some(node);
                };
            }

            if let Some(node) = match self.state {
                ParserState::Header => self.handle_header_state(head, head_idx),
                ParserState::Decorator => self.handle_decorator_state(head, head_idx),
                ParserState::Request => self.handle_request_state(head, head_idx),
            } {
                return Some(node);
            }
        }

        if let ParserStore::Active(x) = &mut self.store {
            let mut nodes = mem::take(x);

            for node in &mut nodes {
                if let AstNode::RequestComponent(x, _) = node {
                    x.error = Some(ScopedNodeError::new(
                        ErrorCode::PD06,
                        RequestComponentScope::None,
                        None,
                        None,
                        self.filepath.to_string(),
                    ));
                }
            }

            self.store = ParserStore::Drain(nodes.into());
        }

        if let ParserStore::Drain(x) = &mut self.store {
            x.pop_front()
        } else {
            unreachable!()
        }
    }


    /* -------------------- *
     *        HEADER        *
     * -------------------- */
    fn handle_header_state(
        &mut self,
        head: Vec<Token>,
        head_idx: usize,
    ) -> Option<AstNode> {
        match (head[head_idx].kind(), head[head_idx].name()) {
            // parse as statement if line starts with '#' and a valid statement name
            (_, TokenName::Hash)
                if head.len() - head_idx == 2 && head[head_idx + 1].kind() == TokenKind::Atom =>
            {
                Some(self.parse_header_statement(head, head_idx))
            }
            // parse as decorator if line starts with '@'
            (_, TokenName::AtSign) => {
                self.state = ParserState::Decorator;
                self.handle_decorator_state(head, head_idx)
            }
            // parse as request if line starts with a valid HTTP method
            (TokenKind::Atom, _) if Self::is_valid_method(head[head_idx].value()) => {
                self.state = ParserState::Request;
                self.handle_request_state(head, head_idx)
            }
            // handle invalid statement
            _ => {
                let line = Line {
                    body: self.collect_line(head),
                    error: Some(NodeError::new(
                        ErrorCode::PS01,
                        None,
                        None,
                        self.filepath.to_string(),
                    )),
                };
                let source = self.get_source(&line);

                Some(AstNode::Line(line, source))
            }
        }
    }

    fn parse_header_statement(&mut self, head: Vec<Token>, head_idx: usize) -> AstNode {
        match unsafe { head[head_idx + 1].value().to_str_unchecked() } {
            // parse assign statement
            "var" => self.parse_assign_statement(head),
            // parse macro statement
            "macro" => self.parse_macro_statement(head),
            // parse require statement
            "require" => self.parse_require_statement(head),
            // handle unknown statement
            _ => {
                let line = Line {
                    body: self.collect_line(head),
                    error: Some(NodeError::new(
                        ErrorCode::PS02,
                        Some(head_idx + 1..head_idx + 2),
                        None,
                        self.filepath.to_string(),
                    )),
                };
                let source = self.get_source(&line);

                AstNode::Line(line, source)
            }
        }
    }

    fn parse_assign_statement(&mut self, head: Vec<Token>) -> AstNode {
        // collect neck
        let (neck, neck_len) = self.collect_identifier_names(
            Vec::new(),
            ErrorCode::PA06,
            |l| l.peek_byte() == b'=' || l.is_newline_next(),
        );

        // early return if neck is invalid
        if neck_len == 0 || !(self.lexer.active() && self.lexer.peek_byte() == b'=') {
            let code = if neck_len == 0 {
                ErrorCode::PA01
            } else {
                ErrorCode::PA02
            };

            let statement = Statement {
                kind: StatementKind::Assign,
                head,
                neck,
                limb: Vec::new(),
                body: Vec::new(),
                tail: self.collect_line(Vec::new()),
                error: Some(ScopedNodeError::new(
                    code,
                    StatementScope::None,
                    None,
                    None,
                    self.filepath.to_string(),
                )),
            };
            let source = self.get_source(&statement);

            return AstNode::Statement(statement, source);
        }

        // collect equal operator
        let limb = vec![self.lexer.next_terminal(false)];

        // collect body
        let (body, body_idx) = self.collect_word(Vec::new(), false);

        // collect tail
        let (tail, tail_len) = self.collect_line_with_len(Vec::new());

        // validate statement
        let error = {
            let err = |code, scope, bounds| {
                Some(ScopedNodeError::new(code, scope, bounds, None, self.filepath.to_string()))
            };

            if body.is_empty() || body[body_idx].is_delimiter() {
                err(ErrorCode::PA03, StatementScope::Body, None)
            } else if neck_len > 1 {
                err(ErrorCode::PA04, StatementScope::Neck, Some(neck.inner_bounds()))
            } else if tail_len > 0 {
                err(ErrorCode::PA05, StatementScope::Tail, Some(tail.inner_bounds()))
            } else if head[0].kind() == TokenKind::Whitespace {
                err(ErrorCode::PA07, StatementScope::Head, Some(0..1))
            } else {
                None
            }
        };

        let statement = Statement {
            kind: StatementKind::Assign,
            head,
            neck,
            limb,
            body,
            tail,
            error,
        };
        let source = self.get_source(&statement);

        AstNode::Statement(statement, source)
    }

    fn parse_macro_statement(&mut self, head: Vec<Token>) -> AstNode {
        // collect identifier
        let neck = self.collect_whitespace(Vec::new());

        let (neck, neck_len) = self.collect_identifier_names(
            neck,
            ErrorCode::PM02,
            |l| l.is_delimiter_next(),
        );

        // collect parameters
        let mut limb = Vec::new();
        let mut index = Vec::new();
        let mut multi_count = 0;
        let mut param_error = None;

        while self.lexer.active() {
            let token = self.lexer.next_terminal(true);
            let token_kind = token.kind();
            let token_val = token.value();

            // validate parameter
            if param_error.is_none() {
                let param_bounds = neck.len()..neck.len() + 1;
                let param_name = if let b'*' | b'+' = token_val[0] {
                    multi_count += 1;
                    &token_val[1..]
                } else {
                    token_val
                };

                match token_kind {
                    TokenKind::Special => {
                        param_error = Some((ErrorCode::PM03, param_bounds));
                    }
                    TokenKind::Atom if !Self::is_valid_macro_param(param_name) => {
                        param_error = Some((ErrorCode::PM03, param_bounds));
                    }
                    TokenKind::Atom if multi_count > 1 => {
                        param_error = Some((ErrorCode::PM04, param_bounds));
                    }
                    TokenKind::Atom if index.iter().any(|x| x == param_name) => {
                        param_error = Some((ErrorCode::PM05, param_bounds));
                    }
                    TokenKind::Atom => {
                        index.push(param_name.to_string());
                    }
                    _ => {}
                }
            }

            // push parameter
            limb.push(token);

            if token_kind == TokenKind::Newline {
                break;
            }
        }

        // collect body and tail
        let mut body = Vec::new();
        let mut tail = Vec::new();
        let mut tail_len = 0;

        while self.lexer.active() {
            // break loop at end statement
            if self.lexer.peek_word() == b"#end" {
                (tail, tail_len) = self.collect_line_with_len(tail);
                break;
            }

            // collect body line by line
            if !self.lexer.is_newline_next() {
                body.push(self.lexer.next_raw(|l| !l.is_newline_next()));
            }

            if self.lexer.active() {
                body.push(self.lexer.next_terminal(false));
            }
        }

        // validate statement
        let error = {
            let err = |code, scope, bounds| {
                Some(ScopedNodeError::new(code, scope, bounds, None, self.filepath.to_string()))
            };

            if neck_len == 0 {
                err(ErrorCode::PM01, StatementScope::Neck, None)
            } else if let Some((code, bounds)) = param_error {
                err(code, StatementScope::Limb, Some(bounds))
            } else if tail_len == 0 {
                err(ErrorCode::PM06, StatementScope::Tail, None)
            } else if tail_len > 2 {
                let bounds = tail.inner_bounds_after(tail.inner_bound_start() + 2);

                err(ErrorCode::PM07, StatementScope::Tail, Some(bounds))
            } else if head[0].kind() == TokenKind::Whitespace {
                err(ErrorCode::PM08, StatementScope::Head, Some(0..1))
            } else if tail[0].kind() == TokenKind::Whitespace {
                err(ErrorCode::PM09, StatementScope::Tail, Some(0..1))
            } else {
                None
            }
        };

        let statement = Statement {
            kind: StatementKind::Macro,
            head,
            neck,
            limb,
            body,
            tail,
            error,
        };
        let source = self.get_source(&statement);

        AstNode::Statement(statement, source)
    }

    fn parse_require_statement(&mut self, head: Vec<Token>) -> AstNode {
        // collect neck
        let (mut neck, neck_idx) = self.collect_word(Vec::new(), true);

        // early return if neck is empty
        if neck.is_empty() || neck[neck_idx].is_delimiter() {
            let statement = Statement {
                kind: StatementKind::Require,
                head,
                neck,
                limb: Vec::new(),
                body: Vec::new(),
                tail: self.collect_line(Vec::new()),
                error: Some(ScopedNodeError::new(
                    ErrorCode::PR01,
                    StatementScope::None,
                    None,
                    None,
                    self.filepath.to_string(),
                )),
            };
            let source = self.get_source(&statement);

            return AstNode::Statement(statement, source);
        }

        // validate module name
        let neck_valid = {
            neck_idx == neck.len() - 1 && Self::is_valid_module_name(neck[neck_idx].value())
        };

        // collect trailing whitespace after neck
        neck = self.collect_whitespace(neck);

        // collect body
        let (body, body_error) = self.parse_require_statement_body();

        // collect tail
        let (tail, tail_len) = self.collect_line_with_len(Vec::new());

        // validate statement
        let error = {
            let err = |code, scope, bounds| {
                Some(ScopedNodeError::new(code, scope, bounds, None, self.filepath.to_string()))
            };

            if body_error.is_some() {
                body_error
            } else if !neck_valid {
                err(ErrorCode::PR02, StatementScope::Neck, Some(neck.inner_bounds()))
            } else if tail_len > 0 {
                err(ErrorCode::PR08, StatementScope::Tail, Some(tail.inner_bounds()))
            } else if head[0].kind() == TokenKind::Whitespace {
                err(ErrorCode::PR09, StatementScope::Head, Some(0..1))
            } else {
                None
            }
        };

        let statement = Statement {
            kind: StatementKind::Require,
            head,
            neck,
            limb: Vec::new(),
            body,
            tail,
            error,
        };
        let source = self.get_source(&statement);

        AstNode::Statement(statement, source)
    }

    fn parse_require_statement_body(&mut self) -> (Vec<Token>, Option<ScopedNodeError<StatementScope>>) {
        // early return if EOB or EOL is reached
        if !self.lexer.active() || self.lexer.is_newline_next() {
            return (Vec::new(), None);
        }

        // utils
        let filepath = self.filepath.clone();
        let err = |code, scope, bounds| {
            Some(ScopedNodeError::new(code, scope, bounds, None, filepath))
        };

        // collect at-sign
        let mut body = vec![self.lexer.next_token(true, true)];

        if body[0].name() != TokenName::AtSign {
            return (body, err(ErrorCode::PR03, StatementScope::Body, None));
        }

        if !self.lexer.active() || self.lexer.is_delimiter_next() {
            return (body, err(ErrorCode::PR04, StatementScope::Body, None));
        }

        // parse body
        body.push(self.lexer.next_token(true, true));

        if self.lexer.active()
            && self.lexer.peek_byte() == b'{'
            && body[1].kind() == TokenKind::Atom
        {
            body.push(self.lexer.next_terminal(false));
        }

        match (body[body.len() - 1].kind(), body[body.len() - 1].name()) {
            // parse atomic prefix
            (TokenKind::Atom, _) => {
                // collect terminal tokens after prefix
                while self.lexer.active() && !self.lexer.is_delimiter_next() {
                    body.push(self.lexer.next_terminal(true));
                }

                // validate body
                let body_err = if body.len() > 2 {
                    err(ErrorCode::PR03, StatementScope::Body, Some(2..body.len()))
                } else {
                    None
                };

                (body, body_err)
            }
            // parse identifiers inside braces
            (_, TokenName::OpenBrace) => {
                // collect identifiers inside braces
                let (mut body, body_len) = self.collect_identifier_names(
                    body,
                    ErrorCode::PR07,
                    |l| l.peek_byte() == b'}',
                );

                // validate closing brace
                if !self.lexer.active() {
                    return (body, err(ErrorCode::PR05, StatementScope::None, None));
                }

                // collect closing brace
                body.push(self.lexer.next_terminal(false));

                // validate body
                let body_err = if body_len == 0 {
                    err(ErrorCode::PR06, StatementScope::Body, None)
                } else {
                    None
                };

                (body, body_err)
            }
            // handle invalid prefix
            _ => {
                (body, err(ErrorCode::PR03, StatementScope::Body, Some(1..2)))
            }
        }
    }


    /* -------------------- *
     *       DECORATOR      *
     * -------------------- */
    fn handle_decorator_state(
        &mut self,
        head: Vec<Token>,
        head_idx: usize,
    ) -> Option<AstNode> {
        match (head[head_idx].kind(), head[head_idx].name()) {
            // parse as decorator if line starts with '@'
            (_, TokenName::AtSign) => {
                let node = self.parse_decorator(head, head_idx);

                if let ParserStore::Active(x) = &mut self.store {
                    x.push(node);
                }

                None
            }
            // parse as request if line starts with a valid HTTP method
            (TokenKind::Atom, _) if Self::is_valid_method(head[head_idx].value()) => {
                self.state = ParserState::Request;
                self.handle_request_state(head, head_idx)
            }
            // handle invalid statement
            _ => {
                let line = Line {
                    body: self.collect_line(head),
                    error: Some(NodeError::new(
                        ErrorCode::PD01,
                        None,
                        None,
                        self.filepath.to_string(),
                    )),
                };
                let source = self.get_source(&line);

                let node = AstNode::Line(line, source);

                if let ParserStore::Active(x) = &mut self.store {
                    x.push(node);
                }

                None
            }
        }
    }

    fn parse_decorator(&mut self, head: Vec<Token>, head_idx: usize) -> AstNode {
        // collect body
        let mut body = Vec::new();
        let mut body_empty = true;

        while self.lexer.active() {
            let mut argument = Vec::new();

            while self.lexer.active() && !self.lexer.is_delimiter_next() {
                argument.push(self.lexer.next_token(true, true));
            }

            if !argument.is_empty() {
                body.push(Token::new_compound(TokenKind::DecoratorArgument, argument, None));
                body_empty = false;
            }

            while self.lexer.active() && matches!(self.lexer.peek_byte(), b';' | b' ' | b'\t') {
                body.push(self.lexer.next_terminal(true));
            }

            if self.lexer.active() && self.lexer.is_newline_next() {
                body.push(self.lexer.next_terminal(false));
                break;
            }
        }

        // validate decorator
        let error = {
            let err = |code, scope, bounds| {
                Some(ScopedNodeError::new(code, scope, bounds, None, self.filepath.to_string()))
            };

            if !(head.len() - head_idx == 2 && head[head_idx + 1].kind() == TokenKind::Atom) {
                err(ErrorCode::PD02, RequestComponentScope::Head, Some(head_idx..head.len()))
            } else if !Self::is_valid_decorator(head[head_idx + 1].value()) {
                err(ErrorCode::PD03, RequestComponentScope::Head, Some(head_idx+1..head.len()))
            } else if body_empty {
                err(ErrorCode::PD04, RequestComponentScope::None, None)
            } else if head[0].kind() == TokenKind::Whitespace {
                err(ErrorCode::PD05, RequestComponentScope::Head, Some(0..1))
            } else {
                None
            }
        };

        let request_component = RequestComponent {
            kind: RequestComponentKind::Decorator,
            head,
            body,
            error,
        };
        let source = self.get_source(&request_component);

        AstNode::RequestComponent(request_component, source)
    }


    /* -------------------- *
     *        REQUEST       *
     * -------------------- */
    fn handle_request_state(
        &mut self,
        head: Vec<Token>,
        head_idx: usize,
    ) -> Option<AstNode> {
        // reset state
        self.state = ParserState::Decorator;

        // dequeue decorators
        let decorators = if let ParserStore::Active(x) = &mut self.store {
            mem::take(x)
        } else {
            unreachable!()
        };

        // parse request
        let request = Request {
            decorators,
            line: self.parse_request_line(head, head_idx),
            headers: self.parse_request_headers(),
            body: self.parse_request_body(),
        };
        let source = self.get_source(&request);

        Some(AstNode::Request(request, source))
    }

    fn parse_request_line(&mut self, head: Vec<Token>, head_idx: usize) -> RequestComponent {
        // collect request URI and version
        let mut body = Vec::new();
        let mut marker = 0;

        while marker < 2 && self.lexer.active() && !self.lexer.is_newline_next() {
            let mut argument = Vec::new();

            while self.lexer.active() && !self.lexer.is_delimiter_next() {
                argument.push(self.lexer.next_token(true, true));
            }

            if !argument.is_empty() {
                let token_kind = match marker {
                    0 => TokenKind::RequestURI,
                    1 => TokenKind::RequestVersion,
                    _ => unreachable!(),
                };

                body.push(Token::new_compound(token_kind, argument, None));
                marker += 1;
            }

            while self.lexer.active() && matches!(self.lexer.peek_byte(), b';' | b' ' | b'\t') {
                body.push(self.lexer.next_terminal(true));
            }
        }

        // collect end of line
        let eol_idx = body.len();
        let eol_len;

        (body, eol_len) = self.collect_line_with_len(body);

        // validate request line
        let error = {
            let err = |code, scope, bounds| {
                Some(ScopedNodeError::new(code, scope, bounds, None, self.filepath.to_string()))
            };

            if head.iter().skip(head_idx + 1).filter(|x| !x.is_delimiter()).count() > 0 {
                err(ErrorCode::PH01, RequestComponentScope::Head, Some(head_idx..head.len()))
            } else if marker == 0 {
                err(ErrorCode::PH02, RequestComponentScope::None, None)
            } else if eol_len != 0 {
                err(ErrorCode::PH03, RequestComponentScope::Body, Some(body.inner_bounds_after(eol_idx)))
            } else if head[0].kind() == TokenKind::Whitespace {
                err(ErrorCode::PH04, RequestComponentScope::Head, Some(0..1))
            } else {
                None
            }
        };

        RequestComponent {
            kind: RequestComponentKind::Line,
            head,
            body,
            error,
        }
    }

    fn parse_request_headers(&mut self) -> Vec<AstNode> {
        let mut headers = Vec::new();

        while self.lexer.active() {
            // break if next token is a decorator or a HTTP method
            let next_word = self.lexer.peek_word();

            if (!next_word.is_empty() && next_word[0] == b'@')
                || HTTP_METHODS.iter().any(|x| x.as_bytes() == next_word)
            {
                break;
            }

            // collect head
            let mut head = self.collect_whitespace(Vec::new());
            let mut head_empty = true;
            let mut head_whitespace_range = None;
            let mut is_comment = true;

            while self.lexer.active()
                && self.lexer.peek_byte() != b':'
                && !self.lexer.is_newline_next()
            {
                let token = self.lexer.next_token(true, true);
                let token_kind = token.kind();

                if token_kind != TokenKind::Comment {
                    is_comment = false;
                }

                if token_kind != TokenKind::Whitespace {
                    head_empty = false;
                } else if head_whitespace_range.is_none() {
                    head_whitespace_range = Some(head.len()..head.len() + 1);
                }

                head.push(token);
            }

            // handle normal lines
            if head_empty || is_comment {
                let line = Line {
                    body: self.collect_line(head),
                    error: None,
                };
                let source = self.get_source(&line);

                headers.push(AstNode::Line(line, source));

                if head_empty {
                    // break if head is empty
                    break;
                } else {
                    // skip if current line is a comment
                    continue;
                }
            }

            // handle missing colon operator
            if self.lexer.peek_byte() != b':' {
                let header = RequestComponent {
                    kind: RequestComponentKind::Header,
                    head: self.collect_line(head),
                    body: Vec::new(),
                    error: Some(ScopedNodeError::new(
                        ErrorCode::PH05,
                        RequestComponentScope::None,
                        None,
                        None,
                        self.filepath.to_string()
                    )),
                };
                let source = self.get_source(&header);

                headers.push(AstNode::RequestComponent(header, source));
                continue;
            }

            // collect colon operator
            head.push(self.lexer.next_terminal(false));

            // collect body
            let (body, body_len) = self.collect_line_with_len(Vec::new());

            // validate header
            let error = {
                let err = |code, scope, bounds| {
                    Some(ScopedNodeError::new(code, scope, bounds, None, self.filepath.to_string()))
                };

                if body_len == 0 {
                    err(ErrorCode::PH06, RequestComponentScope::None, None)
                } else if head_whitespace_range.is_some() {
                    err(ErrorCode::PH07, RequestComponentScope::Head, head_whitespace_range)
                } else if head[0].kind() == TokenKind::Whitespace {
                    err(ErrorCode::PH08, RequestComponentScope::Head, Some(0..1))
                } else {
                    None
                }
            };

            // push header
            let header = RequestComponent {
                kind: RequestComponentKind::Header,
                head,
                body,
                error,
            };
            let source = self.get_source(&header);

            headers.push(AstNode::RequestComponent(header, source));
        }

        headers
    }

    fn parse_request_body(&mut self) -> Vec<Token> {
        let mut body = Vec::new();

        while self.lexer.active() {
            // break if next token is a decorator or a HTTP method
            let next_word = self.lexer.peek_word();

            if (!next_word.is_empty() && next_word[0] == b'@')
                || HTTP_METHODS.iter().any(|x| x.as_bytes() == next_word)
            {
                break;
            }

            // collect next line into body
            let lex_comment = !next_word.is_empty() && next_word[0] == b';';

            while self.lexer.active() {
                body.push(self.lexer.next_token(lex_comment, false));

                if body[body.len() - 1].kind() == TokenKind::Newline {
                    break;
                }
            }
        }

        body
    }


    /* -------------------- *
     *        UTILS         *
     * -------------------- */
    fn get_source(&self, x: &impl GetPosition) -> Box<BStr> {
        let i = x.byte_start() - self.byte_offset;
        let j = x.byte_end() - self.byte_offset;

        self.source[i..=j].to_vec().into_boxed_slice().into()
    }

    fn collect_whitespace(&mut self, mut stack: Vec<Token>) -> Vec<Token> {
        while self.lexer.active() && matches!(self.lexer.peek_byte(), b' ' | b'\t') {
            stack.push(self.lexer.next_terminal(false));
        }

        stack
    }

    fn collect_line(&mut self, mut stack: Vec<Token>) -> Vec<Token> {
        while self.lexer.active() {
            stack.push(self.lexer.next_token(true, true));

            if stack[stack.len() - 1].kind() == TokenKind::Newline {
                break;
            }
        }

        stack
    }

    fn collect_line_with_len(&mut self, mut stack: Vec<Token>) -> (Vec<Token>, usize) {
        let mut len = 0;

        while self.lexer.active() {
            let token = self.lexer.next_token(true, true);
            let token_kind = token.kind();

            if !token.is_delimiter() {
                len += 1;
            }

            stack.push(token);

            if token_kind == TokenKind::Newline {
                break;
            }
        }

        (stack, len)
    }

    fn collect_word(&mut self, mut stack: Vec<Token>, terminal_only: bool) -> (Vec<Token>, usize) {
        let mut word_idx = 0;

        while self.lexer.active() && matches!(self.lexer.peek_byte(), b';' | b' ' | b'\t') {
            stack.push(self.lexer.next_terminal(true));
        }

        if self.lexer.active() && !self.lexer.is_delimiter_next() {
            word_idx = stack.len();

            while self.lexer.active() && !self.lexer.is_delimiter_next() {
                stack.push({
                    if terminal_only {
                        self.lexer.next_terminal(true)
                    } else {
                        self.lexer.next_token(true, true)
                    }
                });
            }
        }

        (stack, word_idx)
    }

    fn collect_identifier_names(
        &mut self,
        mut stack: Vec<Token>,
        invalid_code: ErrorCode,
        stopcond: fn(&Lexer) -> bool,
    ) -> (Vec<Token>, usize) {
        let mut len = 0;

        while self.lexer.active() && !stopcond(&self.lexer) {
            let mut name = Vec::new();
            let mut name_valid = true;

            while self.lexer.active() && !self.lexer.is_delimiter_next() && !stopcond(&self.lexer) {
                let token = self.lexer.next_terminal(true);

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
                if name[name.len() - 1].name() == TokenName::Colon {
                    name_valid = false;
                }

                let name_err = if !name_valid {
                    Some(NodeError::new(invalid_code.clone(), None, None, self.filepath.to_string()))
                } else {
                    None
                };

                stack.push(Token::new_compound(TokenKind::IdentifierName, name, name_err));
                len += 1;
            }

            while self.lexer.active() && self.lexer.is_delimiter_next() && !stopcond(&self.lexer) {
                stack.push(self.lexer.next_terminal(true));
            }
        }

        (stack, len)
    }

    fn is_valid_method(name: &BStr) -> bool {
        HTTP_METHODS.iter().any(|x| x == name)
    }

    fn is_valid_decorator(name: &BStr) -> bool {
        DECORATORS.iter().any(|x| x == name)
    }

    fn is_valid_macro_param(param: &BStr) -> bool {
        const ILLEGAL_NAMES: [&str; 22] = [
            "and", "break", "do", "else", "elseif", "end", "false", "for",
            "function", "goto", "if", "in", "local", "nil", "not", "or",
            "repeat", "return", "then", "true", "until", "while",
        ];

        !param.is_empty()
            && !ILLEGAL_NAMES.iter().any(|x| x == param)
            && !param[0].is_ascii_digit()
            && param.iter().all(|b| matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'))
    }

    fn is_valid_module_name(name: &BStr) -> bool {
        name.split(|&x| x == b'/').all(|x| {
            !x.is_empty()
                && x.iter().all(|b| matches!(b, b'a'..=b'z' | b'A'..=b'A' | b'0'..=b'9' | b'-' | b'_'))
        })
    }
}


/* -------------------- *
 *      LAZY PARSER     *
 * -------------------- */
enum LazyParserState<'a> {
    Top(usize, Option<Arc<AstNode>>),
    Middle(IterativeParser<'a>),
    Bottom(PositionDelta),
}

/// Represents a parser that lazily parses source code reusing the old AST wherever possible.
///
/// # Usage Warning
///
/// [`LazyParser`] has been tested to generate the same output as the normal [`Parser`],
/// However, it should only be used it for highlighting and linting purposes.
///
/// Additionally, consider periodically performing a clean parse for assurance.
pub struct LazyParser<'a> {
    source: &'a [u8],
    old_ast: VecDeque<Arc<AstNode>>,
    filepath: String,
    state: LazyParserState<'a>,
}

/// todo
#[derive(Debug, Clone)]
pub enum LazyNodeStatus {
    /// todo
    Stale,
    /// todo
    Moved,
    /// todo
    Fresh,
}

impl<'a> Iterator for LazyParser<'a> {
    /* -------------------- *
     *       ITERATOR       *
     * -------------------- */
    type Item = (Arc<AstNode>, LazyNodeStatus);

    fn next(&mut self) -> Option<Self::Item> {
        self.next_node()
    }
}

impl<'a> LazyParser<'a> {
    /* -------------------- *
     *        PUBLIC        *
     * -------------------- */
    /// Creates a new [`LazyParser`] instance.
    #[inline]
    #[must_use]
    pub fn new(
        source: &'a [u8],
        old_ast: Vec<Arc<AstNode>>,
        filepath: String,
    ) -> Self {
        Self {
            source,
            old_ast: old_ast.into(),
            filepath,
            state: LazyParserState::Top(0, None),
        }
    }


    /* -------------------- *
     *       NEXT NODE      *
     * -------------------- */
    fn next_node(&mut self) -> Option<(Arc<AstNode>, LazyNodeStatus)> {
        self.next_top_node()
            .or_else(|| self.next_middle_node())
            .or_else(|| self.next_bottom_node())
    }

    fn next_top_node(&mut self) -> Option<(Arc<AstNode>, LazyNodeStatus)> {
        if let LazyParserState::Top(change_start, ref prev_node) = self.state {
            if let Some(old_node) = self.old_ast.front() {
                let reference = old_node.source();

                if self.source[change_start..].starts_with(reference) {
                    if let LazyParserState::Top(change_start, prev_node) = &mut self.state {
                        *change_start += reference.len();
                        *prev_node = Some(old_node.clone());
                    }

                    return Some((
                        self.old_ast.pop_front().unwrap(),
                        LazyNodeStatus::Stale,
                    ));
                }
            }

            // calculate change end
            let mut change_end = self.source.len();
            let mut offset = self.old_ast.len();

            for old_node in self.old_ast.iter().rev() {
                let reference = old_node.source();

                if !self.source[..change_end].ends_with(reference) {
                    break;
                }

                change_end -= reference.len();
                offset -= 1;
            }

            // drop until offset
            for _ in 0..offset {
                self.old_ast.pop_front();
            }

            if change_start < change_end {
                self.state = LazyParserState::Middle(IterativeParser::new(
                    &self.source[change_start..change_end],
                    self.filepath.clone(),
                    Some(change_start),
                    Some(prev_node.as_ref().map_or(1, |x| x.line_end())),
                ));
            } else {
                let delta = prev_node.as_ref().and_then(|x| {
                    self.old_ast
                        .front()
                        .map(|y| Self::calculate_delta(&x, y))
                }).unwrap_or_default();

                self.state = LazyParserState::Bottom(delta);
            }
        }

        None
    }

    fn next_middle_node(&mut self) -> Option<(Arc<AstNode>, LazyNodeStatus)> {
        if let LazyParserState::Middle(parser) = &mut self.state {
            let mut node = parser.next()?;

            if node.has_eob_error() {
                let byte_start = node.byte_start();
                let line_start = node.line_start();

                *parser = IterativeParser::new(
                    &self.source[byte_start..],
                    self.filepath.clone(),
                    Some(byte_start),
                    Some(line_start),
                );

                node = parser.next().unwrap();

                self.old_ast = VecDeque::new();
            }

            if !parser.active() {
                let delta = self.old_ast
                    .front()
                    .map(|x| Self::calculate_delta(&node, x))
                    .unwrap_or_default();

                self.state = LazyParserState::Bottom(delta);
            }

            Some((node, LazyNodeStatus::Fresh))
        } else {
            None
        }
    }

    fn next_bottom_node(&mut self) -> Option<(Arc<AstNode>, LazyNodeStatus)> {
        if let LazyParserState::Bottom(delta) = self.state {
            if let Some(node) = self.old_ast.pop_front() {
                return if delta.is_zero() {
                    Some((node, LazyNodeStatus::Stale))
                } else {
                    let mut node = Arc::unwrap_or_clone(node);

                    node.update_position(delta);

                    Some((Arc::new(node), LazyNodeStatus::Moved))
                };
            }
        }

        None
    }


    /* -------------------- *
     *        UTILS         *
     * -------------------- */
    fn calculate_delta(x: &AstNode, y: &AstNode) -> PositionDelta {
        let prev_line = x.line_end();
        let prev_byte = x.byte_end();
        let next_line = y.line_start();
        let next_byte = y.byte_start().saturating_sub(prev_byte.min(1));

        PositionDelta {
            add: next_byte <= prev_byte,
            byte_delta: prev_byte.abs_diff(next_byte),
            line_delta: prev_line.abs_diff(next_line),
        }
    }
}
