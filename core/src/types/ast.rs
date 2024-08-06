//! todo

use std::fmt::{self, Display};

use bstr::BStr;

use super::{error::*, position::*, token::*};

/* -------------------- *
 *        UTILS         *
 * -------------------- */
#[inline]
fn fmt_nodes<T: Display>(f: &mut fmt::Formatter, collection: &[T]) -> fmt::Result {
    for node in collection {
        node.fmt(f)?;
    }

    Ok(())
}


/* -------------------- *
 *      STATEMENT       *
 * -------------------- */
/// Represents the different types of statements created by the parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementKind {
    Require,
    Assign,
    Macro,
}

/// Represents the scope of an error in a statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementScope {
    Head,
    Neck,
    Limb,
    Body,
    Tail,
    None,
}

/// Represents a statement node created by the parser.
#[derive(Debug, Clone)]
pub struct Statement {
    /// The type of the statement.
    pub kind: StatementKind,
    /// The head of the statement.
    pub head: Vec<Token>,
    /// The neck of the statement.
    pub neck: Vec<Token>,
    /// The limb of the statement.
    pub limb: Vec<Token>,
    /// The body of the statement.
    pub body: Vec<Token>,
    /// The tail of the statement.
    pub tail: Vec<Token>,
    /// An optional error associated with the statement, with a specific scope.
    pub error: Option<ScopedNodeError<StatementScope>>,
}

impl Statement {
    #[inline]
    fn get_first_token(&self) -> &Token {
        &self.head[0]
    }

    fn get_last_token(&self) -> &Token {
        let target = match () {
            _ if !self.tail.is_empty() => &self.tail,
            _ if !self.body.is_empty() => &self.body,
            _ if !self.limb.is_empty() => &self.limb,
            _ if !self.neck.is_empty() => &self.neck,
            _ => &self.head,
        };

        &target[target.len() - 1]
    }
}

impl GetPosition for Statement {
    #[inline]
    fn byte_start(&self) -> usize {
        self.get_first_token().byte_start()
    }

    #[inline]
    fn byte_end(&self) -> usize {
        self.get_last_token().byte_end()
    }

    #[inline]
    fn line_start(&self) -> u32 {
        self.get_first_token().line_start()
    }

    #[inline]
    fn line_end(&self) -> u32 {
        self.get_last_token().line_end()
    }

    #[inline]
    fn col_start(&self) -> u32 {
        self.get_first_token().col_start()
    }

    #[inline]
    fn col_end(&self) -> u32 {
        self.get_last_token().col_end()
    }
}

impl UpdatePosition for Statement {
    fn update_position(&mut self, delta: PositionDelta) {
        self.head.update_position(delta);
        self.neck.update_position(delta);
        self.limb.update_position(delta);
        self.body.update_position(delta);
        self.tail.update_position(delta);
    }
}

impl HasError for Statement {
    #[inline]
    fn has_error(&self) -> bool {
        self.error.is_some()
            || self.neck.has_error()
            || self.body.has_error()
            || self.tail.has_error()
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        self.neck.has_unseen_error()
            || self.body.has_unseen_error()
            || self.tail.has_unseen_error()
    }

    #[inline]
    fn has_eob_error(&self) -> bool {
        self.error.as_ref().is_some_and(|x| STATEMENT_EOB_ERROR_CODES.contains(&x.code()))
            || self.neck.has_eob_error()
            || self.body.has_eob_error()
            || self.tail.has_eob_error()
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_nodes(f, &self.head)?;
        fmt_nodes(f, &self.neck)?;
        fmt_nodes(f, &self.limb)?;
        fmt_nodes(f, &self.body)?;
        fmt_nodes(f, &self.tail)
    }
}


/* -------------------- *
 *       REQUEST        *
 * -------------------- */
/// Represents the different types of request components created by the parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RequestComponentKind {
    Decorator,
    Line,
    Header,
}

/// Represents the scope of an error in a request component.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RequestComponentScope {
    Head,
    Body,
    None,
}

/// Represents a request component created by the parser.
#[derive(Debug, Clone)]
pub struct RequestComponent {
    /// The type of the request component.
    pub kind: RequestComponentKind,
    /// The head of the request component.
    pub head: Vec<Token>,
    /// The body of the request component.
    pub body: Vec<Token>,
    /// An optional error associated with the component, with a specific scope.
    pub error: Option<ScopedNodeError<RequestComponentScope>>,
}

/// Represents a request node created by the parser.
#[derive(Debug, Clone)]
pub struct Request {
    /// The decorators on top of the request.
    pub decorators: Vec<AstNode>,
    /// The request line containing the HTTP method, URL and version information.
    pub line: RequestComponent,
    /// The headers associated with the request.
    pub headers: Vec<AstNode>,
    /// The body of the request.
    pub body: Vec<Token>,
}

impl Request {
    fn get_first_node(&self) -> &dyn GetPosition {
        if self.decorators.is_empty() {
            &self.line
        } else {
            &self.decorators[0]
        }
    }

    fn get_last_node(&self) -> &dyn GetPosition {
        if !self.body.is_empty() {
            &self.body[self.body.len() - 1]
        } else if !self.headers.is_empty() {
            &self.headers[self.headers.len() - 1]
        } else {
            &self.line
        }
    }
}

impl RequestComponent {
    #[inline]
    fn get_first_token(&self) -> &Token {
        &self.head[0]
    }

    fn get_last_token(&self) -> &Token {
        if !self.body.is_empty() {
            &self.body[self.body.len() - 1]
        } else {
            &self.head[self.head.len() - 1]
        }
    }
}

impl GetPosition for RequestComponent {
    #[inline]
    fn byte_start(&self) -> usize {
        self.get_first_token().byte_start()
    }

    #[inline]
    fn byte_end(&self) -> usize {
        self.get_last_token().byte_end()
    }

    #[inline]
    fn line_start(&self) -> u32 {
        self.get_first_token().line_start()
    }

    #[inline]
    fn line_end(&self) -> u32 {
        self.get_last_token().line_end()
    }

    #[inline]
    fn col_start(&self) -> u32 {
        self.get_first_token().col_start()
    }

    #[inline]
    fn col_end(&self) -> u32 {
        self.get_last_token().col_end()
    }
}

impl GetPosition for Request {
    #[inline]
    fn byte_start(&self) -> usize {
        self.get_first_node().byte_start()
    }

    #[inline]
    fn byte_end(&self) -> usize {
        self.get_last_node().byte_end()
    }

    #[inline]
    fn line_start(&self) -> u32 {
        self.get_first_node().line_start()
    }

    #[inline]
    fn line_end(&self) -> u32 {
        self.get_last_node().line_end()
    }

    #[inline]
    fn col_start(&self) -> u32 {
        self.get_first_node().col_start()
    }

    #[inline]
    fn col_end(&self) -> u32 {
        self.get_last_node().col_end()
    }
}

impl UpdatePosition for RequestComponent {
    fn update_position(&mut self, delta: PositionDelta) {
        self.head.update_position(delta);
        self.body.update_position(delta);
    }
}

impl UpdatePosition for Request {
    fn update_position(&mut self, delta: PositionDelta) {
        self.decorators.update_position(delta);
        self.line.update_position(delta);
        self.headers.update_position(delta);
        self.body.update_position(delta);
    }
}

impl HasError for RequestComponent {
    #[inline]
    fn has_error(&self) -> bool {
        self.error.is_some()
            || self.head.has_error()
            || self.body.has_error()
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        self.head.has_unseen_error()
            || self.body.has_unseen_error()
    }

    #[inline]
    fn has_eob_error(&self) -> bool {
        self.head.has_eob_error()
            || self.body.has_eob_error()
    }
}

impl HasError for Request {
    #[inline]
    fn has_error(&self) -> bool {
        self.decorators.has_error()
            || self.line.has_error()
            || self.headers.has_error()
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        self.line.has_unseen_error()
            || self.headers.has_unseen_error()
    }

    #[inline]
    fn has_eob_error(&self) -> bool {
        self.line.has_eob_error()
            || self.headers.has_eob_error()
    }
}

impl Display for RequestComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_nodes(f, &self.head)?;
        fmt_nodes(f, &self.body)
    }
}

impl Display for Request {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_nodes(f, &self.decorators)?;
        self.line.fmt(f)?;
        fmt_nodes(f, &self.headers)?;
        fmt_nodes(f, &self.body)
    }
}


/* -------------------- *
 *         LINE         *
 * -------------------- */
/// Represents a line node created by the parser.
#[derive(Debug, Clone)]
pub struct Line {
    /// The body of the line.
    pub body: Vec<Token>,
    /// An optional error associated with the line.
    pub error: Option<NodeError>,
}

impl GetPosition for Line {
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

impl UpdatePosition for Line {
    #[inline]
    fn update_position(&mut self, delta: PositionDelta) {
        self.body.update_position(delta);
    }
}

impl HasError for Line {
    #[inline]
    fn has_error(&self) -> bool {
        self.error.is_some() || self.body.has_error()
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        self.body.has_unseen_error()
    }

    fn has_eob_error(&self) -> bool {
        self.body.has_eob_error()
    }
}

impl Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_nodes(f, &self.body)
    }
}


/* -------------------- *
 *          AST         *
 * -------------------- */
/// Represents a node in the AST created by the parser.
#[derive(Debug, Clone)]
pub enum AstNode {
    /// A line node created by the parser.
    Line(Line, Box<BStr>),
    /// A statement node created by the parser.
    Statement(Statement, Box<BStr>),
    /// A request node created by the parser.
    Request(Request, Box<BStr>),
    /// A request component created by the parser.
    RequestComponent(RequestComponent, Box<BStr>),
}

impl AstNode {
    /// todo
    pub fn source(&self) -> &BStr {
        match self {
            AstNode::Line(_, s) => &s,
            AstNode::Statement(_, s) => &s,
            AstNode::Request(_, s) => &s,
            AstNode::RequestComponent(_, s) => &s,
        }
    }
}

impl GetPosition for AstNode {
    #[inline]
    fn byte_start(&self) -> usize {
        match self {
            AstNode::Line(x, _) => x.byte_start(),
            AstNode::Statement(x, _) => x.byte_start(),
            AstNode::Request(x, _) => x.byte_start(),
            AstNode::RequestComponent(x, _) => x.byte_start(),
        }
    }

    #[inline]
    fn byte_end(&self) -> usize {
        match self {
            AstNode::Line(x, _) => x.byte_end(),
            AstNode::Statement(x, _) => x.byte_end(),
            AstNode::Request(x, _) => x.byte_end(),
            AstNode::RequestComponent(x, _) => x.byte_end(),
        }
    }

    #[inline]
    fn line_start(&self) -> u32 {
        match self {
            AstNode::Line(x, _) => x.line_start(),
            AstNode::Statement(x, _) => x.line_start(),
            AstNode::Request(x, _) => x.line_start(),
            AstNode::RequestComponent(x, _) => x.line_start(),
        }
    }

    #[inline]
    fn line_end(&self) -> u32 {
        match self {
            AstNode::Line(x, _) => x.line_end(),
            AstNode::Statement(x, _) => x.line_end(),
            AstNode::Request(x, _) => x.line_end(),
            AstNode::RequestComponent(x, _) => x.line_end(),
        }
    }

    #[inline]
    fn col_start(&self) -> u32 {
        match self {
            AstNode::Line(x, _) => x.col_start(),
            AstNode::Statement(x, _) => x.col_start(),
            AstNode::Request(x, _) => x.col_start(),
            AstNode::RequestComponent(x, _) => x.col_start(),
        }
    }

    #[inline]
    fn col_end(&self) -> u32 {
        match self {
            AstNode::Line(x, _) => x.col_end(),
            AstNode::Statement(x, _) => x.col_end(),
            AstNode::Request(x, _) => x.col_end(),
            AstNode::RequestComponent(x, _) => x.col_end(),
        }
    }
}

impl UpdatePosition for AstNode {
    #[inline]
    fn update_position(&mut self, delta: PositionDelta) {
        match self {
            AstNode::Line(x, _) => x.update_position(delta),
            AstNode::Statement(x, _) => x.update_position(delta),
            AstNode::Request(x, _) => x.update_position(delta),
            AstNode::RequestComponent(x, _) => x.update_position(delta),
        }
    }
}

impl HasError for AstNode {
    #[inline]
    fn has_error(&self) -> bool {
        match self {
            AstNode::Line(x, _) => x.has_error(),
            AstNode::Statement(x, _) => x.has_error(),
            AstNode::Request(x, _) => x.has_error(),
            AstNode::RequestComponent(x, _) => x.has_error(),
        }
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        match self {
            AstNode::Line(x, _) => x.has_unseen_error(),
            AstNode::Statement(x, _) => x.has_unseen_error(),
            AstNode::Request(x, _) => x.has_unseen_error(),
            AstNode::RequestComponent(x, _) => x.has_unseen_error(),
        }
    }

    #[inline]
    fn has_eob_error(&self) -> bool {
        match self {
            AstNode::Line(x, _) => x.has_eob_error(),
            AstNode::Statement(x, _) => x.has_eob_error(),
            AstNode::Request(x, _) => x.has_eob_error(),
            AstNode::RequestComponent(x, _) => x.has_eob_error(),
        }
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstNode::Line(x, _) => x.fmt(f),
            AstNode::Statement(x, _) => x.fmt(f),
            AstNode::Request(x, _) => x.fmt(f),
            AstNode::RequestComponent(x, _) => x.fmt(f),
        }
    }
}
