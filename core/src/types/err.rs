//! todo

use std::{
    fmt::{self, Debug, Display},
    ops::Range,
};

/* -------------------- *
 *      ERROR CODE      *
 * -------------------- */
macro_rules! error_code {
    ($($code:ident : $desc:literal),* $(,)?) => {
        /// Represents the error codes used by Hexel.
        #[derive(Clone, PartialEq, Eq)]
        pub enum ErrorCode {
            $(#[doc=$desc] $code,)*
        }

        impl ErrorCode {
            /// Returns the error code as a string.
            #[must_use]
            pub fn as_str(&self) -> &'static str {
                match self {
                    $(ErrorCode::$code => stringify!($code),)*
                }
            }

            /// Returns the description of the error code.
            #[must_use]
            pub fn description(&self) -> &'static str {
                match self {
                    $(ErrorCode::$code => $desc,)*
                }
            }
        }

        impl Debug for ErrorCode {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{} [{}]", self.as_str(), self.description())
            }
        }

        impl Display for ErrorCode {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }
    }
}

error_code! {
    // lex control characters
    LR01: "Unescaped control characters are not allowed",
    // lex char
    LC01: "Incomplete escape sequence",
    LC02: "Unknown escape sequence",
    LC03: "Missing unicode codepoint in escape sequence",
    LC04: "Missing closing brace for escape sequence",
    LC05: "Missing codepoint in escape sequence",
    LC06: "Invalid values after codepoint in escape sequence",
    LC07: "Invalid value inside braces of escape sequence",
    LC08: "Invalid ASCII codepoint in escape sequence",
    LC09: "Invalid unicode codepoint in escape sequence",
    // lex identifier
    LI01: "Missing closing brace for identifier",
    LI02: "Missing identifier name between braces",
    LI03: "Invalid identifier name between braces",
    // lex string
    LS01: "Missing closing quote for string",
    // parse statement
    PS01: "Invalid header statement",
    PS02: "Unknown header statement",
    // parse assignment statement
    PA01: "Missing identifier name in assignment statement",
    PA02: "Missing equal operator in assignment statement",
    PA03: "Missing value in RHS of assignment statement",
    PA04: "Too many values in LHS of assignment statement",
    PA05: "Too many values in RHS of assignment statement",
    PA06: "Invalid identifier name in assignment statement",
    PA07: "Invalid indentation before assignment statement",
    // parse require statement
    PR01: "Missing module name in require statement",
    PR02: "Invalid module name in require statement",
    PR03: "Invalid prefix in require statement",
    PR04: "Missing prefix after '@' in require statement",
    PR05: "Missing closing brace in require statement",
    PR06: "Missing identifier name within braces of require statement",
    PR07: "Invalid identifier name inside braces of require statement",
    PR08: "Invalid values at the end of require statement",
    PR09: "Invalid indentation before require statement",
    // parse macro statement
    PM01: "Missing identifier name in macro statement",
    PM02: "Invalid identifier name in macro statement",
    PM03: "Invalid parameter name in macro statement",
    PM04: "More than one multi-argument parameter in macro statement",
    PM05: "Duplicate parameter in macro statement",
    PM06: "Missing end statement for macro statement",
    PM07: "Invalid values after end statement",
    PM08: "Invalid indentation before macro statement",
    PM09: "Invalid indentation before end statement",
    // parse decorator
    PD01: "Invalid statement in decorator scope",
    PD02: "Invalid decorator name",
    PD03: "Unknown decorator name",
    PD04: "Missing arguments in decorator body",
    PD05: "Invalid indentation before decorator",
    PD06: "Invalid position for a decorator",
    // parse HTTP request
    PH01: "Invalid HTTP method name in request line",
    PH02: "Missing HTTP URL in request line",
    PH03: "Invalid values at the end of request line",
    PH04: "Invalid indentation before request line",
    PH05: "Missing colon operator in request header",
    PH06: "Missing value in RHS of request header",
    PH07: "Invalid whitespace in LHS of request header",
    PH08: "Invalid indentation before request header",
    // collapse context
    CC01: "Invalid identifier name",
    CC02: "No value found for identifier",
    CC03: "Cannot pass arguments to a string value",
    CC04: "Missing required arguments for macro execution",
    CC05: "Error while executing Lua macro",
    CC06: "Missing required arguments for standard function",
    CC07: "Error while calling standard function",
    // collapse require statement
    CR01: "File corresponding to module does not exist",
    CR02: "Error while reading module file",
    CR03: "Identifier doesn't exist in module",
    // collapse decorator
    CD01: "Invalid identifier name in decorator arguments",
    CD02: "No value found for identifier in decorator arguments",
    CD03: "Invalid identifier type in decorator arguments",
    CD04: "Poly-parametric macro not allowed in decorator arguments",
    CD05: "Error while executing Lua macro in decorator arguments",
    CD06: "Poly-parametric standard function not allowed in decorator arguments",
    CD07: "Error while calling standard function in decorator arguments",
    CD08: "Error while parsing JSON output in decorator arguments",
    CD09: "Invalid name in decorator arguments",
    CD10: "Invalid number in decorator arguments",
    CD11: "Invalid duration in decorator arguments",
    CD12: "Error while reading file in decorator arguments",
    // collapse HTTP request
    CH01: "Unknown HTTP version in request",
    CH02: "Invalid HTTP version in request",
    CH03: "Invalid HTTP URL in request",
    CH04: "Invalid HTTP header name in request",
}

pub(crate) const TOKEN_EOB_ERROR_CODES: [ErrorCode; 2] = [
    ErrorCode::LI01,
    ErrorCode::LS01,
];

pub(crate) const STATEMENT_EOB_ERROR_CODES: [ErrorCode; 2] = [
    ErrorCode::PR05,
    ErrorCode::PM06,
];


/* -------------------- *
 *      NODE ERROR      *
 * -------------------- */
/// Represents an error associated with a node in the AST.
#[derive(Clone)]
pub struct NodeError {
    inner: Box<NodeErrorInner>,
}

#[derive(Debug, Clone)]
struct NodeErrorInner {
    code: ErrorCode,
    bounds: Option<Range<usize>>,
    description: Option<String>,
    filepath: String,
    seen: bool,
}

impl Debug for NodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl NodeError {
    /// Creates a new instance of [`NodeError`].
    #[inline]
    #[must_use]
    pub fn new(
        code: ErrorCode,
        bounds: Option<Range<usize>>,
        description: Option<String>,
        filepath: String,
    ) -> Self {
        Self {
            inner: Box::new(NodeErrorInner {
                code,
                bounds,
                description,
                filepath,
                seen: false,
            }),
        }
    }

    /// Gets the error code associated with this error.
    #[inline]
    #[must_use]
    pub fn code(&self) -> ErrorCode {
        self.inner.code.clone()
    }

    /// Gets the optional bounds of the error.
    #[inline]
    #[must_use]
    pub fn bounds(&self) -> Option<Range<usize>> {
        self.inner.bounds.clone()
    }

    /// Gets the optional description of the error.
    #[inline]
    #[must_use]
    pub fn description(&self) -> Option<&String> {
        self.inner.description.as_ref()
    }

    /// Gets the filepath where the error occurred.
    #[inline]
    #[must_use]
    pub fn filepath(&self) -> &str {
        &self.inner.filepath
    }

    /// Checks if the error has been marked as seen by a formatter.
    #[inline]
    #[must_use]
    pub fn seen(&self) -> bool {
        self.inner.seen
    }

    /// Marks the error as seen.
    ///
    /// Used by a formatter to distinguish errors.
    #[inline]
    pub fn set_seen(&mut self) {
        self.inner.seen = true;
    }
}


/* -------------------- *
 *   SCOPED NODE ERROR  *
 * -------------------- */
/// Represents an error associated with a node in the AST within a specific scope.
#[derive(Clone)]
pub struct ScopedNodeError<S: Clone> {
    inner: Box<ScopedNodeErrorInner<S>>,
}

#[derive(Debug, Clone)]
struct ScopedNodeErrorInner<S: Clone> {
    code: ErrorCode,
    scope: S,
    bounds: Option<Range<usize>>,
    description: Option<String>,
    filepath: String,
}

impl<S: Debug + Clone> Debug for ScopedNodeError<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<S: Clone> ScopedNodeError<S> {
    /// Creates a new instance of [`ScopedNodeError`].
    #[inline]
    #[must_use]
    pub fn new(
        code: ErrorCode,
        scope: S,
        bounds: Option<Range<usize>>,
        description: Option<String>,
        filepath: String,
    ) -> Self {
        Self {
            inner: Box::new(ScopedNodeErrorInner {
                code,
                scope,
                bounds,
                description,
                filepath,
            }),
        }
    }

    /// Gets the error code associated with this error.
    #[inline]
    #[must_use]
    pub fn code(&self) -> ErrorCode {
        self.inner.code.clone()
    }

    /// Gets the scope within which the error occurred.
    #[inline]
    #[must_use]
    pub fn scope(&self) -> S {
        self.inner.scope.clone()
    }

    /// Gets the optional bounds of the error in the scope.
    #[inline]
    #[must_use]
    pub fn bounds(&self) -> Option<Range<usize>> {
        self.inner.bounds.clone()
    }

    /// Gets the optional description of the error.
    #[inline]
    #[must_use]
    pub fn description(&self) -> Option<&String> {
        self.inner.description.as_ref()
    }

    /// Gets the filepath where the error occurred.
    #[inline]
    #[must_use]
    pub fn filepath(&self) -> &str {
        &self.inner.filepath
    }
}


/* -------------------- *
 *        TRAITS        *
 * -------------------- */
/// Provides a method to check for the presence of errors in the implementing object.
pub trait HasError {
    /// Checks whether an error is associated with the implementing object.
    #[must_use]
    fn has_error(&self) -> bool;

    /// Checks whether an unseen error is associated with the implementing object.
    #[must_use]
    fn has_unseen_error(&self) -> bool;

    /// Checks whether an end of buffer error is associated with the implementing object.
    #[must_use]
    fn has_eob_error(&self) -> bool;
}

impl<T: HasError> HasError for [T] {
    #[inline]
    fn has_error(&self) -> bool {
        self.iter().any(HasError::has_error)
    }

    #[inline]
    fn has_unseen_error(&self) -> bool {
        self.iter().any(HasError::has_unseen_error)
    }

    #[inline]
    fn has_eob_error(&self) -> bool {
        self.last().is_some_and(HasError::has_eob_error)
    }
}
