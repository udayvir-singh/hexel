//! todo
//!
//! # Interdependency graph
//!
//! ```text
//! lexer ───▶ parser ──┬──▶ collapser
//!                     │
//!                     ├──▶ linter
//!                     │
//!                     └──▶ highlighter
//! ```

pub mod collapser;
pub mod lexer;
pub mod parser;
