//! todo
//!
//! # Interdependency graph
//!
//! ```text
//! error ──────────┬─────────┬─────────┐
//!                 ▼         ▼         │
//! position ───▶ token ───▶ ast ───┐   │
//!     │           │         ▲     │   │
//!     ├─────────────────────┘     │   │
//!     │           │               │   │
//!     │     ┌─────┘               │   │
//!     │     ▼                     │   │
//!     ├──▶ fmt ◀──────────────────┘   │
//!     │     ▲                         │
//!     │     └─────────────────────────┤
//!     │                               │
//!     ├──▶ state ◀────────────────────┘
//!     │      ▲
//!     │      └─────────┐
//!     │                │
//!     └──▶ highlight   │
//!                      │
//! consts ──────────────┤
//!                      │
//! num ─────────────────┘
//!
//! http
//! ```

pub mod ast;
pub mod consts;
pub mod error;
// pub mod fmt;
// pub mod highlight;
// pub mod http;
pub mod num;
pub mod position;
pub mod state;
pub mod token;
