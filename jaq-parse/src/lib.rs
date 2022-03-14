mod lex;
mod ops;
pub mod parse;

pub use lex::{lex, Token};
pub use ops::{MathOp, OrdOp};

pub type Span = std::ops::Range<usize>;
