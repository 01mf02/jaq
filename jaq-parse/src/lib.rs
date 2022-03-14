mod lex;
pub mod parse;

pub use lex::{lex, Token};

pub type Span = std::ops::Range<usize>;
