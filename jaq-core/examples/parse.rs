//! Parse a filter from standard input and output its AST.

use jaq_core::load::{lex::Lexer, parse::Parser};

fn main() {
    let s = std::io::read_to_string(std::io::stdin()).unwrap();
    let tokens = Lexer::new(&s).lex().unwrap();
    let tm = Parser::new(&tokens).parse(Parser::term).unwrap();
    println!("{tm:?}");
}
