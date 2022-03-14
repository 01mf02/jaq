use chumsky::{prelude::*, stream::Stream};
use jaq_parse::{lex, parse};
use std::{env, fs};

fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    let (tokens, lex_errs) = lex().parse_recovery(src.as_str());

    let parse_errs = if let Some(tokens) = tokens {
        // println!("Tokens = {:?}", tokens);
        let len = src.chars().count();
        let (ast, parse_errs) =
            parse::parse_defs().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        println!("{:#?}", ast);
        /*
        if let Some(funcs) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
            if let Some(main) = funcs.get("main") {
                assert_eq!(main.args.len(), 0);
                match eval_expr(&main.body, &funcs, &mut Vec::new()) {
                    Ok(val) => println!("Return value: {}", val),
                    Err(e) => errs.push(Simple::custom(e.span, e.msg)),
                }
            } else {
                panic!("No main function!");
            }
        }
        */

        parse_errs
    } else {
        Vec::new()
    };

    let lex_errs = lex_errs.into_iter().map(|e| e.map(|c| c.to_string()));
    let parse_errs = parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string()));
    lex_errs.chain(parse_errs).for_each(|e| println!("{:?}", e));
}
