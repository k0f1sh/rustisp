use std::io::Write;

mod nothing;

#[macro_use]
mod sexp;

mod lexer;
mod token;

mod parser;

mod eval;

fn main() {
    println!("I'm Rustisp. Your code is BAD");

    // repl
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();
        if input.is_empty() {
            continue;
        }
        let tokens = lexer::lex(input);
        let sexps = parser::parse(tokens).unwrap();
        for sexp in sexps {
            let result = eval::evaluate(&sexp).unwrap();
            println!("{}", result);
        }
    }
}
