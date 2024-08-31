use std::io::Write;

mod nothing;

#[macro_use]
mod sexp;

mod lexer;
mod token;

mod parser;

mod env;

mod eval;

fn main() {
    println!("I'm Rustisp. Your code is SO-SO");

    let mut env = env::Env::new(None);

    // repl
    loop {
        let input = read();
        match eval(&input, &env) {
            Ok(value) => println!("{}", value),
            Err(err) => eprintln!("Error: {}", err),
        }
    }
}

fn read() -> String {
    print!("> ");
    std::io::stdout().flush().unwrap();
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input.trim().to_owned()
}

fn eval(s: &str, env: &env::Env) -> Result<sexp::Sexp, String> {
    let tokens = lexer::lex(s);
    let sexps = parser::parse(tokens)?;
    let mut result = sexp::Sexp::NIL;
    for sexp in sexps {
        result = eval::evaluate(&sexp, env)?;
    }
    Ok(result)
}
