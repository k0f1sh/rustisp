use std::fmt;

use embedded::install;

use crate::env::Env;
use crate::eval;
use crate::sexp::Sexp;

pub type Value = Sexp<Native>;

#[derive(PartialEq, Debug, Clone)]
pub enum Native {
    EmbeddedFun(fn(args: Vec<Value>, env: &Env) -> Result<Value, String>),
    // Lambda(Vec<String>, Vec<Value>),
}

pub mod embedded {
    use super::*;
    fn add(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        Ok(Sexp::Num(
            args.iter()
                .map(|arg| arg.extract_num())
                .sum::<Result<f64, _>>()?,
        ))
    }

    pub fn install(env: &Env) {
        env.set("+", Sexp::Pure(Native::EmbeddedFun(add)));
    }
}

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Native::EmbeddedFun(_) => write!(f, "<embedded-fun>"),
        }
    }
}

// list = (+ 1 2 3)
// list.as_slice() => [Symbol(f), args @ ..]

// (set hoge +)
// (hoge 12 34)

pub fn evaluate(s: &Sexp, env: &Env) -> Result<Value, String> {
    match s {
        Sexp::Symbol(name) => env
            .find(name)
            .ok_or_else(|| format!("Variable not found: {}", name)),
        Sexp::Num(num) => Ok(Sexp::Num(*num)),
        Sexp::Bool(value) => Ok(Sexp::Bool(*value)),
        Sexp::List(list) => match list.as_slice() {
            [] => Ok(Sexp::NIL),
            [ff @ Sexp::Symbol(f), args @ ..] => match f.as_str() {
                "-" => match args {
                    [] => Err("Expected at least one argument for -".to_string()),
                    [arg] => Ok(Sexp::Num(-evaluate(arg, env)?.extract_num()?)),
                    [arg, argss @ ..] => {
                        let n = evaluate(arg, env)?.extract_num();
                        Ok(Sexp::Num(argss.iter().fold(n, |acc, arg| {
                            Ok(acc? - evaluate(arg, env)?.extract_num()?)
                        })?))
                    }
                },
                "*" => Ok(Sexp::Num(
                    args.iter()
                        .map(|arg| evaluate(arg, env)?.extract_num())
                        .product::<Result<f64, _>>()?,
                )),
                "/" => match args {
                    [] => Err("Expected at least one argument for /".to_string()),
                    [arg] => Ok(Sexp::Num(1. / evaluate(arg, env)?.extract_num()?)),
                    [arg, argss @ ..] => {
                        let n = evaluate(arg, env)?.extract_num();
                        Ok(Sexp::Num(argss.iter().fold(n, |acc, arg| {
                            Ok(acc? / evaluate(arg, env)?.extract_num()?)
                        })?))
                    }
                },
                "begin" => {
                    let mut result = Sexp::NIL;
                    for arg in args {
                        result = evaluate(arg, env)?;
                    }
                    Ok(result)
                }
                "lbegin" => {
                    let mut result = Sexp::NIL;
                    let env = Env::new(Some(env.clone()));
                    for arg in args {
                        result = evaluate(arg, &env)?;
                    }
                    Ok(result)
                }
                "if" => {
                    if let [cond, then, else_] = args {
                        if evaluate(cond, env)?.extract_bool()? {
                            evaluate(then, env)
                        } else {
                            evaluate(else_, env)
                        }
                    } else {
                        Err("Expected three arguments for if".to_string())
                    }
                }
                "while" => match args {
                    [cond, body @ ..] => {
                        while evaluate(cond, env)?.extract_bool()? {
                            for expr in body {
                                evaluate(expr, env)?;
                            }
                        }
                        Ok(Sexp::NIL)
                    }
                    _ => Err("Expected more than one arguments for while".to_string()),
                },
                "set" => {
                    if let [Sexp::Symbol(name), value] = args {
                        let value = evaluate(value, env)?;
                        env.set(name, value);
                        Ok(Sexp::NIL)
                    } else {
                        Err("Expected two arguments for set".to_string())
                    }
                }
                "defined?" => {
                    if let [Sexp::Symbol(s)] = args {
                        Ok(Sexp::Bool(env.find(s).is_some()))
                    } else {
                        Err("Syntax error: expected (defined? symbol)".to_string())
                    }
                }
                "=" => {
                    if let [a, b] = args {
                        let a = evaluate(a, env)?;
                        let b = evaluate(b, env)?;
                        Ok(Sexp::Bool(a == b))
                    } else {
                        Err("Syntax error: expected (= a b)".to_string())
                    }
                }
                "not=" => {
                    if let [a, b] = args {
                        let a = evaluate(a, env)?;
                        let b = evaluate(b, env)?;
                        Ok(Sexp::Bool(a != b))
                    } else {
                        Err("Syntax error: expected (not= a b)".to_string())
                    }
                }
                "println" => {
                    for arg in args {
                        println!("{}", evaluate(arg, env)?);
                    }
                    Ok(Sexp::NIL)
                }
                _ => evaluate_call(ff, args, env),
            },
            [f, args @ ..] => evaluate_call(f, args, env),
        },
        Sexp::Pure(a) => a.absurd(),
    }
}

fn evaluate_call(f: &Sexp, arg_ss: &[Sexp], env: &Env) -> Result<Value, String> {
    let f = evaluate(f, env)?;
    let args = arg_ss
        .iter()
        .map(|arg| evaluate(arg, env))
        .collect::<Result<Vec<_>, _>>()?;

    match f {
        Sexp::Pure(Native::EmbeddedFun(f)) => f(args, env),
        _ => Err("cannot call".to_string()),
    }
}

#[test]
fn test_evaluate() {
    fn e(s: &str) -> Result<Value, String> {
        let s = crate::parser::parse(crate::lexer::lex(s))?;
        if s.len() != 1 {
            return Err("Expected one S-expression".to_string());
        }
        let env = Env::new(None);
        embedded::install(&env);
        evaluate(&s[0], &env)
    }

    assert_eq!(e("()"), Ok(Sexp::NIL));
    assert_eq!(e("123"), Ok(Sexp::Num(123.)));
    assert_eq!(e("(+)"), Ok(Sexp::Num(0.)));
    assert_eq!(e("(+ 1)"), Ok(Sexp::Num(1.)));
    assert_eq!(e("(+ 1 2)"), Ok(Sexp::Num(3.)));
    assert_eq!(e("(+ 1 2 3)"), Ok(Sexp::Num(6.)));
    assert_eq!(e("(- 123)"), Ok(Sexp::Num(-123.)));
    assert_eq!(e("(- 123 23)"), Ok(Sexp::Num(100.)));
    assert_eq!(e("(- 123 23 40)"), Ok(Sexp::Num(60.)));
    assert_eq!(e("(*)"), Ok(Sexp::Num(1.)));
    assert_eq!(e("(* 12)"), Ok(Sexp::Num(12.)));
    assert_eq!(e("(* 12 3)"), Ok(Sexp::Num(36.)));
    assert_eq!(e("(* 12 3 2)"), Ok(Sexp::Num(72.)));
    assert_eq!(e("(/ 2)"), Ok(Sexp::Num(0.5)));
    assert_eq!(e("(/ 6 2)"), Ok(Sexp::Num(3.)));
    assert_eq!(e("(/ 6 2 3)"), Ok(Sexp::Num(1.)));
    assert_eq!(e("(* (+ 1 2) (+ 3 4))"), Ok(Sexp::Num(21.)));
    assert!(e("foo").is_err());
    assert!(e("(123)").is_err());
    assert_eq!(e("(begin)"), Ok(Sexp::NIL));
    assert_eq!(e("(begin (set foo 123) foo)"), Ok(Sexp::Num(123.)));
    assert_eq!(
        e("(begin (set foo 123) (set foo 456) foo)"),
        Ok(Sexp::Num(456.))
    );
    assert_eq!(
        e("(begin (set foo 123) (set bar 456) (+ foo bar))"),
        Ok(Sexp::Num(579.))
    );
    assert_eq!(
        e("(begin (set foo 12) (set foo 34) (set bar 56) (* foo bar))"),
        Ok(Sexp::Num(1904.))
    );
    assert_eq!(e("(if false 1 2)"), Ok(Sexp::Num(2.)));
    assert_eq!(e("(if true 1 2)"), Ok(Sexp::Num(1.)));
    assert_eq!(e("(if (not= 1 1) (+ 2 3) (+ 4 5))"), Ok(Sexp::Num(9.)));
    assert_eq!(e("(defined? foo)"), Ok(Sexp::FALSE));
    assert_eq!(e("(lbegin)"), Ok(Sexp::NIL));
    assert_eq!(e("(lbegin (set foo 123) foo)"), Ok(Sexp::Num(123.)));
    assert_eq!(
        e("(lbegin (set foo 123) (set foo 456) foo)"),
        Ok(Sexp::Num(456.))
    );
    assert_eq!(
        e("(lbegin (set foo 123) (set bar 456) (+ foo bar))"),
        Ok(Sexp::Num(579.))
    );
    assert_eq!(
        e("(lbegin (set foo 12) (set foo 34) (set bar 56) (* foo bar))"),
        Ok(Sexp::Num(1904.))
    );
    assert_eq!(
        e("(begin (set foo 123) (lbegin (set foo 456) foo))"),
        Ok(Sexp::Num(456.))
    );
    assert_eq!(
        e("(begin (set foo 123) (lbegin (set foo 456)) foo)"),
        Ok(Sexp::Num(456.))
    );
    assert!(e("(begin (lbegin (set foo 123)) foo)").is_err());
    assert_eq!(e("(= 1 1)"), Ok(Sexp::TRUE));
    assert_eq!(e("(= 1 2)"), Ok(Sexp::FALSE));
    assert_eq!(e("(not= 1 1)"), Ok(Sexp::FALSE));
    assert_eq!(e("(not= 1 2)"), Ok(Sexp::TRUE));
    assert_eq!(
        e("(begin (set a 0) (while (not= a 100) (begin (set a (+ a 1))))a)"),
        Ok(Sexp::Num(100.))
    );
}
