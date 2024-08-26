
use crate::sexp::Sexp;
use crate::env::Env;

// (set name value)
// (begin a b c)
// name (参照)

pub fn evaluate(s: &Sexp, env: &mut Env) -> Result<Sexp, String> {
    match s {
        Sexp::Symbol(name) => {
            env.find(name).ok_or_else(|| format!("Variable not found: {}", name))
        },
        Sexp::Num(num) => Ok(Sexp::Num(*num)),
        Sexp::List(list) => match list.as_slice() {
            [] => Ok(Sexp::NIL),
            [Sexp::Symbol(f), args @ ..] => {
                match f.as_str() {
                    "+" => Ok(Sexp::Num(args.iter().map(|arg| evaluate(arg, env)?.to_num()).sum::<Result<f64, _>>()?)),
                    "-" => match args {
                        [] => Err("Expected at least one argument for -".to_string()),
                        [arg] => Ok(Sexp::Num(- evaluate(arg, env)?.to_num()?)),
                        [arg, argss @ ..] => {
                            let n = evaluate(arg, env)?.to_num();
                            Ok(Sexp::Num(argss.iter().fold(n, |acc, arg| Ok(acc? - evaluate(arg, env)?.to_num()?))?))
                        }
                   }
                    "*" => Ok(Sexp::Num(args.iter().map(|arg| evaluate(arg, env)?.to_num()).product::<Result<f64, _>>()?)),
                    "/" => match args {
                       [] => Err("Expected at least one argument for /".to_string()),
                       [arg] => Ok(Sexp::Num(1. / evaluate(arg, env)?.to_num()?)),
                       [arg, argss @ ..] => {
                           let n = evaluate(arg, env)?.to_num();
                           Ok(Sexp::Num(argss.iter().fold(n, |acc, arg| Ok(acc? / evaluate(arg, env)?.to_num()?))?))
                       }
                    }
                    "begin" => {
                        let mut result = Sexp::NIL;
                        for arg in args {
                            result = evaluate(arg, env)?;
                        }
                        Ok(result)
                    }
                    "if" => {
                        if let [cond, then, else_] = args {
                            if evaluate(cond, env)?.to_num()? != 0. { // Boolがないので、0以外をtrueとする
                                evaluate(then, env)
                            } else {
                                evaluate(else_, env)
                            }
                        } else {
                            Err("Expected three arguments for if".to_string())
                        }
                    }
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
                            Ok(Sexp::Num(env.find(s).map(|_| 1.).unwrap_or(0.)))
                        } else {
                            Err("Syntax error: expected (defined? symbol)".to_string())  
                        }
                    }
                    _ => Err(format!("Unkown function: {}", f)),
                }
            }
            [f, ..] => Err(format!("Cannot call: {}", f)),
        }
        Sexp::Pure(a) => a.absurd(),
    }
}


#[test]
fn test_evaluate() {
    fn e(s: &str) -> Result<Sexp, String> {
        let s = crate::parser::parse(crate::lexer::lex(s))?;
        if s.len() != 1 {
            return Err("Expected one S-expression".to_string());
        }
        evaluate(&s[0], &mut Env::new())
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
    assert_eq!(e("(begin (set foo 123) (set foo 456) foo)"), Ok(Sexp::Num(456.)));
    assert_eq!(e("(begin (set foo 123) (set bar 456) (+ foo bar))"), Ok(Sexp::Num(579.)));
    assert_eq!(e("(begin (set foo 12) (set foo 34) (set bar 56) (* foo bar))"), Ok(Sexp::Num(1904.)));
    assert_eq!(e("(if 0 1 2)"), Ok(Sexp::Num(2.)));
    assert_eq!(e("(if 3 1 2)"), Ok(Sexp::Num(1.)));
    assert_eq!(e("(if (- 1 1) (+ 2 3) (+ 4 5))"), Ok(Sexp::Num(9.)));
    assert_eq!(e("(defined? foo)"), Ok(Sexp::Num(0.)));
    assert_eq!(e("(begin (set foo 123) (defined? foo))"), Ok(Sexp::Num(1.)));
}
