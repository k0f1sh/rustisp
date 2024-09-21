use std::fmt;

use crate::env::Env;
use crate::sexp::Sexp;

pub type Value = Sexp<Native>;

#[derive(PartialEq, Debug, Clone)]
pub enum Native {
    EmbeddedFun(fn(args: Vec<Value>, env: &Env) -> Result<Value, String>),
    Closure(Vec<String>, Vec<Sexp>, Env),
    Macro(Vec<String>, Vec<Sexp>, Env),
}

impl TryFrom<Sexp> for Value {
    type Error = String;
    fn try_from(value: Sexp) -> Result<Self, Self::Error> {
        match value {
            Sexp::Symbol(s) => Ok(Sexp::Symbol(s)),
            Sexp::Num(n) => Ok(Sexp::Num(n)),
            Sexp::List(sexp) => Ok(Sexp::List(
                sexp.into_iter()
                    .map(Value::try_from)
                    .collect::<Result<_, _>>()?,
            )),
            Sexp::Bool(value) => Ok(Sexp::Bool(value)),
            _ => Err("cannot convert to Value".to_string()),
        }
    }
}

impl TryFrom<Value> for Sexp {
    type Error = String;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Sexp::Symbol(s) => Ok(Sexp::Symbol(s)),
            Sexp::Num(n) => Ok(Sexp::Num(n)),
            Sexp::List(sexp) => Ok(Sexp::List(
                sexp.into_iter()
                    .map(Sexp::try_from)
                    .collect::<Result<_, _>>()?,
            )),
            Sexp::Bool(value) => Ok(Sexp::Bool(value)),
            _ => Err("cannot convert to Sexp".to_string()),
        }
    }
}

// lambda syntax
// 固定長引数
// (lambda (x y) (+ x y))

pub mod embedded {
    use super::*;

    fn add(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        Ok(Sexp::Num(
            args.iter()
                .map(|arg| arg.extract_num())
                .sum::<Result<f64, _>>()?,
        ))
    }

    fn sub(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        match args.as_slice() {
            [] => Err("Expected at least one argument for -".to_string()),
            [arg] => Ok(Sexp::Num(-arg.extract_num()?)),
            [arg, argss @ ..] => {
                let n = arg.extract_num();
                Ok(Sexp::Num(
                    argss
                        .iter()
                        .fold(n, |acc, arg| Ok(acc? - arg.extract_num()?))?,
                ))
            }
        }
    }

    fn mul(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        Ok(Sexp::Num(
            args.iter()
                .map(|arg| arg.extract_num())
                .product::<Result<f64, _>>()?,
        ))
    }

    fn div(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        match args.as_slice() {
            [] => Err("Expected at least one argument for /".to_string()),
            [arg] => Ok(Sexp::Num(1. / arg.extract_num()?)),
            [arg, argss @ ..] => {
                let n = arg.extract_num();
                Ok(Sexp::Num(
                    argss
                        .iter()
                        .fold(n, |acc, arg| Ok(acc? / arg.extract_num()?))?,
                ))
            }
        }
    }

    fn eq(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        if let [a, b] = args.as_slice() {
            Ok(Sexp::Bool(a == b))
        } else {
            Err("Argument error: expected (= a b)".to_string())
        }
    }

    fn not_eq(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        if let [a, b] = args.as_slice() {
            Ok(Sexp::Bool(a != b))
        } else {
            Err("Argument error: expected (not= a b)".to_string())
        }
    }

    fn lt(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        if let [a, b] = args.as_slice() {
            let a = a.extract_num()?;
            let b = b.extract_num()?;
            Ok(Sexp::Bool(a < b))
        } else {
            Err("Argument error: expectejkkkkkukjkjkd (< a b)".to_string())
        }
    }

    fn gt(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        if let [a, b] = args.as_slice() {
            let a = a.extract_num()?;
            let b = b.extract_num()?;
            Ok(Sexp::Bool(a > b))
        } else {
            Err("Argument error: expected (> a b)".to_string())
        }
    }

    fn println(args: Vec<Value>, _env: &Env) -> Result<Value, String> {
        for arg in args {
            println!("{}", arg);
        }
        Ok(Sexp::NIL)
    }

    pub fn install(env: &Env) {
        env.set("+", Sexp::Pure(Native::EmbeddedFun(add)));
        env.set("-", Sexp::Pure(Native::EmbeddedFun(sub)));
        env.set("*", Sexp::Pure(Native::EmbeddedFun(mul)));
        env.set("/", Sexp::Pure(Native::EmbeddedFun(div)));
        env.set("=", Sexp::Pure(Native::EmbeddedFun(eq)));
        env.set("not=", Sexp::Pure(Native::EmbeddedFun(not_eq)));
        env.set("<", Sexp::Pure(Native::EmbeddedFun(lt)));
        env.set(">", Sexp::Pure(Native::EmbeddedFun(gt)));
        env.set("println", Sexp::Pure(Native::EmbeddedFun(println)));
    }
}

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Native::EmbeddedFun(_) => write!(f, "<embedded-fun>"),
            Native::Closure(_, _, _) => write!(f, "<closure>"),
            Native::Macro(_, _, _) => write!(f, "<macro>"),
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
                "begin" => evaluate_sequence(args, env),
                "lbegin" => {
                    let env = Env::new(Some(env.clone()));
                    evaluate_sequence(args, &env)
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
                "lambda" => {
                    if let [Sexp::List(params), body @ ..] = args {
                        let params = params
                            .into_iter()
                            .map(|p| match p {
                                Sexp::Symbol(s) => Ok(s.clone()),
                                _ => Err("Syntax error: expected symbol".to_string()),
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        let body = body.to_vec();
                        Ok(Sexp::Pure(Native::Closure(params, body, env.clone())))
                    } else {
                        Err("Syntax error: expected (lambda (params) body)".to_string())
                    }
                }
                "quote" => {
                    if let [sexp] = args {
                        Ok(sexp.clone().try_into()?)
                    } else {
                        Err("Syntax error: expected (quote sexp)".to_string())
                    }
                }
                "quasiquote" => {
                    if let [sexp] = args {
                        evaluate_quasiquote(sexp, env).map(|v| v.into())
                    } else {
                        Err("Syntax error: expected (quasiquote sexp)".to_string())
                    }
                }
                "unquote" => Err("unquote should only appear inside quasiquote".to_string()),
                "unquote-splicing" => {
                    Err("unquote should only appear inside quasiquote".to_string())
                }
                "defmacro" => {
                    if let [Sexp::Symbol(name), Sexp::List(params), body @ ..] = args {
                        let params = params
                            .into_iter()
                            .map(|p| match p {
                                Sexp::Symbol(s) => Ok(s.clone()),
                                _ => Err("Syntax error: expected symbol".to_string()),
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        let body = body.to_vec();
                        let value = Sexp::Pure(Native::Macro(params, body, env.clone()));
                        env.set(name, value);
                        Ok(Sexp::NIL)
                    } else {
                        Err("Syntax error: expected (defmacro name (params) body)".to_string())
                    }
                }
                _ => evaluate_call(ff, args, env),
            },
            [f, args @ ..] => evaluate_call(f, args, env),
        },
        Sexp::Pure(a) => a.absurd(),
    }
}

fn macroexpand(
    arg_ss: &[Sexp],
    params: Vec<String>,
    body: Vec<Sexp>,
    env: Env,
) -> Result<Value, String> {
    if params.len() != arg_ss.len() {
        return Err(format!(
            "Argument error: expected {} arguments, but got {}",
            params.len(),
            arg_ss.len()
        ));
    }
    let env = Env::new(Some(env));
    for (param, arg) in params.iter().zip(arg_ss) {
        let value = arg.clone().try_into()?;
        env.set(param, value);
    }
    evaluate_sequence(&body, &env)
}

fn evaluate_call(f: &Sexp, arg_ss: &[Sexp], env: &Env) -> Result<Value, String> {
    let f = evaluate(f, env)?;

    // macro
    if let Sexp::Pure(Native::Macro(params, body, env)) = f {
        let expanded = macroexpand(arg_ss, params, body, env.clone())?;
        return evaluate(&expanded.try_into()?, &env);
    }

    let args = arg_ss
        .iter()
        .map(|arg| evaluate(arg, env))
        .collect::<Result<Vec<_>, _>>()?;

    match f {
        Sexp::Pure(Native::EmbeddedFun(f)) => f(args, env),
        Sexp::Pure(Native::Closure(params, body, env)) => {
            if params.len() != args.len() {
                return Err(format!(
                    "Argument error: expected {} arguments, but got {}",
                    params.len(),
                    args.len()
                ));
            }
            let env = Env::new(Some(env));
            for (param, arg) in params.iter().zip(args) {
                env.set(param, arg);
            }
            evaluate_sequence(&body, &env)
        }
        _ => Err("cannot call".to_string()),
    }
}

enum QuasiquoteValue {
    Value(Value),
    SplicingValue(Value),
}

impl Into<Value> for QuasiquoteValue {
    fn into(self) -> Value {
        match self {
            QuasiquoteValue::Value(v) => v,
            QuasiquoteValue::SplicingValue(v) => v,
        }
    }
}

fn evaluate_quasiquote(s: &Sexp, env: &Env) -> Result<QuasiquoteValue, String> {
    match s {
        Sexp::List(list) => {
            if let [Sexp::Symbol(f), sexp] = list.as_slice() {
                match f.as_str() {
                    "unquote" => evaluate(sexp, env).map(QuasiquoteValue::Value),
                    "unquote-splicing" => {
                        let evaled = evaluate(sexp, env)?;
                        if let Sexp::List(list) = evaled {
                            Ok(QuasiquoteValue::SplicingValue(Sexp::List(list)))
                        } else {
                            Err("unquote-splicing should return a list".to_string())
                        }
                    }
                    _ => {
                        let qs = list
                            .iter()
                            .map(|s| evaluate_quasiquote(s, env))
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(QuasiquoteValue::Value(expand_quasiquote_value(qs)?))
                    }
                }
            } else {
                let qs = list
                    .iter()
                    .map(|s| evaluate_quasiquote(s, env))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(QuasiquoteValue::Value(expand_quasiquote_value(qs)?))
            }
        }
        _ => Ok(QuasiquoteValue::Value(s.clone().try_into()?)),
    }
}

fn expand_quasiquote_value(qs: Vec<QuasiquoteValue>) -> Result<Value, String> {
    let mut result = vec![];
    for quasiquote_value in qs {
        match quasiquote_value {
            QuasiquoteValue::Value(v) => result.push(v),
            QuasiquoteValue::SplicingValue(v) => {
                if let Sexp::List(list) = v {
                    result.extend(list);
                } else {
                    return Err("unquote-splicing should return a list".to_string());
                }
            }
        }
    }
    Ok(Sexp::List(result))
}

fn evaluate_sequence(ss: &[Sexp], env: &Env) -> Result<Value, String> {
    let mut result = Sexp::NIL;
    for s in ss {
        result = evaluate(s, env)?;
    }
    Ok(result)
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
    assert_eq!(e("((if true + -) 3 2)"), Ok(Sexp::Num(5.)));
    assert_eq!(e("((if false + -) 3 2)"), Ok(Sexp::Num(1.)));
    assert_eq!(
        e("(quote (1 2 3))"),
        Ok(Sexp::List(vec![
            Sexp::Num(1.),
            Sexp::Num(2.),
            Sexp::Num(3.)
        ]))
    );
    assert_eq!(
        e("(quasiquote (1 2 3))"),
        Ok(Sexp::List(vec![
            Sexp::Num(1.),
            Sexp::Num(2.),
            Sexp::Num(3.)
        ]))
    );
    assert_eq!(
        e("(quasiquote (1 (unquote (+ 1 1)) 3))"),
        Ok(Sexp::List(vec![
            Sexp::Num(1.),
            Sexp::Num(2.),
            Sexp::Num(3.)
        ]))
    );
    assert_eq!(
        e("(quasiquote (1 (unquote-splicing (quote (2 3))) 4))"),
        Ok(Sexp::List(vec![
            Sexp::Num(1.),
            Sexp::Num(2.),
            Sexp::Num(3.),
            Sexp::Num(4.)
        ]))
    );
}
