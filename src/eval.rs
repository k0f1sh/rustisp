
use crate::sexp::Sexp;

// (+ 1 2) => 3
// (concat "foo" "bar") => "foobar"
// (open "javascript.java") => FileHandle <- S式じゃねーよハゲ
// S式の中に値を持てるようにするっていう表現方法

// List<T> T型モテる
// Sexp<T> 

// + - * /
// (define foo 123)
// (+ foo bar)
// (<fun> 123 456)

pub fn evaluate(s: &Sexp) -> Result<Sexp, String> {
    match s {
        Sexp::Symbol(_symbol) => Err("not implemented".to_string()),
        Sexp::Num(num) => Ok(Sexp::Num(*num)),
        Sexp::List(list) => match list.as_slice() {
            [] => Ok(Sexp::NIL),
            [Sexp::Symbol(f), args @ ..] => {
                match f.as_str() {
                    "+" => Ok(Sexp::Num(args.iter().map(|arg| evaluate(arg)?.to_num()).sum::<Result<f64, _>>()?)),
                    "-" => match args {
                        [] => Err("Expected at least one argument for -".to_string()),
                        [arg] => Ok(Sexp::Num(- evaluate(arg)?.to_num()?)),
                        [arg, argss @ ..] => {
                            let n = evaluate(arg)?.to_num();
                            Ok(Sexp::Num(argss.iter().fold(n, |acc, arg| Ok(acc? - evaluate(arg)?.to_num()?))?))
                        }
                   }
                    "*" => Ok(Sexp::Num(args.iter().map(|arg| evaluate(arg)?.to_num()).product::<Result<f64, _>>()?)),
                    "/" => match args {
                       [] => Err("Expected at least one argument for /".to_string()),
                       [arg] => Ok(Sexp::Num(1. / evaluate(arg)?.to_num()?)),
                       [arg, argss @ ..] => {
                           let n = evaluate(arg)?.to_num();
                           Ok(Sexp::Num(argss.iter().fold(n, |acc, arg| Ok(acc? / evaluate(arg)?.to_num()?))?))
                       }
                    }
                    _ => panic!("Unknown function: {}", f),
                }
            }
            [f, ..] => panic!("Cannot call: {}", f),
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
        evaluate(&s[0])
    }

    assert_eq!(e("()"), Ok(Sexp::NIL));
    assert_eq!(e("123"), Ok(Sexp::Num(123.)));
    // assert_eq!(e("foo"), Ok(Sexp::Num(123.)));
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
}
