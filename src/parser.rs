// 我々の目的: パースしたい
// パースしたいときまず考えるもの: 正規表現
// LIST = LPAREN EXP* RPAREN みたいなことを正規表現でやるが
// ここで問題にぶち当たる
// EXP ってどうパースすんのというと
// EXP = LIST | NUM | SYMBOL みたいなことをすると
// LISTがまた出てくると
// 再帰的に書けねえ困った
// => 正規表現の表現力を超えてる

use std::iter::Peekable;

use crate::sexp::Sexp;
use crate::token::Token;

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Sexp>, String> {
    let mut tokens = tokens.iter().peekable();
    let mut sexps = Vec::new();
    while tokens.peek().is_some() {
        sexps.push(parse_sexp(&mut tokens)?);
    }
    Ok(sexps)
}

fn parse_sexp<'a, I>(tokens: &mut Peekable<I>) -> Result<Sexp, String>
where
    I: Iterator<Item = &'a Token>,
{
    match tokens.peek() {
        Some(Token::LParen) => {
            tokens.next();
            let mut sexps = vec![];
            while let Some(token) = tokens.peek() {
                match token {
                    Token::RParen => {
                        tokens.next();
                        return Ok(Sexp::List(sexps));
                    }
                    _ => {
                        sexps.push(parse_sexp(tokens)?);
                    }
                }
            }
            Err("Expected RParen".to_string())
        }
        Some(Token::RParen) => Err("Unexpected RParen".to_string()),
        Some(Token::Num(n)) => {
            tokens.next();
            Ok(Sexp::Num(*n))
        }
        Some(Token::Symbol(s)) => {
            tokens.next();
            Ok(Sexp::Symbol(s.clone()))
        }
        Some(Token::True) => {
            tokens.next();
            Ok(Sexp::TRUE)
        }
        Some(Token::False) => {
            tokens.next();
            Ok(Sexp::FALSE)
        }
        None => Err("Unexpected EOF".to_string()),
    }
}

#[test]
fn test_parse() {
    fn p(s: &str) -> Result<Vec<Sexp>, String> {
        parse(crate::lexer::lex(s))
    }

    assert_eq!(p(""), Ok(vec![]));
    assert_eq!(p("(42)"), Ok(vec![slist![Sexp::Num(42.)]]));
    assert_eq!(
        p("(foo)"),
        Ok(vec![Sexp::List(vec![Sexp::Symbol("foo".to_string())])])
    );
    assert_eq!(
        p("((1)(2))"),
        Ok(vec![slist![slist![Sexp::Num(1.)], slist![Sexp::Num(2.)]]])
    );
    assert_eq!(p("42"), Ok(vec![Sexp::Num(42.)]));
    assert_eq!(p("foo"), Ok(vec![Sexp::Symbol("foo".to_string())]));
    assert_eq!(p("true"), Ok(vec![Sexp::Bool(true)]));
    assert_eq!(p("false"), Ok(vec![Sexp::Bool(false)]));
    assert_eq!(p("())"), Err("Unexpected RParen".to_string()));
    assert_eq!(p("(1 foo"), Err("Expected RParen".to_string()));
}
