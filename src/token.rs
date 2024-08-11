use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Token {
    Symbol(String), // foo-bar
    Num(f64),       // 123.4
    LParen,         // (
    RParen,         // )
}

impl Token {
    pub fn symbol(s: impl Into<String>) -> Self {
        Token::Symbol(s.into())
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Symbol(s) => write!(f, "{}", s),
            Token::Num(n) => write!(f, "{}", n),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
        }
    }
}

#[test]
fn test_display() {
    assert_eq!(Token::symbol("foo").to_string(), "foo".to_string());
    assert_eq!(Token::Num(123.).to_string(), "123".to_string());
    assert_eq!(Token::LParen.to_string(), "(");
    assert_eq!(Token::RParen.to_string(), ")");
}
