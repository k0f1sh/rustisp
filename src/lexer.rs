use crate::token::Token;

// TODO Result型返すようにしたい
pub fn lex(input: &str) -> Vec<Token> {
    // inputを先頭から順に読み、
    // * ( が来たら LParen を
    // * ) が来たら RParen を
    // * [0-9]+(\.[0-9]+)? が来たら Num を
    // * [-a-zA-Z_]+ が来たら Symbol を
    // 生成し、読み進める。スペースや改行は無視する
    let mut tokens = Vec::<Token>::new();
    let mut it = input.chars().peekable();

    loop {
        tokens.push(match it.next() {
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some(head @ ('a'..='z' | 'A'..='Z' | '-' | '_' | '+' | '*' | '/' | '?' | '=')) => {
                let mut tmp = head.to_string();
                while it
                    .peek()
                    .is_some_and(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' | '+' | '*' | '/' | '?' | '='))
                {
                    tmp.push(it.next().unwrap());
                }
                Token::symbol(tmp)
            }
            Some(head @ ('0'..='9')) => {
                let mut tmp = head.to_string();
                while it.peek().is_some_and(|c| matches!(c, '0'..='9')) {
                    tmp.push(it.next().unwrap());
                }
                Token::Num(tmp.parse().unwrap())
            }
            Some(c) if c.is_whitespace() => continue,
            Some(c) => panic!("unexpected character: {}", c),
            None => return tokens,
        })
    }
}

#[test]
fn test_lex() {
    use crate::token::Token;
    assert_eq!(lex("foo"), vec![Token::Symbol("foo".to_string())]);
    assert_eq!(
        lex("()()"),
        vec![Token::LParen, Token::RParen, Token::LParen, Token::RParen]
    );
    assert_eq!(
        lex("(foo 12 34)"),
        vec![
            Token::LParen,
            Token::Symbol("foo".to_string()),
            Token::Num(12.),
            Token::Num(34.),
            Token::RParen
        ]
    );
    assert_eq!(
        lex("(foo (bar baz))"),
        vec![
            Token::LParen,
            Token::Symbol("foo".to_string()),
            Token::LParen,
            Token::Symbol("bar".to_string()),
            Token::Symbol("baz".to_string()),
            Token::RParen,
            Token::RParen
        ]
    );
}
