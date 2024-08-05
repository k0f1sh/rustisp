pub fn lex(input: &str) -> Vec<Token> {
    // inputを先頭から順に読み、
    // * ( が来たら LParen を
    // * ) が来たら RParen を
    // * [0-9]+(\.[0-9]+)? が来たら Num を
    // * [-a-zA-Z_]+ が来たら Symbol を
    // 生成し、読み進める。スペースや改行は無視する
    todo!("implement this")
}

#[test]
fn test_lex() {
    use Token::*;
    assert_eq!(lex("foo"), vec![symbol("foo")]);
    assert_eq!(lex("()()"), vec![LParen, RParen, LParen, RParen]);
    assert_eq!(lex("(foo 12 34)"), vec![LParen, symbol("foo"), Num(12), Num(34), RParen]);
    assert_eq!(lex("(foo (bar baz))", vec![LParen, symbol("foo"), LParen, symbol("bar"), symbol("baz"), RParen, RParen]));
}