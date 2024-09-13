use crate::nothing::Nothing;
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Sexp<T = Nothing> {
    Symbol(String),
    Num(f64),
    List(Vec<Sexp<T>>),
    Bool(bool),
    Pure(T), // T の値がない => Sexp::Pure(_) => 存在しえない
}

// 今
// (begin (begin (set foo 123) foo) foo)
// -> 環境=グローバルなハッシュテーブルがある
// -> 今後はlbeginを考えると、そうじゃなくなる
//

// (begin (set foo 123) foo)              => 123
// (begin (begin (set foo 123) 456) foo)  => 123
// (begin (lbegin (set foo 123) 456) foo) => error: undefined variable: `foo`
// (begin (set foo 123) (lbegin (set foo 456)) foo) => 456

macro_rules! slist {
    ($( $e:expr ),*) => {
        Sexp::List(vec![$( $e ),*])
    };
}

impl<T> Sexp<T> {
    pub const RESERVED_KEYWORDS: &[&str] = &["true", "false"];

    pub fn symbol(s: impl Into<String>) -> Self {
        let s = s.into();
        if Self::RESERVED_KEYWORDS
            .iter()
            .any(|keyword| *keyword == s.as_str())
        {
            panic!("reserved keyword: {}", s);
        }
        Sexp::Symbol(s.into())
    }

    pub const TRUE: Self = Sexp::Bool(true);
    pub const FALSE: Self = Sexp::Bool(false);
    pub const NIL: Self = Sexp::List(Vec::new());

    pub fn map<S>(self, f: impl Fn(T) -> S) -> Sexp<S> {
        match self {
            Sexp::Symbol(s) => Sexp::Symbol(s),
            Sexp::Num(n) => Sexp::Num(n),
            Sexp::List(sexp) => Sexp::List(sexp.into_iter().map(|s| s.map(|v| f(v))).collect()),
            Sexp::Bool(value) => Sexp::Bool(value),
            Sexp::Pure(value) => Sexp::Pure(f(value)),
        }
    }
}

impl<T: fmt::Display> Sexp<T> {
    pub fn extract_num(&self) -> Result<f64, String> {
        match self {
            Sexp::Num(n) => Ok(*n),
            _ => Err(format!("expected number, but got {}", self)),
        }
    }

    pub fn extract_bool(&self) -> Result<bool, String> {
        match self {
            Sexp::Bool(value) => Ok(*value),
            _ => Err(format!("expected bool, but got {}", self)),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Sexp<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Sexp::Symbol(s) => write!(f, "{}", s),
            Sexp::Num(n) => write!(f, "{}", n),
            Sexp::List(sexp) => {
                write!(f, "(")?;
                for (i, s) in sexp.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", s)?;
                }
                write!(f, ")")
            }
            Sexp::Bool(value) => write!(f, "{}", value),
            Sexp::Pure(value) => write!(f, "{}", value),
        }
    }
}

#[test]
fn test_display() {
    assert_eq!(
        Sexp::<Nothing>::symbol("foo-bar").to_string(),
        "foo-bar".to_string()
    );
    assert_eq!(Sexp::<Nothing>::Num(123.).to_string(), "123".to_string());
    assert_eq!(Sexp::<Nothing>::Bool(true).to_string(), "true".to_string());
    assert_eq!(
        Sexp::<Nothing>::Bool(false).to_string(),
        "false".to_string()
    );
    assert_eq!(
        slist![Sexp::<Nothing>::symbol("foo-bar")].to_string(),
        "(foo-bar)"
    );
    assert_eq!(
        slist![
            Sexp::<Nothing>::symbol("foo"),
            slist![
                Sexp::<Nothing>::symbol("bar"),
                Sexp::<Nothing>::symbol("baz")
            ]
        ]
        .to_string(),
        "(foo (bar baz))"
    );
}
