use std::fmt;
use crate::nothing::Nothing;

#[derive(PartialEq, Debug, Clone)]
pub enum Sexp<T = Nothing> {
    Symbol(String),
    Num(f64),
    List(Vec<Sexp<T>>),
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
    pub fn symbol(s: impl Into<String>) -> Self {
        Sexp::Symbol(s.into())
    }

    pub const NIL: Self = Sexp::List(Vec::new());
    
    pub fn map<S>(self, f: impl Fn(T) -> S) -> Sexp<S> {
        match self {
            Sexp::Symbol(s) => Sexp::Symbol(s),
            Sexp::Num(n) => Sexp::Num(n),
            Sexp::List(sexp) => Sexp::List(sexp.into_iter().map(|s| s.map(|v| f(v))).collect()),
            Sexp::Pure(value) => Sexp::Pure(f(value)),
        }
    }
}

impl<T: fmt::Display> Sexp<T> {
    pub fn to_num(&self) -> Result<f64, String> {
        match self {
            Sexp::Num(n) => Ok(*n),
            _ => Err(format!("expected number, but got {}", self)),
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
            Sexp::Pure(value) => write!(f, "{}", value),
        }
    }
}

#[test]
fn test_display() {
    assert_eq!(Sexp::<Nothing>::symbol("foo-bar").to_string(), "foo-bar".to_string());
    assert_eq!(Sexp::<Nothing>::Num(123.).to_string(), "123".to_string());
    assert_eq!(slist![Sexp::<Nothing>::symbol("foo-bar")].to_string(), "(foo-bar)");
    assert_eq!(
        slist![
            Sexp::<Nothing>::symbol("foo"),
            slist![Sexp::<Nothing>::symbol("bar"), Sexp::<Nothing>::symbol("baz")]
        ]
        .to_string(),
        "(foo (bar baz))"
    );
}
