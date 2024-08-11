use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Sexp {
    Symbol(String),
    Num(f64),
    List(Vec<Sexp>),
}

macro_rules! slist {
    ($( $e:expr ),*) => {
        Sexp::List(vec![$( $e ),*])
    };
}

impl Sexp {
    pub fn symbol(s: impl Into<String>) -> Self {
        Sexp::Symbol(s.into())
    }

    pub const NIL: Sexp = Sexp::List(Vec::new());
}

impl fmt::Display for Sexp {
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
        }
    }
}

#[test]
fn test_display() {
    assert_eq!(Sexp::symbol("foo-bar").to_string(), "foo-bar".to_string());
    assert_eq!(Sexp::Num(123.).to_string(), "123".to_string());
    assert_eq!(slist![Sexp::symbol("foo-bar")].to_string(), "(foo-bar)");
    assert_eq!(
        slist![
            Sexp::symbol("foo"),
            slist![Sexp::symbol("bar"), Sexp::symbol("baz")]
        ]
        .to_string(),
        "(foo (bar baz))"
    );
}
