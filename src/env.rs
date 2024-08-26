use std::collections::HashMap;
use crate::sexp::Sexp;

#[derive(Debug)]
pub struct Env {
    vars: HashMap<String, Sexp>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }
    
    pub fn set(&mut self, name: &str, value: Sexp) {
        self.vars.insert(name.to_string(), value);
    }
    
    pub fn find(&mut self, name: &str) -> Option<Sexp> {
        self.vars.get(name).cloned()
    }
}

#[test]
fn test_env() {
    let mut env = Env::new();
    assert_eq!(env.find("foo"), None);
    env.set("foo", Sexp::Num(123.));
    assert_eq!(env.find("foo"), Some(Sexp::Num(123.)));
    assert_eq!(env.find("bar"), None);
    env.set("bar", Sexp::Num(456.));
    assert_eq!(env.find("foo"), Some(Sexp::Num(123.)));
    assert_eq!(env.find("bar"), Some(Sexp::Num(456.)));
}