use crate::eval::Value;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct Env {
    payload: Rc<RefCell<EnvPayload>>,
}

#[derive(Debug, Clone)]
pub struct EnvPayload {
    vars: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Env {
    pub fn new(parent: Option<Env>) -> Self {
        let payload = EnvPayload {
            vars: HashMap::new(),
            parent,
        };
        Env {
            payload: Rc::new(RefCell::new(payload)),
        }
    }

    // (begin (set foo 123) foo)              => 123
    // (begin (begin (set foo 123) 456) foo)  => 123
    // (begin (lbegin (set foo 123) 456) foo) => error: undefined variable: `foo`
    // (begin (set foo 123) (lbegin (set foo 456)) foo) => 456
    pub fn set(&self, name: &str, mut value: Value) {
        if let Some(ref mut parent) = self.payload.borrow_mut().parent {
            match parent.set_if_defined(name, value) {
                Some(returned_value) => value = returned_value,
                None => return,
            }
        }

        self.payload
            .borrow_mut()
            .vars
            .insert(name.to_string(), value);
    }

    fn set_if_defined(&self, name: &str, value: Value) -> Option<Value> {
        let mut payload = self.payload.borrow_mut();
        // TODO: HashMap::entry を使う
        if payload.vars.contains_key(name) {
            payload.vars.insert(name.to_string(), value);
            None
        } else {
            Some(value)
        }
    }

    pub fn find(&self, name: &str) -> Option<Value> {
        let payload = self.payload.borrow();
        match payload.vars.get(name) {
            Some(value) => Some(value.clone()),
            None => payload.parent.as_ref().and_then(|p| p.find(name)),
        }
    }
}

#[test]
fn test_env() {
    use crate::sexp::Sexp;

    let env = Env::new(None);
    assert_eq!(env.find("foo"), None);
    env.set("foo", Sexp::Num(123.));
    assert_eq!(env.find("foo"), Some(Sexp::Num(123.)));
    assert_eq!(env.find("bar"), None);
    env.set("bar", Sexp::Num(456.));
    assert_eq!(env.find("foo"), Some(Sexp::Num(123.)));
    assert_eq!(env.find("bar"), Some(Sexp::Num(456.)));

    let child_env = Env::new(Some(env.clone()));
    child_env.set("bar", Sexp::Num(0.));
    child_env.set("baz", Sexp::Num(789.));
    assert_eq!(child_env.find("foo"), Some(Sexp::Num(123.)));
    assert_eq!(child_env.find("bar"), Some(Sexp::Num(0.)));
    assert_eq!(child_env.find("baz"), Some(Sexp::Num(789.)));
    assert_eq!(env.find("foo"), Some(Sexp::Num(123.)));
    assert_eq!(env.find("bar"), Some(Sexp::Num(0.)));
    assert_eq!(env.find("baz"), None);
}
