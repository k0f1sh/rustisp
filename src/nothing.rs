use std::fmt;

// Kotlinリスペクト
#[derive(PartialEq, Debug)]
pub enum Nothing {}

impl Nothing {
    pub fn absurd(&self) -> ! {
        match *self {}
    }
}

impl fmt::Display for Nothing {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        match *self {}
    }
}
