use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Sufix {
    Plus,
    Minus,
}

impl fmt::Display for Sufix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Sufix::Plus => write!(f, "++"),
            Sufix::Minus => write!(f, "--"),
        }
    }
}
