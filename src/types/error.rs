use std::*;

pub type Result<T> = result::Result<T, SymbolError>;

#[derive(Fail, Debug)]
pub enum SymbolError {
    #[fail(display = "left type is {}. right type is {}", _0, _1)]
    NotEqualSymbolType(String, String),

    #[fail(display = "{} is undefined", _0)]
    UndefinedSymbol(String),

    #[fail(display = "{} is already used", _0)]
    AlreadyUsedSymbol(String),
}
