use crate::types::symbol::*;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TokenType {
    Integer,
    String(usize),
    Identifier,
    PrimaryType(PrimaryType),

    Eof,
    Assign,
    Colon,
    Comma,
    Period,
    Semicolon,

    Lparen,   // (
    Rparen,   // )
    Lbrace,   // {
    Rbrace,   // }
    Lbracket, // [
    Rbracket, // ]

    // preserve word
    Fn,     // fn
    True,   // true
    False,  // false
    If,     // if
    Else,   // else
    ElseIf, // elseif
    Let,    // let
    Return, // return
    While,  // while
    Break,  // break

    // for Arithmetic
    Eq,       // =
    NotEq,    // !=
    Lt,       // <
    Lte,      // <=
    Gt,       // >
    Gte,      // >=
    Plus,     // +
    Minus,    // -
    Divide,   // /
    Multiply, // *
    Rem,      // %
    Bang,     // !

    Null, // for Parser::new
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub current_row: usize,
}

impl Token {
    pub fn new(token_type: TokenType, value: String, row: usize) -> Token {
        Token {
            token_type: token_type,
            value: value,
            current_row: row,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.token_type == other.token_type && self.value == other.value
    }
}
