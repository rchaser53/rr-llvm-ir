use std::*;

use crate::lexer::token::*;
use crate::types::symbol::*;

#[derive(Fail, Debug)]
pub enum CompilerError {
    #[fail(display = "")]
    EndOfFile,

    #[fail(display = "invalid syntax for lexer")]
    InvalidSyntax,

    #[fail(display = "need to delcare type after {}", _0)]
    DeclareType(String),

    #[fail(display = "invalid syntax for parser")]
    InvalidParserSyntax,
}

pub type Result<T> = result::Result<T, CompilerError>;

#[derive(Debug)]
pub struct Lexer<'a> {
    pub bytes: &'a [u8],
    pub position: usize,
    pub rewind_position: usize,
    pub current_row: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let bytes = input.as_bytes();
        Lexer {
            bytes: bytes,
            position: 0,
            rewind_position: 0,
            current_row: 0,
        }
    }

    pub fn save_rewind_position(&mut self) {
        self.rewind_position = self.position;
    }

    pub fn rewind_position(&mut self) {
        self.position = self.rewind_position;
    }

    pub fn create_eof_token(&mut self) -> Token {
        Token::new(TokenType::Eof, String::new(), self.position)
    }

    pub fn handle_reserved_word(&self, word: &str, token: TokenType) -> TokenType {
        match word {
            "while" => TokenType::While,
            "let" => TokenType::Let,
            "fn" => TokenType::Fn,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "elseif" => TokenType::ElseIf,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "break" => TokenType::Break,
            "boolean" => TokenType::PrimaryType(SymbolType::Boolean),
            "int" => TokenType::PrimaryType(SymbolType::Integer),
            "string" => TokenType::PrimaryType(SymbolType::String),
            "null" => TokenType::PrimaryType(SymbolType::Null),
            _ => token,
        }
    }

    pub fn get_next_char(&mut self) -> Result<u8> {
        if self.position < self.bytes.len() {
            Ok(self.bytes[self.position])
        } else {
            Err(CompilerError::EndOfFile)
        }
    }

    pub fn consume_comment(&mut self) -> Result<()> {
        loop {
            let byte = self.get_next_char()?;
            self.position += 1;
            if byte == b'*' {
                let next = self.get_next_char()?;
                if next == b'/' {
                    self.position += 1;
                    return Ok(());
                }
            }
        }
    }

    pub fn consumue_character(&mut self, first_byte: u8, mut num_flag: bool) -> Result<Token> {
        let mut temp_vec: Vec<u8> = Vec::new();
        temp_vec.push(first_byte);
        loop {
            let byte = self.get_next_char()?;
            let break_flg = match byte {
                b'0'...b'9' => {
                    self.position += 1;
                    temp_vec.push(byte);
                    false
                }
                b'a'...b'z' | b'A'...b'Z' | b'_' => {
                    self.position += 1;
                    temp_vec.push(byte);
                    num_flag = false;
                    false
                }
                _ => true,
            };

            if break_flg == true {
                break;
            }
        }

        let token_type = if num_flag == true {
            TokenType::Integer
        } else {
            TokenType::Identifier
        };

        Ok(self.create_token_by_value(token_type, temp_vec)?)
    }

    pub fn create_token_by_value(&mut self, token: TokenType, value_vec: Vec<u8>) -> Result<Token> {
        let ret_string = String::from_utf8(value_vec).map_err(|_| {
            return CompilerError::InvalidSyntax;
        })?;
        Ok(Token::new(
            self.handle_reserved_word(&ret_string, token),
            ret_string.to_owned(),
            self.current_row,
        ))
    }

    pub fn consume_slash(&mut self, target_token: Token) -> Result<(Token, bool)> {
        Ok(if self.get_next_char()? == b'*' {
            self.position += 1;
            self.consume_comment()?;
            (target_token, false)
        } else {
            (
                self.create_token_by_value(TokenType::Divide, vec![b'/'])?,
                true,
            )
        })
    }

    pub fn consume_equal(&mut self) -> Result<Token> {
        Ok(if self.get_next_char()? == b'=' {
            self.position += 1;
            self.create_token_by_value(TokenType::Eq, vec![b'=', b'='])?
        } else {
            self.create_token_by_value(TokenType::Assign, vec![b'='])?
        })
    }

    pub fn consume_ban(&mut self) -> Result<Token> {
        Ok(if self.get_next_char()? == b'=' {
            self.position += 1;
            self.create_token_by_value(TokenType::NotEq, vec![b'!', b'='])?
        } else {
            self.create_token_by_value(TokenType::Bang, vec![b'!'])?
        })
    }

    pub fn consume_lt(&mut self) -> Result<Token> {
        Ok(if self.get_next_char()? == b'=' {
            self.position += 1;
            self.create_token_by_value(TokenType::Lte, vec![b'<', b'='])?
        } else {
            self.create_token_by_value(TokenType::Lt, vec![b'<'])?
        })
    }

    pub fn consume_gt(&mut self) -> Result<Token> {
        Ok(if self.get_next_char()? == b'=' {
            self.position += 1;
            self.create_token_by_value(TokenType::Gte, vec![b'>', b'='])?
        } else {
            self.create_token_by_value(TokenType::Gt, vec![b'>'])?
        })
    }

    pub fn consume_string(&mut self) -> Result<Token> {
        let mut char_vec = Vec::new();
        loop {
            let next_char = self.get_next_char()?;
            self.position += 1;
            if next_char == b'"' {
                break;
            }
            char_vec.push(next_char);
        }
        let string_length = char_vec.len();
        Ok(self.create_token_by_value(TokenType::String(string_length), char_vec)?)
    }

    pub fn next_token(&mut self) -> Result<Token> {
        let mut ret_val: Token = self.create_eof_token();
        loop {
            let byte = self.get_next_char()?;
            self.position += 1;
            let flag = match byte {
                b'0'...b'9' => {
                    ret_val = self.consumue_character(byte, true)?;
                    true
                }
                b'a'...b'z' | b'A'...b'Z' | b'_' => {
                    ret_val = self.consumue_character(byte, false)?;
                    true
                }
                b'"' => {
                    ret_val = self.consume_string()?;
                    true
                }
                b'/' => {
                    let (temp_ret, flag) = self.consume_slash(ret_val)?;
                    ret_val = temp_ret;
                    flag
                }
                b'=' => {
                    ret_val = self.consume_equal()?;
                    true
                }
                b',' => {
                    ret_val = self.create_token_by_value(TokenType::Comma, vec![byte])?;
                    true
                }
                b'.' => {
                    ret_val = self.create_token_by_value(TokenType::Period, vec![byte])?;
                    true
                }
                b'{' => {
                    ret_val = self.create_token_by_value(TokenType::Lbrace, vec![byte])?;
                    true
                }
                b'}' => {
                    ret_val = self.create_token_by_value(TokenType::Rbrace, vec![byte])?;
                    true
                }
                b'(' => {
                    ret_val = self.create_token_by_value(TokenType::Lparen, vec![byte])?;
                    true
                }
                b')' => {
                    ret_val = self.create_token_by_value(TokenType::Rparen, vec![byte])?;
                    true
                }
                b'[' => {
                    ret_val = self.create_token_by_value(TokenType::Lbracket, vec![byte])?;
                    true
                }
                b']' => {
                    ret_val = self.create_token_by_value(TokenType::Rbracket, vec![byte])?;
                    true
                }
                b'!' => {
                    ret_val = self.consume_ban()?;
                    true
                }
                b'*' => {
                    ret_val = self.create_token_by_value(TokenType::Multiply, vec![byte])?;
                    true
                }
                b'%' => {
                    ret_val = self.create_token_by_value(TokenType::Rem, vec![byte])?;
                    true
                }
                b'+' => {
                    ret_val = self.create_token_by_value(TokenType::Plus, vec![byte])?;
                    true
                }
                b'-' => {
                    ret_val = self.create_token_by_value(TokenType::Minus, vec![byte])?;
                    true
                }
                b'<' => {
                    ret_val = self.consume_lt()?;
                    true
                }
                b'>' => {
                    ret_val = self.consume_gt()?;
                    true
                }
                b':' => {
                    ret_val = self.create_token_by_value(TokenType::Colon, vec![byte])?;
                    true
                }
                b';' => {
                    ret_val = self.create_token_by_value(TokenType::Semicolon, vec![byte])?;
                    true
                }
                b'\n' | b'\r' => {
                    self.current_row += 1;
                    false
                }
                b' ' => false,
                _ => {
                    panic!("{} cannot be handled.", byte);
                }
            };

            if flag == true {
                break;
            }
        }
        Ok(ret_val)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::*;

    #[allow(dead_code)]
    fn lexer_assert(token: Token, token_type: TokenType, value: &str) {
        let expected = Token::new(token_type, value.to_string(), 0);
        assert_eq!(token, expected);
    }

    #[test]
    fn digit() {
        let mut lexer = Lexer::new(
            r#"
    123 456
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "123");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "456");
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new(
            r#"
    123 ab_c 45d6
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "123");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Identifier, "ab_c");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Identifier, "45d6");
    }

    #[test]
    fn string() {
        let mut lexer = Lexer::new(
            r#"
    "abc" "def3"
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::String(3), "abc");
        lexer_assert(lexer.next_token().unwrap(), TokenType::String(4), "def3");
    }

    #[test]
    fn array() {
        let mut lexer = Lexer::new(
            r#"
    [ 1, 2, 3 ]
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::Lbracket, "[");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "1");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Comma, ",");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "2");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Comma, ",");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "3");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Rbracket, "]");
    }

    #[test]
    fn comment() {
        let mut lexer = Lexer::new(
            r#"
    0 /* 123 */ 2
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "0");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "2");
    }

    #[test]
    fn ban() {
        let mut lexer = Lexer::new(
            r#"
    let abc = !abc
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::Let, "let");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Identifier, "abc");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Assign, "=");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Bang, "!");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Identifier, "abc");
    }

    #[test]
    fn division_multiple() {
        let mut lexer = Lexer::new(
            r#"
    1 / 323 * 3 / 2
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "1");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Divide, "/");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "323");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Multiply, "*");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "3");
    }

    #[test]
    fn gt() {
        let mut lexer = Lexer::new(
            r#"
    123 <= 456
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "123");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Lte, "<=");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "456");
    }

    #[test]
    fn if_test() {
        let mut lexer = Lexer::new(
            r#"
    if 123 == 456
    "#,
        );
        lexer_assert(lexer.next_token().unwrap(), TokenType::If, "if");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "123");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Eq, "==");
        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "456");
    }

    #[test]
    fn llvm_token_test() {
        let mut lexer = Lexer::new(
            r#"
    int string boolean null
    "#,
        );
        lexer_assert(
            lexer.next_token().unwrap(),
            TokenType::PrimaryType(SymbolType::Integer),
            "int",
        );
        lexer_assert(
            lexer.next_token().unwrap(),
            TokenType::PrimaryType(SymbolType::String),
            "string",
        );
        lexer_assert(
            lexer.next_token().unwrap(),
            TokenType::PrimaryType(SymbolType::Boolean),
            "boolean",
        );
        lexer_assert(
            lexer.next_token().unwrap(),
            TokenType::PrimaryType(SymbolType::Null),
            "null",
        );
    }

    #[test]
    fn rewind_test() {
        let mut lexer = Lexer::new(
            r#"
    123 == 456
    "#,
        );

        lexer_assert(lexer.next_token().unwrap(), TokenType::Integer, "123");
        lexer.save_rewind_position();
        lexer_assert(lexer.next_token().unwrap(), TokenType::Eq, "==");
        lexer.rewind_position();
        lexer_assert(lexer.next_token().unwrap(), TokenType::Eq, "==");
    }

}
