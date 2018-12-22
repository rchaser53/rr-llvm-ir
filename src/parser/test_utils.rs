use std::*;

use crate::lexer::lexer::*;

use crate::parser::parser::*;
use crate::parser::statements::*;

/* below the test implementation */
#[allow(dead_code)]
pub fn statement_assert(statement: &Statement, expect: &str) {
    assert_eq!(statement.string(), expect);
}

#[allow(dead_code)]
pub fn parse_input(input: &str) -> Result<Program> {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer)?;
    parser.parse_program()
}

#[allow(dead_code)]
pub fn parse_and_emit_error(input: &str, error_stack: Vec<&str>) -> Result<()> {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer)?;
    let program = parser.parse_program();

    if parser.has_error() == false {
        panic!("no errors found. return program is {:?}", program);
    }

    assert_eq!(parser.emit_error(), error_stack.join("\n"));

    Ok(())
}
