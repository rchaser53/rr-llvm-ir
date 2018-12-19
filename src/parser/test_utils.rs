use crate::lexer::lexer::*;

use crate::parser::parser::*;
use crate::parser::statements::*;

/* below the test implementation */
#[allow(dead_code)]
pub fn statement_assert(statement: &Statement, expect: &str) {
    assert!(
        statement.string() == expect,
        "\r\nexpected: {:?} \r\nactual: {:?}",
        expect,
        statement.string()
    );
}

#[allow(dead_code)]
pub fn parse_input(input: &str) -> Program {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    parser.parse_program()
}

#[allow(dead_code)]
pub fn parse_and_emit_error(input: &str, error_stack: Vec<&str>) {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    if parser.has_error() == false {
        panic!("no errors found. return program is {:?}", program);
    }

    assert!(
        parser.emit_error() == error_stack.join("\n"),
        "\r\nexpected: {:?} \r\nactual: {:?}",
        parser.emit_error(),
        error_stack.join("\n")
    );
}
