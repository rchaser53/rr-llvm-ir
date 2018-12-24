use std::*;

use crate::lexer::lexer::*;
use crate::lexer::token::*;

use crate::parser::expressions::*;
use crate::parser::infix::*;
use crate::parser::precedence::*;
use crate::parser::prefix::*;
use crate::parser::statements::*;
use crate::parser::sufix::*;

use crate::types::symbol::*;

#[macro_export]
macro_rules! create_err_1 {
    ($name:ident, $error_message:expr) => {
        pub fn $name(input: &str) {
            format!($error_message, input)
        }
    };
}

#[macro_export]
macro_rules! create_err_2 {
    ($name:ident, $input_type1:ty, $input_type2:ty, $error_message:expr ) => {
        pub fn $name(input1: $input_type1, input2: $input_type2) -> String {
            format!($error_message, input1, input2)
        }
    };
}

create_err_2!(
    parse_sufix_error,
    TokenType,
    usize,
    "could not parse sufix {:?} as integer. row: {:?}"
);

pub fn else_next_token(err: CompilerError) -> Result<Token> {
    match err {
        CompilerError::EndOfFile => Ok(Token::new(TokenType::Null, String::new(), 0)),
        _ => Err(err),
    }
}

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer<'a>,
    pub cur_token: Token,
    pub peek_token: Token,
    pub preserve_cur_token: Token,
    pub preserve_peek_token: Token,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Result<Parser<'a>> {
        let current_token = lexer.next_token().or_else(else_next_token)?;
        let peek_token = lexer.next_token().or_else(else_next_token)?;

        let none_token = Token::new(TokenType::Null, String::new(), 0);

        Ok(Parser {
            lexer: lexer,
            cur_token: current_token,
            peek_token: peek_token,
            preserve_cur_token: none_token.clone(),
            preserve_peek_token: none_token,
            errors: Vec::new(),
        })
    }

    pub fn next_token(&mut self) -> Result<()> {
        self.cur_token = self.peek_token.to_owned();
        self.peek_token = self.lexer.next_token().or_else(else_next_token)?;
        Ok(())
    }

    pub fn save_rewind_position(&mut self) {
        self.lexer.save_rewind_position();
        self.preserve_cur_token = self.cur_token.clone();
        self.preserve_peek_token = self.peek_token.clone();
    }

    pub fn rewind_position(&mut self) {
        self.lexer.rewind_position();
        self.cur_token = self.preserve_cur_token.clone();
        self.peek_token = self.preserve_peek_token.clone();
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Vec::new();
        while self.peek_token.token_type != TokenType::Null {
            program.push(self.parse_statement()?);
            self.next_token()?;
        }
        Ok(program)
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        Ok(match self.cur_token.token_type {
            TokenType::Identifier => self.handle_identifier()?,
            TokenType::Let => self.parse_let_statement()?,
            TokenType::Return => self.parse_return_statement()?,
            TokenType::While => self.parse_while_statement()?,
            TokenType::If => self.parse_if_statement()?,
            _ => self.parse_expression_statement()?,
        })
    }

    pub fn handle_identifier(&mut self) -> Result<Statement> {
        self.save_rewind_position();
        let result = if self.try_parse_array_element()? {
            self.parse_assign_array_element()?
        } else if self.peek_token_is(TokenType::Assign) {
            self.parse_assign_statement()?
        } else {
            self.parse_expression_statement()?
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token()?;
        }

        Ok(result)
    }

    pub fn try_parse_array_element(&mut self) -> Result<bool> {
        if self.peek_token_is(TokenType::Lbracket) == false {
            return Ok(false);
        };
        self.next_token()?;
        self.next_token()?;

        self.parse_expression(Precedences::Lowest)?;
        self.next_token()?;
        let result =
            self.cur_token_is(TokenType::Rbracket) && self.peek_token_is(TokenType::Assign);

        self.rewind_position();
        Ok(result)
    }

    pub fn parse_assign_array_element(&mut self) -> Result<Statement> {
        let left = self.parse_expression(Precedences::Lowest)?;
        self.next_token()?;
        self.next_token()?;

        let right = self.parse_expression(Precedences::Lowest)?;
        match left {
            Expression::ArrayElement(ident, index_expression, _) => Ok(
                Statement::AssignmentAggregate(ident, right, *index_expression),
            ),
            _ => unreachable!(),
        }
    }

    pub fn parse_assign_statement(&mut self) -> Result<Statement> {
        let token = self.cur_token.to_owned();
        let name = Identifier(token.value.to_owned());

        if self.expect_peek(TokenType::Assign)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }

        self.next_token()?;
        let value = self.parse_expression(Precedences::Lowest)?;

        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token()?;
        }

        Ok(Statement::Assignment(name, value))
    }

    pub fn parse_let_statement(&mut self) -> Result<Statement> {
        if self.expect_peek(TokenType::Identifier)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }
        let token = self.cur_token.to_owned();
        let name = Identifier(token.value.to_owned());
        let symbol_type = self.parse_identifier_symbol_type()?;

        if self.expect_peek(TokenType::Assign)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }

        self.next_token()?;
        let expression = self.parse_expression(Precedences::Lowest)?;

        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token()?;
        }

        Ok(Statement::Let(name, expression, symbol_type))
    }

    pub fn parse_identifier_symbol_type(&mut self) -> Result<SymbolType> {
        if self.peek_token_is(TokenType::Colon) == false {
            return Err(CompilerError::DeclareType(
                self.peek_token.value.to_string(),
            ))?;
        }
        self.next_token()?;
        self.next_token()?;
        let base_type = self.extract_symbol_type()?;

        if self.peek_token_is(TokenType::Lbracket) == false {
            return Ok(base_type);
        }
        self.next_token()?;

        if self.peek_token_is(TokenType::Rbracket) == false {
            return Err(CompilerError::InvalidParserSyntax)?;
        }
        self.next_token()?;

        Ok(SymbolType::Array(Box::new(base_type)))
    }

    pub fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token()?;
        let return_value = self.parse_expression(Precedences::Lowest)?;

        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token()?;
        }

        Ok(Statement::Return(return_value))
    }

    pub fn parse_while_statement(&mut self) -> Result<Statement> {
        if self.expect_peek(TokenType::Lparen)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }
        self.next_token()?;

        let condition = self.parse_expression(Precedences::Lowest)?;
        if self.expect_peek(TokenType::Rparen)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }

        if self.expect_peek(TokenType::Lbrace)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }

        let block = self.parse_block_statement()?;
        Ok(Statement::While(condition, block))
    }

    pub fn parse_if_statement(&mut self) -> Result<Statement> {
        let if_row = self.lexer.current_row;
        let mut condtions = Vec::new();
        let mut bodies = Vec::new();
        let mut loop_flag = true;

        while loop_flag {
            if self.expect_peek(TokenType::Lparen)? == false {
                return Err(CompilerError::InvalidParserSyntax);
            }
            self.next_token()?;
            condtions.push(self.parse_expression(Precedences::Lowest)?);

            if self.expect_peek(TokenType::Rparen)? == false {
                return Err(CompilerError::InvalidParserSyntax);
            }
            if self.expect_peek(TokenType::Lbrace)? == false {
                return Err(CompilerError::InvalidParserSyntax);
            }

            bodies.push(self.parse_block_statement()?);
            if self.peek_token_is(TokenType::ElseIf) {
                self.next_token()?;
            } else {
                loop_flag = false;
            }
        }

        if self.peek_token_is(TokenType::Else) {
            self.next_token()?;
            condtions.push(Expression::Boolean(true, Location::new(if_row)));
            if self.expect_peek(TokenType::Lbrace)? == false {
                return Err(CompilerError::InvalidParserSyntax);
            }
            bodies.push(self.parse_block_statement()?);
        } else {
            condtions.push(Expression::Boolean(false, Location::new(if_row)));
            bodies.push(Vec::new());
        };

        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token()?;
        }

        Ok(Statement::If {
            conditions: condtions,
            bodies: bodies,
            location: Location::new(if_row),
        })
    }

    pub fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let mut block = Vec::new();
        self.next_token()?;

        while self.cur_token_is(TokenType::Rbrace) == false {
            block.push(self.parse_statement()?);
            self.next_token()?;
        }
        Ok(block)
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expression = self.parse_expression(Precedences::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token()?;
        }

        Ok(Statement::Expression(expression))
    }

    pub fn parse_expression(&mut self, precedence: Precedences) -> Result<Expression> {
        let mut left_exp = self.parse_left_expression()?;
        let is_identifier = match &left_exp {
            Expression::Identifier(_, _) => true,
            _ => false,
        };

        while self.peek_token_is(TokenType::Semicolon) == false
            && precedence < self.peek_precedence()
        {
            let token = self.peek_token.to_owned();
            left_exp = match token.token_type {
                TokenType::Minus | TokenType::Plus => {
                    if is_identifier && self.try_parse_sufix()? {
                        self.next_token()?;
                        self.parse_sufix(left_exp)?
                    } else {
                        self.next_token()?;
                        self.parse_infix_expression(left_exp)?
                    }
                }
                TokenType::Divide
                | TokenType::Eq
                | TokenType::Gt
                | TokenType::Gte
                | TokenType::Lt
                | TokenType::Lte
                | TokenType::Multiply
                | TokenType::NotEq
                | TokenType::Rem => {
                    self.next_token()?;
                    self.parse_infix_expression(left_exp)?
                }
                TokenType::Lparen => {
                    self.next_token()?;
                    self.parse_call_expression(left_exp)?
                }
                _ => {
                    self.no_prefix_parse_fn_error(token)?;
                    return Ok(left_exp);
                }
            };
        }

        Ok(left_exp)
    }

    pub fn parse_left_expression(&mut self) -> Result<Expression> {
        let token = self.cur_token.to_owned();
        match token.token_type {
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::Integer => self.parse_integer_literal(),
            TokenType::Fn => self.parse_function_literal(),
            TokenType::Identifier => self.parse_identifier(),
            TokenType::Lbracket => self.parse_array(),
            TokenType::Lparen => self.parse_grouped_expression(),
            TokenType::String(_) => self.parse_string_literal(),
            TokenType::True | TokenType::False => self.parse_boolean(),
            _ => {
                self.no_prefix_parse_fn_error(token)?;
                Err(CompilerError::InvalidParserSyntax)
            }
        }
    }

    pub fn try_parse_sufix(&mut self) -> Result<bool> {
        self.save_rewind_position();
        let previous_token = self.peek_token.to_owned();
        self.next_token()?;

        let result = self.peek_token.token_type == previous_token.token_type;
        self.rewind_position();

        Ok(result)
    }

    pub fn parse_sufix(&mut self, left: Expression) -> Result<Expression> {
        let token = self.peek_token.to_owned();
        let sufix_type = match token.token_type {
            TokenType::Minus => Sufix::Minus,
            TokenType::Plus => Sufix::Plus,
            _ => {
                self.errors
                    .push(parse_sufix_error(token.token_type, token.current_row));
                return Err(CompilerError::InvalidParserSyntax);
            }
        };
        self.next_token()?;

        Ok(Expression::Sufix(
            sufix_type,
            Box::new(left),
            Location::new(self.lexer.current_row),
        ))
    }

    pub fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let token = self.cur_token.to_owned();
        self.next_token()?;
        let right = self.parse_expression(Precedences::Prefix)?;
        Ok(Expression::Prefix(
            self.convert_token_to_prefix(token)?,
            Box::new(right),
            Location::new(self.lexer.current_row),
        ))
    }

    pub fn parse_integer_literal(&mut self) -> Result<Expression> {
        let token = &self.cur_token;
        if let Ok(value) = token.value.parse::<u64>() {
            return Ok(Expression::IntegerLiteral(
                value,
                Location::new(self.lexer.current_row),
            ));
        } else {
            self.errors.push(format!(
                "could not parse {} as integer. row: {}",
                token.value, token.current_row
            ));
            return Err(CompilerError::InvalidParserSyntax);
        };
    }

    pub fn parse_function_literal(&mut self) -> Result<Expression> {
        if self.expect_peek(TokenType::Lparen)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }

        let parameter_symbols = self.parse_function_parameters()?;
        let return_type = self.parse_return_type()?;

        if self.expect_peek(TokenType::Lbrace)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }

        Ok(Expression::Function {
            parameter_symbols,
            body: self.parse_block_statement()?,
            location: Location::new(self.lexer.current_row),
            return_type,
        })
    }

    pub fn parse_function_parameters(&mut self) -> Result<Vec<Box<Symbol>>> {
        let mut symbols = Vec::new();

        if self.peek_token_is(TokenType::Rparen) {
            self.next_token()?;
            return Ok(symbols);
        }
        let name = self.peek_token.value.to_string();

        self.next_token()?;
        if self.expect_peek(TokenType::Colon)? == false {
            self.emit_error_for_funciton()?;
        }
        self.next_token()?;

        symbols.push(Box::new(self.extract_symbol(name)?));

        while self.peek_token_is(TokenType::Comma) {
            self.next_token()?;
            self.next_token()?;
            let name = self.cur_token.value.to_string();

            if self.expect_peek(TokenType::Colon)? == false {
                self.emit_error_for_funciton()?;
            }

            self.next_token()?;
            symbols.push(Box::new(self.extract_symbol(name)?));
        }

        if self.expect_peek(TokenType::Rparen)? == false {
            self.emit_error_for_funciton()?;
        }

        Ok(symbols)
    }

    pub fn parse_return_type(&mut self) -> Result<SymbolType> {
        if self.expect_peek(TokenType::Colon)? == false {
            self.emit_error_for_funciton()?;
        }

        self.next_token()?;
        Ok(self.extract_symbol_type()?)
    }

    pub fn parse_identifier(&mut self) -> Result<Expression> {
        let token = self.cur_token.clone();
        if self.peek_token_is(TokenType::Lbracket) {
            return Ok(self.parse_array_element(token)?);
        }

        Ok(Expression::Identifier(
            Identifier(token.value.to_owned()),
            Location::new(self.lexer.current_row),
        ))
    }

    pub fn parse_array_element(&mut self, token: Token) -> Result<Expression> {
        self.next_token()?;
        self.next_token()?;
        let index_expression = self.parse_expression(Precedences::Lowest)?;
        while self.cur_token_is(TokenType::Rbracket) == false {
            self.next_token()?;
        }

        Ok(Expression::ArrayElement(
            Identifier(token.value.to_owned()),
            Box::new(index_expression),
            Location::new(self.lexer.current_row),
        ))
    }

    pub fn parse_array(&mut self) -> Result<Expression> {
        let mut elements: Vec<Expression> = Vec::new();

        if self.peek_token_is(TokenType::Rbracket) == true {
            return Ok(Expression::Array(elements)); // TODO
        }
        self.next_token()?;

        elements.push(self.parse_expression(Precedences::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token()?;
            self.next_token()?;
            elements.push(self.parse_expression(Precedences::Lowest)?);
        }

        if self.expect_peek(TokenType::Rbracket)? == false {
            panic!("parse error."); // TODO
        }

        Ok(Expression::Array(elements))
    }

    pub fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token()?;
        let exp = self.parse_expression(Precedences::Lowest)?;

        if self.expect_peek(TokenType::Rparen)? == false {
            return Err(CompilerError::InvalidParserSyntax);
        }
        Ok(exp)
    }

    pub fn parse_string_literal(&mut self) -> Result<Expression> {
        Ok(Expression::StringLiteral(
            self.cur_token.value.to_owned(),
            Location::new(self.lexer.current_row),
        ))
    }

    pub fn parse_boolean(&mut self) -> Result<Expression> {
        Ok(Expression::Boolean(
            self.cur_token_is(TokenType::True),
            Location::new(self.lexer.current_row),
        ))
    }

    pub fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let token = self.cur_token.to_owned();
        let precedence = self.cur_precedence();
        self.next_token()?;

        let right = self.parse_expression(precedence)?;
        Ok(Expression::Infix(
            self.convert_token_to_infix(token.clone())?,
            Box::new(left),
            Box::new(right),
            Location::new(self.lexer.current_row),
        ))
    }

    pub fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let expr = Expression::Call(Call {
            function: Box::new(function),
            arguments: self.parse_call_arguments()?,
            location: Location::new(self.lexer.current_row),
        });

        match expr.clone() {
            Expression::Function { .. } => {
                let token = self.peek_token.to_owned();
                match token.token_type {
                    TokenType::Lparen => {
                        self.next_token()?;
                        Ok(self.parse_call_expression(expr)?)
                    }
                    _ => Ok(expr),
                }
            }
            _ => Ok(expr),
        }
    }

    pub fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut args = Vec::new();
        if self.peek_token_is(TokenType::Rparen) {
            self.next_token()?;
            return Ok(args);
        }
        self.next_token()?;
        args.push(self.parse_expression(Precedences::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token()?;
            self.next_token()?;
            args.push(self.parse_expression(Precedences::Lowest)?);
        }

        if self.expect_peek(TokenType::Rparen)? == false {
            return Ok(args);
        }
        Ok(args)
    }

    pub fn convert_token_to_infix(&mut self, token: Token) -> Result<Infix> {
        match token.token_type {
            TokenType::Plus => Ok(Infix::Plus),
            TokenType::Minus => Ok(Infix::Minus),
            TokenType::Divide => Ok(Infix::Divide),
            TokenType::Multiply => Ok(Infix::Multiply),
            TokenType::Rem => Ok(Infix::Rem),
            TokenType::Eq => Ok(Infix::Eq),
            TokenType::NotEq => Ok(Infix::NotEq),
            TokenType::Gte => Ok(Infix::Gte),
            TokenType::Gt => Ok(Infix::Gt),
            TokenType::Lte => Ok(Infix::Lte),
            TokenType::Lt => Ok(Infix::Lt),
            _ => {
                self.errors.push(format!(
                    "{:?} is not a token for infix. row: {}",
                    token.token_type, token.current_row
                ));
                Err(CompilerError::InvalidParserSyntax)
            }
        }
    }

    pub fn convert_token_to_prefix(&mut self, token: Token) -> Result<Prefix> {
        match token.token_type {
            TokenType::Plus => Ok(Prefix::Plus),
            TokenType::Minus => Ok(Prefix::Minus),
            TokenType::Bang => Ok(Prefix::Bang),
            _ => {
                self.errors.push(format!(
                    "{:?} is not a token for prefix. row: {}",
                    token.token_type, token.current_row
                ));
                Err(CompilerError::InvalidParserSyntax)
            }
        }
    }

    pub fn cur_precedence(&self) -> Precedences {
        let token_type = &self.cur_token.token_type;
        if PRECEDENCE_TOKEN_MAP.contains_key(token_type) {
            return PRECEDENCE_TOKEN_MAP[token_type].to_owned();
        }
        Precedences::Lowest
    }

    pub fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    pub fn expect_peek(&mut self, token_type: TokenType) -> Result<bool> {
        Ok(if self.peek_token_is(token_type) {
            self.next_token()?;
            true
        } else {
            self.peek_error(self.peek_token.clone())?;
            false
        })
    }

    pub fn peek_token_is(&mut self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    pub fn peek_precedence(&mut self) -> Precedences {
        let token_type = &self.peek_token.token_type;
        if PRECEDENCE_TOKEN_MAP.contains_key(token_type) {
            return PRECEDENCE_TOKEN_MAP[token_type].to_owned();
        }
        Precedences::Lowest
    }

    pub fn has_error(&self) -> bool {
        self.errors.len() > 0
    }

    pub fn emit_error(&self) -> String {
        self.errors.join("\n")
    }

    pub fn emit_error_for_funciton(&mut self) -> Result<()> {
        self.errors
            .push(format!("parse failed at row:{}", self.lexer.current_row));
        self.skip_until_semicolon()?;
        Ok(())
    }

    pub fn no_prefix_parse_fn_error(&mut self, token: Token) -> Result<()> {
        self.errors.push(format!(
            "no prefix parse function for {:?}. row: {}",
            token.token_type, token.current_row
        ));
        self.skip_until_semicolon()?;
        Ok(())
    }

    pub fn peek_error(&mut self, token: Token) -> Result<()> {
        self.errors.push(format!(
            "expected next token to be {:?} instead. row: {}",
            token.token_type, token.current_row
        ));
        self.skip_until_semicolon()?;
        Ok(())
    }

    pub fn skip_until_semicolon(&mut self) -> Result<()> {
        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token()?;
        }
        Ok(())
    }

    pub fn extract_symbol(&mut self, name: String) -> Result<Symbol> {
        Ok(Symbol::new(&name, self.extract_symbol_type()?, false))
    }

    pub fn extract_symbol_type(&mut self) -> Result<SymbolType> {
        let token = self.cur_token.to_owned();
        match &token.token_type {
            TokenType::Identifier => Ok(SymbolType::Custom(token.value.to_string())),
            TokenType::PrimaryType(parimary_type) => Ok(SymbolType::Primary(parimary_type.clone())),
            _ => Err(CompilerError::InvalidParserSyntax),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::*;

    #[test]
    fn let_statements() {
        let input = r#"
      let x: int = 5;
      let y: boolean = false;
      let z: string = "abc";
      let foobar: int = 939393;
    "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "let x: int = 5");
        statement_assert(&program[1], "let y: boolean = false");
        statement_assert(&program[2], r#"let z: string = "abc""#);
        statement_assert(&program[3], "let foobar: int = 939393");
    }

    #[test]
    fn return_statements() {
        let input = r#"
      return 5;
      return 10;
      return 939393;
    "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "return 5");
        statement_assert(&program[1], "return 10");
        statement_assert(&program[2], "return 939393");
    }

    #[test]
    fn while_statements() {
        let input = r#"
    while (true) {
      let i: int = i + 1;
    }
    let a: int = 3;
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "while (true) { let i: int = (i + 1) }");
    }

    #[test]
    fn assign_statements() {
        let input = r#"
      let x: int = 5;
      x = 10;
      x = 10 * 3;
    "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "let x: int = 5");
        statement_assert(&program[1], "x = 10");
        statement_assert(&program[2], "x = (10 * 3)");
    }

    #[test]
    fn assign_aggregate_statements() {
        let input = r#"
      let x: int[] = [1, 2, 3];
      x[0] = 10;
    "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "let x: int[] = [1, 2, 3]");
        statement_assert(&program[1], "x[0] = 10");
    }

    #[test]
    fn operator_precedence_parsing() {
        let input = r#"
    -a * b;
    !-a;
    a + b + c;
    a + b - c;
    a * b * c;
    a * b / c;
    a + b / c;
    a + b * c + d / e - f;
    3 + 4 - 5 * 5;
    5 > 4 == 3 < 4;
    5 < 4 != 3 > 4;
    3 + 4 * 5 == 3 * 1 + 4 * 5;
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "((-a) * b)");
        statement_assert(&program[1], "(!(-a))");
        statement_assert(&program[2], "((a + b) + c)");
        statement_assert(&program[3], "((a + b) - c)");
        statement_assert(&program[4], "((a * b) * c)");
        statement_assert(&program[5], "((a * b) / c)");
        statement_assert(&program[6], "(a + (b / c))");
        statement_assert(&program[7], "(((a + (b * c)) + (d / e)) - f)");
        statement_assert(&program[8], "((3 + 4) - (5 * 5))");
        statement_assert(&program[9], "((5 > 4) == (3 < 4))");
        statement_assert(&program[10], "((5 < 4) != (3 > 4))");
        statement_assert(&program[11], "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))");
    }

    #[test]
    fn if_else_parsing() {
        let input = r#"
    if(a > b) {};
    if(a > b) { return 1; };
    if(a > b) { return 1; } else { return 0; };
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "if((a > b)) {  } elseif(false) {  }");
        statement_assert(&program[1], "if((a > b)) { return 1 } elseif(false) {  }");
        statement_assert(
            &program[2],
            "if((a > b)) { return 1 } elseif(true) { return 0 }",
        );
    }

    #[test]
    fn boolean_parsing() {
        let input = r#"
    true;
    false;
    3 > 5 == false;
    3 < 5 == true;
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "true");
        statement_assert(&program[1], "false");
        statement_assert(&program[2], "((3 > 5) == false)");
        statement_assert(&program[3], "((3 < 5) == true)");
    }

    #[test]
    fn array_parsing() {
        let input = r#"
    [1, 2, 3];
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "[1, 2, 3]");
    }

    #[test]
    fn array_element_parsing() {
        let input = r#"
      let a: int[] = [1, 2, 3];
      a[1];
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "let a: int[] = [1, 2, 3]");
        statement_assert(&program[1], "a[1]");
    }

    #[test]
    fn funciton_parsing() {
        let input = r#"
    fn(): null {};
    fn(x: int): int {};
    fn(x: int, y: boolean, z: string): boolean {};
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "fn() {  }");
        statement_assert(&program[1], "fn(x) {  }");
        statement_assert(&program[2], "fn(x, y, z) {  }");
    }

    #[test]
    fn call_parsing() {
        let input = r#"
    a + add(b * c) + d;
    add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));
    add(a + b + c * d / f + g);
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "((a + add((b * c))) + d)");
        statement_assert(
            &program[1],
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        );
        statement_assert(&program[2], "add((((a + b) + ((c * d) / f)) + g))");
    }

    #[test]
    fn wrong_prefix() {
        let input = r#"

      return > 3;
    "#;
        let _ = parse_and_emit_error(input, vec!["no prefix parse function for Gt. row: 2"]);
    }

    #[test]
    fn sufix_parsing() {
        let input = r#"
      a++;
  "#;
        let program = parse_input(input).unwrap();
        statement_assert(&program[0], "(a++)");
    }
}
