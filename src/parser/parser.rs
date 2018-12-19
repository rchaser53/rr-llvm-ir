use crate::lexer::lexer::*;
use crate::lexer::token::*;

use crate::parser::expressions::*;
use crate::parser::infix::*;
use crate::parser::precedence::*;
use crate::parser::prefix::*;
use crate::parser::statements::*;
use crate::parser::sufix::*;
use crate::parser::test_utils::*;

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

pub struct Parser<'a> {
    pub lexer: &'a mut Lexer<'a>,
    pub cur_token: Option<Token>,
    pub peek_token: Option<Token>,
    pub preserve_cur_token: Option<Token>,
    pub preserve_peek_token: Option<Token>,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer: lexer,
            cur_token: current_token,
            peek_token: peek_token,
            preserve_cur_token: None,
            preserve_peek_token: None,
            errors: Vec::new(),
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.to_owned();
        self.peek_token = self.lexer.next_token();
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

    pub fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();

        while self.cur_token != None {
            if let Some(stmt) = self.parse_statement() {
                program.push(stmt);
            }
            self.next_token();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        if let Some(token) = self.cur_token.to_owned() {
            return match token.token_type {
                TokenType::Identifier => self.handle_identifier(),
                TokenType::Let => self.parse_let_statement(),
                TokenType::Return => self.parse_return_statement(),
                TokenType::While => self.parse_while_statement(),
                TokenType::If => self.parse_if_statement(),
                _ => self.parse_expression_statement(),
            };
        } else {
            return None;
        }
    }

    pub fn handle_identifier(&mut self) -> Option<Statement> {
        self.save_rewind_position();
        let result = if self.try_parse_array_element() {
            self.parse_assign_array_element()
        } else if self.peek_token_is(TokenType::Assign) {
            self.parse_assign_statement()
        } else {
            self.parse_expression_statement()
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        result
    }

    pub fn try_parse_array_element(&mut self) -> bool {
        if self.peek_token_is(TokenType::Lbracket) == false {
            return false;
        };
        self.next_token();
        self.next_token();

        let result = if let Some(_) = self.parse_expression(Precedences::Lowest) {
            self.next_token();
            self.cur_token_is(TokenType::Rbracket) && self.peek_token_is(TokenType::Assign)
        } else {
            false
        };

        self.rewind_position();
        result
    }

    pub fn parse_assign_array_element(&mut self) -> Option<Statement> {
        let left = if let Some(expression) = self.parse_expression(Precedences::Lowest) {
            expression
        } else {
            unreachable!();
        };
        self.next_token();
        self.next_token();

        let right = if let Some(expression) = self.parse_expression(Precedences::Lowest) {
            expression
        } else {
            unreachable!();
        };

        match left {
            Expression::ArrayElement(ident, index_expression, _) => Some(
                Statement::AssignmentAggregate(ident, right, *index_expression),
            ),
            _ => unreachable!(),
        }
    }

    pub fn parse_assign_statement(&mut self) -> Option<Statement> {
        if let Some(token) = self.cur_token.to_owned() {
            let name = Identifier(token.value.to_owned());

            if self.expect_peek(TokenType::Assign) == false {
                return None;
            }

            self.next_token();
            let value = if let Some(value) = self.parse_expression(Precedences::Lowest) {
                value
            } else {
                return None;
            };

            while self.peek_token_is(TokenType::Semicolon) {
                self.next_token();
            }

            return Some(Statement::Assignment(name, value));
        }
        None
    }

    pub fn parse_let_statement(&mut self) -> Option<Statement> {
        if self.expect_peek(TokenType::Identifier) == false {
            return None;
        }

        if let Some(token) = self.cur_token.to_owned() {
            let name = Identifier(token.value.to_owned());
            if let Some(symbol_type) = self.extract_symbol_type(token) {
                if self.expect_peek(TokenType::Assign) == false {
                    return None;
                }

                self.next_token();
                let expression = if let Some(value) = self.parse_expression(Precedences::Lowest) {
                    value
                } else {
                    return None;
                };

                while self.peek_token_is(TokenType::Semicolon) {
                    self.next_token();
                }

                return Some(Statement::Let(name, expression, symbol_type));
            }
        }
        None
    }

    pub fn extract_symbol_type(&mut self, token: Token) -> Option<SymbolType> {
        if self.peek_token_is(TokenType::Colon) == false {
            return None;
        }
        self.next_token();

        if let Some(token) = self.peek_token.to_owned() {
            let base_type = match token.token_type {
                TokenType::Identifier => SymbolType::Custom(token.value),
                TokenType::PrimaryType(symbol_type) => symbol_type,
                _ => panic!("{}: failed to extract symbol type", token.value),
            };
            self.next_token();

            if self.peek_token_is(TokenType::Lbracket) == false {
                return Some(base_type);
            }
            self.next_token();

            if self.peek_token_is(TokenType::Rbracket) == false {
                return None;
            }
            self.next_token();

            Some(SymbolType::Array(Box::new(base_type)))
        } else {
            None
        }
    }

    pub fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();
        let return_value = if let Some(value) = self.parse_expression(Precedences::Lowest) {
            value
        } else {
            return None;
        };

        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Return(return_value));
    }

    pub fn parse_while_statement(&mut self) -> Option<Statement> {
        if self.expect_peek(TokenType::Lparen) == false {
            return None;
        }
        self.next_token();

        if let Some(condition) = self.parse_expression(Precedences::Lowest) {
            if self.expect_peek(TokenType::Rparen) == false {
                return None;
            }

            if self.expect_peek(TokenType::Lbrace) == false {
                return None;
            }

            if let Some(block) = self.parse_block_statement() {
                return Some(Statement::While(condition, block));
            }
        }
        None
    }

    pub fn parse_if_statement(&mut self) -> Option<Statement> {
        let if_row = self.lexer.current_row;
        let mut condtions = Vec::new();
        let mut bodies = Vec::new();
        let mut loop_flag = true;
        while loop_flag {
            if self.expect_peek(TokenType::Lparen) == false {
                return None;
            }
            self.next_token();

            if let Some(condition) = self.parse_expression(Precedences::Lowest) {
                condtions.push(condition);
            } else {
                return None;
            }

            if self.expect_peek(TokenType::Rparen) == false {
                return None;
            }
            if self.expect_peek(TokenType::Lbrace) == false {
                return None;
            }

            if let Some(body) = self.parse_block_statement() {
                bodies.push(body);
            } else {
                return None;
            }

            if self.peek_token_is(TokenType::ElseIf) {
                self.next_token();
            } else {
                loop_flag = false;
            }
        }

        if self.peek_token_is(TokenType::Else) {
            self.next_token();
            condtions.push(Expression::Boolean(true, Location::new(if_row)));
            if self.expect_peek(TokenType::Lbrace) == false {
                return None;
            }

            if let Some(alt) = self.parse_block_statement() {
                bodies.push(alt);
            } else {
                bodies.push(Vec::new());
            }
        } else {
            condtions.push(Expression::Boolean(false, Location::new(if_row)));
            bodies.push(Vec::new());
        };

        return Some(Statement::If {
            conditions: condtions,
            bodies: bodies,
            location: Location::new(if_row),
        });
    }

    pub fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut block = Vec::new();
        self.next_token();

        while self.cur_token_is(TokenType::Rbrace) == false && self.cur_token.is_none() == false {
            if let Some(stmt) = self.parse_statement() {
                block.push(stmt);
            }
            self.next_token();
        }
        return Some(block);
    }

    pub fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = if let Some(expression) = self.parse_expression(Precedences::Lowest) {
            expression
        } else {
            return None;
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Expression(expression));
    }

    pub fn parse_expression(&mut self, precedence: Precedences) -> Option<Expression> {
        let mut left_exp = self.parse_left_expression();
        let is_identifier = if let Some(exp) = &left_exp {
            match exp {
                Expression::Identifier(_, _) => true,
                _ => false,
            }
        } else {
            false
        };

        while self.peek_token_is(TokenType::Semicolon) == false
            && precedence < self.peek_precedence()
        {
            if let Some(token) = self.peek_token.to_owned() {
                left_exp = match token.token_type {
                    TokenType::Minus | TokenType::Plus => {
                        if is_identifier && self.try_parse_sufix() {
                            self.next_token();
                            self.parse_sufix(left_exp)
                        } else {
                            self.next_token();
                            self.parse_infix_expression(left_exp)
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
                        self.next_token();
                        self.parse_infix_expression(left_exp)
                    }
                    TokenType::Lparen => {
                        self.next_token();
                        self.parse_call_expression(left_exp)
                    }
                    _ => {
                        self.no_prefix_parse_fn_error(token);
                        return left_exp;
                    }
                };
            }
        }
        left_exp
    }

    pub fn parse_left_expression(&mut self) -> Option<Expression> {
        if let Some(token) = self.cur_token.to_owned() {
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
                    self.no_prefix_parse_fn_error(token);
                    None
                }
            }
        } else {
            None
        }
    }

    pub fn try_parse_sufix(&mut self) -> bool {
        self.save_rewind_position();
        let previous_token = if let Some(token) = self.peek_token.to_owned() {
            token
        } else {
            unreachable!()
        };
        self.next_token();

        let result = if let Some(token) = self.peek_token.to_owned() {
            token.token_type == previous_token.token_type
        } else {
            false
        };

        self.rewind_position();

        result
    }

    pub fn parse_sufix(&mut self, left: Option<Expression>) -> Option<Expression> {
        if left.is_none() {
            return None;
        }

        if let Some(token) = self.peek_token.to_owned() {
            let sufix_type = match token.token_type {
                TokenType::Minus => Sufix::Minus,
                TokenType::Plus => Sufix::Plus,
                _ => {
                    self.errors
                        .push(parse_sufix_error(token.token_type, token.current_row));
                    return None;
                }
            };
            self.next_token();

            return Some(Expression::Sufix(
                sufix_type,
                Box::new(left.unwrap()),
                Location::new(self.lexer.current_row),
            ));
        };

        None
    }

    pub fn parse_prefix_expression(&mut self) -> Option<Expression> {
        if let Some(token) = self.cur_token.to_owned() {
            self.next_token();
            if let Some(right) = self.parse_expression(Precedences::Prefix) {
                if let Some(prefix) = self.convert_token_to_prefix(token) {
                    return Some(Expression::Prefix(
                        prefix,
                        Box::new(right),
                        Location::new(self.lexer.current_row),
                    ));
                }
            }
        }
        None
    }

    pub fn parse_integer_literal(&mut self) -> Option<Expression> {
        if let Some(token) = &self.cur_token {
            if let Ok(value) = token.value.parse::<u64>() {
                return Some(Expression::IntegerLiteral(
                    value,
                    Location::new(self.lexer.current_row),
                ));
            } else {
                self.errors.push(format!(
                    "could not parse {} as integer. row: {}",
                    token.value, token.current_row
                ));
            }
        }
        None
    }

    pub fn parse_function_literal(&mut self) -> Option<Expression> {
        if self.expect_peek(TokenType::Lparen) == false {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if self.expect_peek(TokenType::Lbrace) == false {
            return None;
        }

        if let Some(body) = self.parse_block_statement() {
            return Some(Expression::Function {
                parameters: parameters,
                body: body,
                location: Location::new(self.lexer.current_row),
            });
        }
        None
    }

    pub fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut parameters = Vec::new();

        if self.peek_token_is(TokenType::Rparen) {
            self.next_token();
            return self.parser_return_type(parameters);
        }
        self.next_token();

        if let Some(token) = self.cur_token.to_owned() {
            parameters.push(Identifier(token.value.to_owned()));

            if self.expect_peek(TokenType::Colon) == false {
                self.emit_error_for_funciton();
            }

            if let Some(_token) = self.peek_token.to_owned() {
                self.next_token();
            }
        }

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            if let Some(token) = self.cur_token.to_owned() {
                parameters.push(Identifier(token.value.to_owned()));
            }

            if self.expect_peek(TokenType::Colon) == false {
                self.emit_error_for_funciton();
            }

            if let Some(_token) = self.peek_token.to_owned() {
                self.next_token();
            }
        }

        if self.expect_peek(TokenType::Rparen) == false {
            self.emit_error_for_funciton();
        }

        self.parser_return_type(parameters)
    }

    pub fn parser_return_type(&mut self, parameters: Vec<Identifier>) -> Vec<Identifier> {
        if self.expect_peek(TokenType::Colon) == false {
            self.emit_error_for_funciton();
        }

        if let Some(_token) = self.peek_token.to_owned() {
            self.next_token();
            return parameters;
        }

        self.emit_error_for_funciton();
        unreachable!();
    }

    pub fn parse_identifier(&mut self) -> Option<Expression> {
        if let Some(token) = self.cur_token.clone() {
            if self.peek_token_is(TokenType::Lbracket) {
                return self.parse_array_element(token);
            }

            return Some(Expression::Identifier(
                Identifier(token.value.to_owned()),
                Location::new(self.lexer.current_row),
            ));
        }
        None
    }

    pub fn parse_array_element(&mut self, token: Token) -> Option<Expression> {
        self.next_token();
        self.next_token();
        if let Some(index_expression) = self.parse_expression(Precedences::Lowest) {
            while self.cur_token_is(TokenType::Rbracket) == false {
                self.next_token();
            }

            return Some(Expression::ArrayElement(
                Identifier(token.value.to_owned()),
                Box::new(index_expression),
                Location::new(self.lexer.current_row),
            ));
        } else {
            return None;
        }
    }

    pub fn parse_array(&mut self) -> Option<Expression> {
        let mut elements: Vec<Expression> = Vec::new();

        if self.peek_token_is(TokenType::Rbracket) == true {
            return Some(Expression::Array(elements)); // TODO
        }
        self.next_token();

        if let Some(expression) = self.parse_expression(Precedences::Lowest) {
            elements.push(expression);
        }

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            if let Some(expression) = self.parse_expression(Precedences::Lowest) {
                elements.push(expression);
            }
        }

        if self.expect_peek(TokenType::Rbracket) == false {
            panic!("parse error."); // TODO
        }

        Some(Expression::Array(elements))
    }

    pub fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let exp = if let Some(ret) = self.parse_expression(Precedences::Lowest) {
            ret
        } else {
            return None;
        };

        if self.expect_peek(TokenType::Rparen) == false {
            return None;
        }
        Some(exp)
    }

    pub fn parse_string_literal(&mut self) -> Option<Expression> {
        if let Some(token) = &self.cur_token {
            return Some(Expression::StringLiteral(
                token.value.to_owned(),
                Location::new(self.lexer.current_row),
            ));
        }
        None
    }

    pub fn parse_boolean(&mut self) -> Option<Expression> {
        return Some(Expression::Boolean(
            self.cur_token_is(TokenType::True),
            Location::new(self.lexer.current_row),
        ));
    }

    pub fn parse_infix_expression(&mut self, left: Option<Expression>) -> Option<Expression> {
        if left.is_none() {
            return None;
        }

        if let Some(token) = self.cur_token.to_owned() {
            let precedence = self.cur_precedence();
            self.next_token();
            if let Some(right) = self.parse_expression(precedence) {
                if let Some(infix) = self.convert_token_to_infix(token.clone()) {
                    return Some(Expression::Infix(
                        infix,
                        Box::new(left.unwrap()),
                        Box::new(right),
                        Location::new(self.lexer.current_row),
                    ));
                } else {
                    self.errors.push(format!(
                        "{:?} {:?} {:?} cannot be parsed. row: {}",
                        left, token.token_type, right, token.current_row
                    ));
                }
            }
        }
        None
    }

    pub fn parse_call_expression(&mut self, function: Option<Expression>) -> Option<Expression> {
        if let Some(function) = function {
            let expr = Expression::Call(Call {
                function: Box::new(function),
                arguments: self.parse_call_arguments(),
                location: Location::new(self.lexer.current_row),
            });

            match expr.clone() {
                Expression::Function {
                    parameters: _,
                    body: _,
                    location: _,
                } => {
                    if let Some(token) = self.peek_token.to_owned() {
                        return match token.token_type {
                            TokenType::Lparen => {
                                self.next_token();
                                self.parse_call_expression(Some(expr))
                            }
                            _ => Some(expr),
                        };
                    }
                    None
                }
                _ => Some(expr),
            }
        } else {
            None
        }
    }

    pub fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        if self.peek_token_is(TokenType::Rparen) {
            self.next_token();
            return args;
        }
        self.next_token();

        if let Some(arg) = self.parse_expression(Precedences::Lowest) {
            args.push(arg);
        }

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            if let Some(arg) = self.parse_expression(Precedences::Lowest) {
                args.push(arg);
            }
        }

        if self.expect_peek(TokenType::Rparen) == false {
            return args;
        }
        args
    }

    pub fn convert_token_to_infix(&mut self, token: Token) -> Option<Infix> {
        match token.token_type {
            TokenType::Plus => Some(Infix::Plus),
            TokenType::Minus => Some(Infix::Minus),
            TokenType::Divide => Some(Infix::Divide),
            TokenType::Multiply => Some(Infix::Multiply),
            TokenType::Rem => Some(Infix::Rem),
            TokenType::Eq => Some(Infix::Eq),
            TokenType::NotEq => Some(Infix::NotEq),
            TokenType::Gte => Some(Infix::Gte),
            TokenType::Gt => Some(Infix::Gt),
            TokenType::Lte => Some(Infix::Lte),
            TokenType::Lt => Some(Infix::Lt),
            _ => {
                self.errors.push(format!(
                    "{:?} is not a token for infix. row: {}",
                    token.token_type, token.current_row
                ));
                None
            }
        }
    }

    pub fn convert_token_to_prefix(&mut self, token: Token) -> Option<Prefix> {
        match token.token_type {
            TokenType::Plus => Some(Prefix::Plus),
            TokenType::Minus => Some(Prefix::Minus),
            TokenType::Bang => Some(Prefix::Bang),
            _ => {
                self.errors.push(format!(
                    "{:?} is not a token for prefix. row: {}",
                    token.token_type, token.current_row
                ));
                None
            }
        }
    }

    pub fn cur_precedence(&self) -> Precedences {
        if let Some(token) = &self.cur_token {
            let token_type = &token.token_type;
            if PRECEDENCE_TOKEN_MAP.contains_key(token_type) {
                return PRECEDENCE_TOKEN_MAP[token_type].to_owned();
            }
        }
        Precedences::Lowest
    }

    pub fn cur_token_is(&self, token_type: TokenType) -> bool {
        if let Some(ref token) = &self.cur_token {
            return token.token_type == token_type;
        }
        false
    }

    pub fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            return true;
        } else {
            if let Some(token) = self.peek_token.clone() {
                self.peek_error(token);
            }
            return false;
        }
    }

    pub fn peek_token_is(&mut self, token_type: TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            return token.token_type == token_type;
        }
        false
    }

    pub fn peek_precedence(&mut self) -> Precedences {
        if let Some(token) = &self.peek_token {
            let token_type = &token.token_type;
            if PRECEDENCE_TOKEN_MAP.contains_key(token_type) {
                return PRECEDENCE_TOKEN_MAP[token_type].to_owned();
            }
        }
        Precedences::Lowest
    }

    pub fn has_error(&self) -> bool {
        self.errors.len() > 0
    }

    pub fn emit_error(&self) -> String {
        self.errors.join("\n")
    }

    pub fn emit_error_for_funciton(&mut self) {
        self.errors
            .push(format!("parse failed at row:{}", self.lexer.current_row));
        self.skip_until_semicolon();
    }

    pub fn no_prefix_parse_fn_error(&mut self, token: Token) {
        self.errors.push(format!(
            "no prefix parse function for {:?}. row: {}",
            token.token_type, token.current_row
        ));
        self.skip_until_semicolon();
    }

    pub fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "expected next token to be {:?} instead. row: {}",
            token.token_type, token.current_row
        ));
        self.skip_until_semicolon();
    }

    pub fn skip_until_semicolon(&mut self) {
        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }
    }
}

#[test]
fn let_statements() {
    let input = r#"
    let x: int = 5;
    let y: boolean = false;
    let z: string = "abc";
    let foobar: int = 939393;
  "#;
    let program = parse_input(input);
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
    let program = parse_input(input);
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
"#;
    let program = parse_input(input);
    statement_assert(&program[0], "while (true) { let i: int = (i + 1) }");
}

#[test]
fn assign_statements() {
    let input = r#"
    let x: int = 5;
    x = 10;
    x = 10 * 3;
  "#;
    let program = parse_input(input);
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
    let program = parse_input(input);
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
    let program = parse_input(input);
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
    let program = parse_input(input);
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
    let program = parse_input(input);
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
    let program = parse_input(input);
    statement_assert(&program[0], "[1, 2, 3]");
}

#[test]
fn array_element_parsing() {
    let input = r#"
    let a: int[] = [1, 2, 3];
    a[1];
"#;
    let program = parse_input(input);
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
    let program = parse_input(input);
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
    let program = parse_input(input);
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
    parse_and_emit_error(input, vec!["no prefix parse function for Gt. row: 2"]);
}

#[test]
fn sufix_parsing() {
    let input = r#"
    a++;
"#;
    let program = parse_input(input);
    statement_assert(&program[0], "(a++)");
}
