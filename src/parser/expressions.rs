use std::fmt;

use crate::parser::infix::*;
use crate::parser::prefix::*;
use crate::parser::statements::*;
use crate::parser::sufix::*;

#[derive(PartialEq, Clone, Debug)]
pub struct Identifier(pub String);

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Identifier(Identifier, Location),
    IntegerLiteral(u64, Location),
    StringLiteral(String, Location),
    Boolean(bool, Location),
    Array(Vec<Expression>),
    ArrayElement(Identifier, Box<Expression>, Location),
    Prefix(Prefix, Box<Expression>, Location),
    Infix(Infix, Box<Expression>, Box<Expression>, Location),
    Sufix(Sufix, Box<Expression>, Location),
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        location: Location,
    },
    Call(Call),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Location {
    pub row: usize,
}

impl Location {
    pub fn new(row: usize) -> Self {
        Location { row: row }
    }
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::Identifier(ident, _location) => ident.0.to_string(),
            Expression::IntegerLiteral(int, _location) => int.to_string(),
            Expression::StringLiteral(literal, _location) => {
                format!(r#""{}""#, literal.to_string())
            }
            Expression::Boolean(boolean, _location) => boolean.to_string(),
            Expression::Array(elements) => {
                let elements_string = elements
                    .iter()
                    .fold(Vec::new(), |mut stack, element| {
                        stack.push(element.string());
                        stack
                    })
                    .join(", ");

                format!("[{}]", elements_string)
            }
            Expression::ArrayElement(ident, index_expression, _) => {
                format!("{}[{}]", ident.0.to_string(), index_expression.string())
            }
            Expression::Prefix(prefix, expr, _location) => format!("({}{})", prefix, expr.string()),
            Expression::Infix(infix, left, right, _location) => {
                format!("({} {} {})", left.string(), infix, right.string())
            }
            Expression::Sufix(sufix, left, _location) => {
                format!("({}{})", left.string(), sufix)
            }
            Expression::Function {
                parameters,
                body,
                location: _,
            } => {
                let mut param_string = String::new();
                for (index, Identifier(ref string)) in parameters.iter().enumerate() {
                    if index == 0 {
                        param_string.push_str(&format!("{}", string));
                    } else {
                        param_string.push_str(&format!(", {}", string));
                    }
                }
                let mut ret_string = String::new();
                for (index, statement) in body.iter().enumerate() {
                    if index == 0 {
                        ret_string.push_str(&format!("{}", statement.string()));
                    } else {
                        ret_string.push_str(&format!(" {}", statement.string()));
                    }
                }

                format!("fn({}) {{ {} }}", param_string, ret_string)
            }
            Expression::Call(call) => {
                let mut ret_string = String::new();
                for (index, parameter) in call.arguments.iter().enumerate() {
                    if index == 0 {
                        ret_string.push_str(&format!("{}", &parameter.string()));
                    } else {
                        ret_string.push_str(&format!(", {}", &parameter.string()));
                    }
                }

                format!("{}({})", call.function.string(), ret_string)
            }
        }
    }
}
