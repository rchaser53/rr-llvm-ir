use crate::parser::infix::*;
use crate::parser::prefix::*;
use crate::parser::statements::*;
use crate::parser::sufix::*;

use crate::types::symbol::*;

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
        parameter_symbols: Vec<Box<Symbol>>,
        body: BlockStatement,
        return_type: SymbolType,
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
            Expression::Sufix(sufix, left, _location) => format!("({}{})", left.string(), sufix),
            Expression::Function {
                parameter_symbols,
                body,
                ..
            } => {
                let mut param_string = String::new();
                for (index, symbol) in parameter_symbols.iter().enumerate() {
                    if index == 0 {
                        param_string.push_str(&format!("{}", symbol.name));
                    } else {
                        param_string.push_str(&format!(", {}", symbol.name));
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
