use crate::parser::expressions::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
    While(Expression, BlockStatement),
    Assignment(Identifier, Expression),
    AssignmentAggregate(Identifier, Expression, Expression),
}

pub type BlockStatement = Vec<Statement>;
pub type Program = BlockStatement;

impl Statement {
    #[allow(dead_code)]
    pub fn emit_debug_info(&self) -> String {
        match self {
            Statement::Let(Identifier(ref string), expr) => {
                format!("[ identifiy: {}, expression: {} ]", string, expr.string()).to_string()
            }
            Statement::Return(expr) => format!("[ expression: {} ]", expr.string()).to_string(),
            Statement::Expression(expr) => format!("[ expression: {} ]", expr.string()).to_string(),
            Statement::While(expr, body) => {
                let mut ret_string = String::new();
                for (index, statement) in body.iter().enumerate() {
                    if index == 0 {
                        ret_string.push_str(&format!("{}", statement.string()));
                    } else {
                        ret_string.push_str(&format!(" {}", statement.string()));
                    }
                }
                format!("[ expression: {}, block: {} ]", expr.string(), ret_string).to_string()
            }
            Statement::Assignment(Identifier(ref string), expr) => {
                format!("[ identifiy: {}, expression: {} ]", string, expr.string()).to_string()
            }
            Statement::AssignmentAggregate(Identifier(ref string), assign_expr, index_expr) => {
                format!(
                    "[ identifiy: {}, expression: {}, idnex: {} ]",
                    string,
                    assign_expr.string(),
                    index_expr.string()
                )
                .to_string()
            }
        }
    }

    pub fn string(&self) -> String {
        match self {
            Statement::Let(Identifier(ref string), expr) => {
                format!("let {} = {}", string, &expr.string())
            }
            Statement::Return(expr) => ("return ".to_owned() + &expr.string()).to_string(),
            Statement::Expression(expr) => expr.string(),
            Statement::While(expr, body) => {
                let mut ret_string = String::new();
                for (index, statement) in body.iter().enumerate() {
                    if index == 0 {
                        ret_string.push_str(&format!("{}", statement.string()));
                    } else {
                        ret_string.push_str(&format!(" {}", statement.string()));
                    }
                }
                format!("while ({}) {{ {} }}", expr.string(), ret_string)
            }
            Statement::Assignment(Identifier(ref string), expr) => {
                format!("{} = {}", string, &expr.string())
            }
            Statement::AssignmentAggregate(Identifier(ref string), assign_expr, index_expr) => {
                format!(
                    "{}[{}] = {}",
                    string,
                    &index_expr.string(),
                    &assign_expr.string()
                )
            }
        }
    }
}
