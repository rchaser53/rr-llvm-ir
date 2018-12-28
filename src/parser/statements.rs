use crate::parser::expressions::*;
use crate::types::symbol::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Let(Identifier, Expression, SymbolType),
    Return(Expression),
    Expression(Expression),
    If(Vec<Expression>, Vec<BlockStatement>, Location),
    While(Expression, BlockStatement),
    Assignment(Identifier, Expression),
    AssignmentAggregate(Identifier, Expression, Expression),
    Scope(Vec<Box<Statement>>),
}

pub type BlockStatement = Vec<Statement>;
pub type Program = BlockStatement;

impl Statement {
    pub fn string(&self) -> String {
        match self {
            Statement::Let(Identifier(ref string), expr, symbol_type) => format!(
                "let {}: {} = {}",
                string,
                symbol_type.string(),
                &expr.string()
            ),
            Statement::Return(expr) => ("return ".to_owned() + &expr.string()).to_string(),
            Statement::Expression(expr) => expr.string(),
            Statement::If(conditions, bodies, _) => {
                let condition_strings = conditions.iter().map(|s| s.string()).collect::<Vec<_>>();

                let body_strings = bodies.iter().fold(Vec::new(), |mut stack, body| {
                    let body_string = body
                        .iter()
                        .map(|s| s.string())
                        .collect::<Vec<_>>()
                        .join("\n");
                    stack.push(body_string);
                    stack
                });

                let mut ret_string = String::new();
                for (index, condition_string) in condition_strings.iter().enumerate() {
                    if index == 0 {
                        ret_string.push_str(&format!(
                            "if({}) {{ {} }} ",
                            condition_string, body_strings[index]
                        ));
                    } else {
                        ret_string.push_str(&format!(
                            "elseif({}) {{ {} }}",
                            condition_string, body_strings[index]
                        ));
                    }
                }

                format!("{}", ret_string)
            }
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
            Statement::Scope(boxed_statements) => {
                let mut ret_string = String::new();
                for (index, boxed) in boxed_statements.iter().enumerate() {
                    if index == 0 {
                        ret_string.push_str(&format!("{}", boxed.string()));
                    } else {
                        ret_string.push_str(&format!(" {}", boxed.string()));
                    }
                }
                format!("{{ {} }}", ret_string)
            }
        }
    }
}
