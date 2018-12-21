use crate::types::symbol::*;

use crate::parser::expressions::*;
use crate::parser::prefix::*;
use crate::parser::statements::*;
use crate::parser::test_utils::*;

#[derive(Debug)]
pub struct Walker {
    pub symbol_tables: Vec<SymbolTable>,
    pub error_stack: Vec<String>,
}

impl Walker {
    pub fn new() -> Walker {
        Walker {
            symbol_tables: vec![SymbolTable::new("global", None)],
            error_stack: Vec::new(),
        }
    }

    pub fn walk(&mut self, statements: Vec<Statement>) {
        for statement in statements.into_iter() {
            self.walk_statement(statement);
        }
    }

    pub fn walk_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Let(ident, expr, _) => self.walk_let(ident, expr),
            _ => {}
        }
    }

    pub fn walk_let(&mut self, ident: Identifier, expr: Expression) {
        let ident_name = ident.0.as_ref();
        let symbol_type = self.resolve_expr_type(expr);
        if let Some(table) = self.symbol_tables.last_mut() {
            table.define(ident_name, Symbol::new(ident_name, symbol_type, false));
        }
    }

    pub fn resolve_expr_type(&self, expr: Expression) -> SymbolType {
        match expr {
            Expression::IntegerLiteral(_, _) => SymbolType::Integer,
            Expression::StringLiteral(_, _) => SymbolType::String,
            Expression::Boolean(_, _) => SymbolType::Boolean,
            Expression::Prefix(prefix, boxed_exp, _) => {
                self.resolve_prefix_expr_type(prefix, *boxed_exp)
            }
            Expression::Infix(_, boxed_exp, _, _) => self.resolve_expr_type(*boxed_exp),
            Expression::Sufix(_, boxed_exp, _) => self.resolve_expr_type(*boxed_exp),
            _ => unreachable!(),
        }
    }

    pub fn resolve_prefix_expr_type(&self, prefix: Prefix, expr: Expression) -> SymbolType {
        match prefix {
            Prefix::Bang => SymbolType::Boolean,
            _ => self.resolve_expr_type(expr),
        }
    }
}

#[allow(dead_code)]
pub fn walk_ast(input: &str) -> Vec<SymbolTable> {
    let statements = parse_input(input).unwrap();
    let mut walker = Walker::new();
    walker.walk(statements);
    walker.symbol_tables
}

pub fn assert_symbol_tables(symbol: &Symbol, expected: &str) {
    let actual = symbol.string();
    assert_eq!(
        actual, expected
    );
}

#[test]
fn let_int() {
    let input = r#"
  let a: int = 1;
"#;
    let mut tables = walk_ast(input);
    assert_symbol_tables(&tables[0].resolve("a").unwrap(), "a: int");
}
