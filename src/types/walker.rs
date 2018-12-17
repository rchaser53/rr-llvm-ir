use crate::types::symbol::*;

use crate::parser::expressions::*;
use crate::parser::statements::*;

#[derive(Debug)]
pub struct Walker {
    pub symbol_table: SymbolTable,
}

impl Walker {
    pub fn new() -> Walker {
        Walker {
            symbol_table: SymbolTable::new("global", None),
        }
    }

    pub fn walk(&mut self, statements: Vec<Statement>) {
        for statement in statements.into_iter() {
            self.walk_statement(statement);
        }
    }

    pub fn walk_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Let(ident, expr) => self.walk_let(ident, expr),
            _ => {}
        }
    }

    pub fn walk_let(&mut self, ident: Identifier, expr: Expression) {
        let ident_name = ident.0.as_ref();
        let symbol_type = self.resolve_expr_type(expr);
        self.symbol_table
            .define(ident_name, Symbol::new(ident_name, symbol_type, false));
    }

    pub fn resolve_expr_type(&self, expr: Expression) -> SymbolType {
        match expr {
            Expression::IntegerLiteral(_, _) => SymbolType::Int,
            Expression::StringLiteral(_, _) => SymbolType::String,
            Expression::Boolean(_, _) => SymbolType::Boolean,
            _ => unreachable!(),
        }
    }
}
