use crate::types::error::*;
use crate::types::symbol::*;

use crate::parser::expressions::*;
use crate::parser::prefix::*;
use crate::parser::statements::*;

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
            Statement::Let(ident, expr, symbol_type) => self.walk_let(ident, expr, symbol_type),
            _ => {}
        }
    }

    pub fn walk_let(&mut self, ident: Identifier, expr: Expression, left: SymbolType) {
        let ident_name = ident.0.as_ref();
        match self.resolve_expr_type(expr, left) {
            Ok(symbol_type) => {
                if let Some(table) = self.symbol_tables.last_mut() {
                    let _ = table
                        .define(ident_name, Symbol::new(ident_name, symbol_type, false))
                        .map_err(|err| {
                            self.error_stack.push(format!("{}", err));
                        });
                }
            }
            Err(err) => {
                self.error_stack.push(format!("{}", err));
            }
        };
    }

    pub fn resolve_expr_type(&mut self, expr: Expression, left: SymbolType) -> Result<SymbolType> {
        match expr {
            Expression::Identifier(Identifier(string), _) => {
                self.resolve_current_ident(string, left)
            }
            Expression::IntegerLiteral(_, _) => {
                self.match_symbol_type(left, SymbolType::Primary(PrimaryType::Integer))
            }
            Expression::StringLiteral(_, _) => {
                self.match_symbol_type(left, SymbolType::Primary(PrimaryType::String))
            }
            Expression::Boolean(_, _) => {
                self.match_symbol_type(left, SymbolType::Primary(PrimaryType::Boolean))
            }
            Expression::Prefix(prefix, boxed_exp, _) => {
                self.resolve_prefix_expr_type(prefix, *boxed_exp, left)
            }
            Expression::Infix(_, boxed_exp, _, _) => self.resolve_expr_type(*boxed_exp, left),
            Expression::Sufix(_, boxed_exp, _) => self.resolve_expr_type(*boxed_exp, left),
            Expression::Function {
                parameter_symbols,
                body,
                return_type,
                ..
            } => self.resolve_function(parameter_symbols, body, return_type, left),
            _ => unreachable!(),
        }
    }

    pub fn resolve_current_ident(&mut self, ident: String, left: SymbolType) -> Result<SymbolType> {
        let symbol_table = self.symbol_tables.last_mut().unwrap();
        match symbol_table.resolve(&ident) {
            Ok(symbol) => self.match_symbol_type(left, symbol.symbol_type),
            Err(_) => Err(SymbolError::UndefinedSymbol(ident)),
        }
    }

    pub fn resolve_prefix_expr_type(
        &mut self,
        prefix: Prefix,
        expr: Expression,
        left: SymbolType,
    ) -> Result<SymbolType> {
        match prefix {
            Prefix::Bang => Ok(SymbolType::Primary(PrimaryType::Boolean)),
            _ => self.resolve_expr_type(expr, left),
        }
    }

    pub fn resolve_function(
        &mut self,
        right_parameters: Vec<Box<Symbol>>,
        body: Vec<Statement>,
        right_return_type: SymbolType,
        left: SymbolType,
    ) -> Result<SymbolType> {
        if let Some(last_table) = self.symbol_tables.last() {
            let new_scope = SymbolTable::new("test", Some(last_table.clone()));
            self.symbol_tables.push(new_scope);
            self.walk(body);

            let validaton_result = if let SymbolType::Function(ref function_type) = left {
                if let FunctionType::Declare(left_parameter_types, left_return_type) =
                    function_type.clone()
                {
                    let right_parameter_types: Vec<Box<SymbolType>> = right_parameters
                        .clone()
                        .into_iter()
                        .map(|parameter| Box::new(parameter.symbol_type))
                        .collect::<Vec<_>>();

                    left_parameter_types == right_parameter_types
                        && *left_return_type == right_return_type
                } else {
                    false
                }
            } else {
                false
            };

            let right = SymbolType::Function(FunctionType::Definition(
                right_parameters,
                Box::new(right_return_type),
                Box::new(self.symbol_tables.pop().unwrap()),
            ));

            if validaton_result == false {
                return Err(SymbolError::NotEqualSymbolType(
                    left.string(),
                    right.string(),
                ));
            }

            return Ok(right);
        }
        unreachable!();
    }

    pub fn match_symbol_type(&self, left: SymbolType, right: SymbolType) -> Result<SymbolType> {
        if left == right {
            Ok(left)
        } else {
            Err(SymbolError::NotEqualSymbolType(
                left.string(),
                right.string(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::*;
    use crate::types::walker::*;

    pub fn walk_ast(input: &str) -> Walker {
        let statements = parse_input(input).unwrap();
        let mut walker = Walker::new();
        walker.walk(statements);
        walker
    }

    pub fn assert_symbol_tables(symbol: &Symbol, expected: &str) {
        let actual = symbol.string();
        assert_eq!(actual, expected);
    }

    #[test]
    fn let_int() {
        let input = r#"
    let a: int = 1;
  "#;
        let tables = walk_ast(input).symbol_tables;
        assert_symbol_tables(&tables[0].resolve("a").unwrap(), "a: int");
    }

    #[test]
    fn use_identfier() {
        let input = r#"
    let a: int = 1;
    let b: int = a;
  "#;
        let tables = walk_ast(input).symbol_tables;
        assert_symbol_tables(&tables[0].resolve("a").unwrap(), "a: int");
        assert_symbol_tables(&tables[0].resolve("b").unwrap(), "b: int");
    }

    #[test]
    fn already_used_identify() {
        let input = r#"
    let a: int = 1;
    let a: int = 1;
  "#;
        let walker = walk_ast(input);
        assert_eq!(
            &walker.error_stack.join(""),
            &format!("{}", SymbolError::AlreadyUsedSymbol(String::from("a")))
        );
    }

    #[test]
    fn not_equal_symbol_type() {
        let input = r#"
    let a: int = "abc";
  "#;
        let walker = walk_ast(input);
        assert_eq!(
            &walker.error_stack.join(""),
            &format!(
                "{}",
                SymbolError::NotEqualSymbolType(String::from("int"), String::from("string"))
            )
        );
    }

    #[test]
    fn symbol_type_function() {
        let input = r#"
    let a: fn(int, string, bool):int = fn(a: int, b: string, c: bool): int {
      return 3;
    };
  "#;
        let tables = walk_ast(input).symbol_tables;
        assert_symbol_tables(
            &tables[0].resolve("a").unwrap(),
            "a: fn(a: int, b: string, c: bool): int",
        );
    }

    #[test]
    fn not_equal_symbol_type_function() {
        let input = r#"
    let a: fn():int = fn(): bool {
      return 3;
    };
  "#;
        let walker = walk_ast(input);
        assert_eq!(
            &walker.error_stack.join(""),
            &format!(
                "{}",
                SymbolError::NotEqualSymbolType(
                    String::from("fn(): int"),
                    String::from("fn(): bool")
                )
            )
        );
    }
}
