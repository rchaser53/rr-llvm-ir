use std::collections::HashMap;
use std::rc::Rc;
use std::rc::Weak;

use crate::types::error::*;

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub enclosing_scope: Weak<SymbolTable>,
    pub symbols: HashMap<String, Symbol>,
    pub scope_name: String,
}

impl SymbolTable {
    pub fn new(name: &str, scope: Option<SymbolTable>) -> Self {
        let enclosing_scope = if let Some(actual_scope) = scope {
            let scope = Rc::new(actual_scope);
            Rc::downgrade(&scope)
        } else {
            Weak::new()
        };

        SymbolTable {
            symbols: SymbolTable::initialize(),
            enclosing_scope,
            scope_name: name.to_string(),
        }
    }

    pub fn initialize() -> HashMap<String, Symbol> {
        let mut symbols = HashMap::new();
        symbols.insert(
            String::from("int"),
            Symbol::new("int", SymbolType::Integer, true),
        );
        symbols.insert(
            String::from("float"),
            Symbol::new("float", SymbolType::Float, true),
        );
        symbols.insert(
            String::from("string"),
            Symbol::new("string", SymbolType::String, true),
        );
        symbols.insert(
            String::from("bool"),
            Symbol::new("bool", SymbolType::Boolean, true),
        );
        symbols.insert(
            String::from("void"),
            Symbol::new("void", SymbolType::Void, true),
        );

        symbols
    }

    pub fn resolve(&self, name: &str) -> Result<Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            return Ok(symbol.clone());
        };

        if let Some(upgraded) = self.enclosing_scope.upgrade() {
            upgraded.resolve(name)
        } else {
            Err(SymbolError::UndefinedSymbol(name.to_string()))
        }
    }

    pub fn define(&mut self, key: &str, symbol: Symbol) -> Result<Symbol> {
        self.symbols
            .insert(key.to_string(), symbol)
            .ok_or(SymbolError::AlreadyUsedSymbol(key.to_string()))
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub is_build_in: bool,
}

impl Symbol {
    pub fn new(name: &str, symbol_type: SymbolType, is_build_in: bool) -> Self {
        Symbol {
            name: name.to_string(),
            symbol_type,
            is_build_in,
        }
    }

    pub fn string(&self) -> String {
        let name = self.name.to_owned();
        match &self.symbol_type {
            SymbolType::Integer => format!("{}: int", name),
            SymbolType::Float => format!("{}: float", name),
            SymbolType::String => format!("{}: string", name),
            SymbolType::Boolean => format!("{}: boolean", name),
            SymbolType::Void => format!("{}: void", name),
            SymbolType::Null => format!("{}: null", name),
            SymbolType::Array(boxed_type) => format!("{}: {}[]", name, boxed_type.string()),
            SymbolType::Function(args, return_type) => {
                let arg_types_string: String = args
                    .iter()
                    .map(|arg| arg.string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "{}: function({}): {}",
                    name,
                    arg_types_string,
                    return_type.string()
                )
            }
            SymbolType::Custom(custom_name) => format!("{}: {}", name, custom_name),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SymbolType {
    Integer,
    Float,
    String,
    Boolean,
    Void,
    Null,
    Array(Box<SymbolType>),
    Function(Vec<Box<SymbolType>>, Box<SymbolType>),
    Custom(String),
}

impl SymbolType {
    pub fn string(&self) -> String {
        match self {
            SymbolType::Integer => String::from("int"),
            SymbolType::Float => String::from("float"),
            SymbolType::String => String::from("string"),
            SymbolType::Boolean => String::from("boolean"),
            SymbolType::Void => String::from("void"),
            SymbolType::Null => String::from("null"),
            SymbolType::Array(boxed_type) => format!("{}[]", boxed_type.string()),
            SymbolType::Function(args, return_type) => {
                let arg_types_string: String = args
                    .iter()
                    .map(|arg| arg.string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("function({}): {}", arg_types_string, return_type.string())
            }
            SymbolType::Custom(name) => name.to_string(),
        }
    }
}
