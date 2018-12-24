use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::rc::Weak;

use crate::types::error::*;

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub enclosing_scope: Weak<SymbolTable>,
    pub symbols: HashMap<String, Symbol>,
    pub scope_name: String,
}

impl PartialEq for SymbolTable {
    fn eq(&self, other: &SymbolTable) -> bool {
        self.scope_name == other.scope_name // TBD: need to fix
    }
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
            Symbol::new("int", SymbolType::Primary(PrimaryType::Integer), true),
        );
        symbols.insert(
            String::from("float"),
            Symbol::new("float", SymbolType::Primary(PrimaryType::Float), true),
        );
        symbols.insert(
            String::from("string"),
            Symbol::new("string", SymbolType::Primary(PrimaryType::String), true),
        );
        symbols.insert(
            String::from("bool"),
            Symbol::new("bool", SymbolType::Primary(PrimaryType::Boolean), true),
        );
        symbols.insert(
            String::from("void"),
            Symbol::new("void", SymbolType::Primary(PrimaryType::Void), true),
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

    pub fn define(&mut self, key: &str, symbol: Symbol) -> Result<()> {
        match self.symbols.insert(key.to_string(), symbol) {
            Some(_) => Err(SymbolError::AlreadyUsedSymbol(key.to_string())),
            None => Ok(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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
            SymbolType::Primary(PrimaryType::Integer) => format!("{}: int", name),
            SymbolType::Primary(PrimaryType::Float) => format!("{}: float", name),
            SymbolType::Primary(PrimaryType::String) => format!("{}: string", name),
            SymbolType::Primary(PrimaryType::Boolean) => format!("{}: boolean", name),
            SymbolType::Primary(PrimaryType::Void) => format!("{}: void", name),
            SymbolType::Primary(PrimaryType::Null) => format!("{}: null", name),
            SymbolType::Array(boxed_type) => format!("{}: {}[]", name, boxed_type.string()),
            SymbolType::Function(function_type) => {
                let function_string = match function_type {
                    FunctionType::Declare(args, return_type) => {
                        create_function_string(args, return_type)
                    }
                    FunctionType::Definition(args, return_type, _) => {
                        create_function_string(args, return_type)
                    }
                };
                format!("{}: {}", name, function_string)
            }
            SymbolType::Scope(scope) => format!("scope: {}", scope.scope_name),
            SymbolType::Custom(custom_name) => format!("{}: {}", name, custom_name),
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolType {
    Primary(PrimaryType),
    Array(Box<SymbolType>),
    Function(FunctionType),
    Scope(Box<SymbolTable>),
    Custom(String),
}

#[derive(Clone, Eq, Hash, Debug, PartialEq)]
pub enum PrimaryType {
    Integer,
    Float,
    String,
    Boolean,
    Void,
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionType {
    Declare(Vec<Box<SymbolType>>, Box<SymbolType>),
    Definition(Vec<Box<Symbol>>, Box<SymbolType>, Box<SymbolTable>),
}

impl SymbolType {
    pub fn string(&self) -> String {
        match self {
            SymbolType::Primary(PrimaryType::Integer) => String::from("int"),
            SymbolType::Primary(PrimaryType::Float) => String::from("float"),
            SymbolType::Primary(PrimaryType::String) => String::from("string"),
            SymbolType::Primary(PrimaryType::Boolean) => String::from("boolean"),
            SymbolType::Primary(PrimaryType::Void) => String::from("void"),
            SymbolType::Primary(PrimaryType::Null) => String::from("null"),
            SymbolType::Array(boxed_type) => format!("{}[]", boxed_type.string()),
            SymbolType::Function(function_type) => match function_type {
                FunctionType::Declare(args, return_type) => {
                    create_function_string(args, return_type)
                }
                FunctionType::Definition(args, return_type, _) => {
                    create_function_string(args, return_type)
                }
            },
            SymbolType::Scope(scope) => scope.scope_name.to_string(),
            SymbolType::Custom(name) => name.to_string(),
        }
    }
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string())
    }
}

fn create_function_string<T: fmt::Display>(args: &Vec<T>, return_type: &Box<SymbolType>) -> String {
    let arg_types_string: String = args
        .iter()
        .map(|arg| format!("{}", arg))
        .collect::<Vec<String>>()
        .join(", ");
    format!("fn({}): {}", arg_types_string, return_type.string())
}
