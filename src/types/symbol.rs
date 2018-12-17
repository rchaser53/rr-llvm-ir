use linked_hash_map::LinkedHashMap;

use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub enclosing_scope: Box<Option<SymbolTable>>,
    pub symbols: LinkedHashMap<String, Symbol>,
    pub scope_name: String,
}

impl SymbolTable {
    pub fn new(name: &str, scope: Option<SymbolTable>) -> Self {
        SymbolTable {
            symbols: SymbolTable::initialize(),
            enclosing_scope: Box::new(scope),
            scope_name: name.to_string(),
        }
    }

    pub fn initialize() -> LinkedHashMap<String, Symbol> {
        let mut symbols = LinkedHashMap::new();
        symbols.insert(
            String::from("int"),
            Symbol::new("int", SymbolType::Int, true),
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

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            return Some(symbol.clone());
        };

        if let Some(mut scope) = *(self.enclosing_scope.clone()) {
            return scope.resolve(name);
        } else {
            return None;
        }
    }

    pub fn define(&mut self, key: &str, symbol: Symbol) -> Option<Symbol> {
        self.symbols.insert(key.to_string(), symbol)
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
        format!("<{}: {:?}>", self.name, self.symbol_type)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SymbolType {
    Int,
    Float,
    String,
    Boolean,
    Void,
}
