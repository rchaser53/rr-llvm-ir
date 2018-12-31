use std::fmt;

use inkwell::support::LLVMString;
use inkwell::values::{ArrayValue, FunctionValue, IntValue};

#[derive(Debug)]
pub enum Object {
    Integer(IntValue, IntSize),
    Boolean(IntValue),
    String(LLVMString),
    Array(ArrayValue),
    Function(FunctionValue),
    Null,
    Error(String),
    BuildIn(BuildIn),
}

#[derive(Debug)]
pub enum IntSize {
    I1,
    I8,
    I32,
    I64,
}

#[derive(Debug)]
pub enum BuildIn {
    Printf,
    Length,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(int, _) => write!(f, "{}", int.print_to_string().to_string()),
            Object::Boolean(boolean) => write!(f, "{}", boolean.print_to_string().to_string()),
            Object::String(string) => write!(f, "{}", string.to_string()),
            Object::Array(array) => write!(f, "{}", array.print_to_string().to_string()),
            Object::Function(function) => write!(f, "{}", function.print_to_string().to_string()),
            Object::Null => write!(f, "Null"),
            Object::Error(string) => write!(f, "{}", string),
            Object::BuildIn(build_in) => match build_in {
                BuildIn::Printf => write!(f, "printf"),
                BuildIn::Length => write!(f, "length"),
            },
        }
    }
}

// #[derive(Debug, Clone)]
// pub struct Function {
//     pub llvm_value: *mut LLVMValue,
//     pub llvm_block: *mut LLVMBasicBlock,
//     pub return_type: LLVMExpressionType,
// }
