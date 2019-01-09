use std::cell::RefCell;
use std::rc::Rc;

use inkwell::builder::Builder;
use inkwell::context::Context;

use crate::evaluate::creator::LLVMCreator;
use crate::evaluate::object::*;

use crate::parser::statements::*;

use crate::types::symbol::*;

pub struct Eval {
    pub creator: LLVMCreator,
    pub error_stack: Vec<String>,
    pub symbol_tables: Vec<Rc<RefCell<SymbolTable>>>,
}

#[allow(dead_code)]
impl Eval {
    pub fn new(name: &str, symbol_tables: Vec<Rc<RefCell<SymbolTable>>>) -> Self {
        let creator = LLVMCreator::new(name);

        Eval {
            creator,
            error_stack: Vec::new(),
            symbol_tables,
        }
    }

    pub fn entry_eval_program(&mut self, program: Program) -> Object {
        let i64_type = self.creator.context.i64_type();
        for statement in program.into_iter() {
            if let Some(obj) = self.eval_statement(statement) {
                build_return(&obj, &self.creator.context, &self.creator.builder);
                return obj;
            }
        }
        self.creator
            .builder
            .build_return(Some(&i64_type.const_int(1, false)));
        Object::Null
    }

    pub fn eval_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            // Statement::Let(ident, expr_type, expr) => {
            //     let obj = self.eval_let_statement(ident, expr_type, expr);
            //     let _ = self.accumultae_error(obj);
            //     None
            // },
            _ => unimplemented!(),
        }
    }

    // pub fn eval_let_statement(
    //     &mut self,
    //     ident: Identifier,
    //     expr_type: LLVMExpressionType,
    //     expr: Expression,
    //     env: &mut Environment,
    // ) -> Object {
    //     let mut object = self.eval_expression(expr, env);
    //     // let llvm_type = convert_llvm_type(expr_type.clone());
    //     let llvm_value = unwrap_object(&mut object);

    //     match expr_type {
    //         LLVMExpressionType::Function
    //         | LLVMExpressionType::Array(_, _)
    //         | LLVMExpressionType::String(_) => env.set(ident.0, object),
    //         LLVMExpressionType::Call => match object {
    //             Object::Integer(value) | Object::String(value, _) | Object::Boolean(value) => {
    //                 self.set_value_to_identify(value, object, &ident.0, env)
    //             }
    //             _ => env.set(ident.0, object),
    //         },
    //         _ => self.set_value_to_identify(llvm_value, object, &ident.0, env),
    //     }
    // }

    pub fn accumultae_error(&mut self, obj: Object) -> Option<Object> {
        match obj {
            Object::Error(_) => {
                self.error_stack.push(format!("{}", obj)); // TODO
                None
            }
            _ => Some(obj),
        }
    }

    pub fn emit_error(&mut self) -> String {
        let mut error_message = String::new();
        for (index, err_obj) in self.error_stack.iter().enumerate() {
            if index == 0 {
                error_message = format!("{}", err_obj);
            } else {
                error_message = format!("{}\n{}", error_message, err_obj);
            }
        }
        error_message.to_string()
    }

    pub fn has_error(&self) -> bool {
        self.error_stack.len() > 0
    }
}

// pub fn eval_program(&mut self, program: Program) -> Object {
//     for statement in program.into_iter() {
//         if let Some(mut obj) = self.eval_statement(statement) {
//             let llvm_value = unwrap_object(&mut obj);
//             build_ret(self.lc.builder, llvm_value);
//             return obj;
//         }
//     }
//     Object::Null
// }

// pub fn eval_let_statement(
//     &mut self,
//     ident: Identifier,
//     expr_type: LLVMExpressionType,
//     expr: Expression,
//     env: &mut Environment,
// ) -> Object {
//     let mut object = self.eval_expression(expr, env);
//     // let llvm_type = convert_llvm_type(expr_type.clone());
//     let llvm_value = unwrap_object(&mut object);

//     match expr_type {
//         LLVMExpressionType::Function
//         | LLVMExpressionType::Array(_, _)
//         | LLVMExpressionType::String(_) => env.set(ident.0, object),
//         LLVMExpressionType::Call => match object {
//             Object::Integer(value) | Object::String(value, _) | Object::Boolean(value) => {
//                 self.set_value_to_identify(value, object, &ident.0, env)
//             }
//             _ => env.set(ident.0, object),
//         },
//         _ => self.set_value_to_identify(llvm_value, object, &ident.0, env),
//     }
// }

// pub fn dump_llvm(&mut self) {
//     self.lc.dump();
//     validate_module(self.lc.module);
// }

// pub fn emit_llvm(&mut self, file_name: &str) {
//     self.lc.emit_file(file_name);
// }

pub fn build_return(obj: &Object, context: &Context, builder: &Builder) {
    match obj {
        Object::Integer(int_value, int_size) => {
            let int_type = match int_size {
                IntSize::I1 => context.bool_type(),
                IntSize::I8 => context.i8_type(),
                IntSize::I32 => context.i32_type(),
                IntSize::I64 => context.i64_type(),
            };

            let return_val = if let Some(val) = int_value.get_sign_extended_constant() {
                val as u64
            } else {
                0
            };

            builder.build_return(Some(&int_type.const_int(return_val, false)));
        }
        _ => unimplemented!(),
    }
}
