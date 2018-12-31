use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};

pub struct LLVMCreator {
    pub context: Context,
    pub builder: Builder,
    pub module: Module,
}

impl LLVMCreator {
    pub fn new(module_name: &str) -> LLVMCreator {
        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let context = Context::create();
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        LLVMCreator {
            builder: builder,
            module: module,
            context: context,
        }
    }

    #[allow(dead_code)]
    pub fn dump(&self) {
        &self.module.print_to_stderr();
    }

    #[allow(dead_code)]
    pub fn emit_file(&self, filename: &str) {
        let _ = self.module.verify().map_err(|err| {
            panic!(err.to_string());
        });
        let _ = self
            .module
            .print_to_file(Path::new(filename))
            .map_err(|err| {
                panic!(err.to_string());
            });
    }
}

#[cfg(test)]
mod tests {
    use inkwell::builder::Builder;
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::module::Module;

    use inkwell::targets::{InitializationConfig, Target};
    use inkwell::types::IntType;
    use inkwell::values::{InstructionValue, IntValue, PointerValue};
    use inkwell::OptimizationLevel;

    type SumFunc = unsafe extern "C" fn(u64, u64) -> u64;

    #[test]
    fn example_run_function() {
        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let context = Context::create();
        let module = context.create_module("test_module");
        let builder = context.create_builder();

        create_function(&context, &module, &builder);
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        unsafe {
            let funcion_in_rust: JitFunction<SumFunc> =
                execution_engine.get_function("sum").unwrap();
            assert!(3 == funcion_in_rust.call(1, 2), "test failed!");
        };
    }

    #[allow(dead_code)]
    fn create_function(context: &Context, module: &Module, builder: &Builder) {
        let i64_type = context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into()], false);

        let sum = module.add_function("sum", fn_type, None);
        let basic_block = context.append_basic_block(&sum, "entry");
        builder.position_at_end(&basic_block);

        let x = sum.get_nth_param(0).unwrap().into_int_value();
        let y = sum.get_nth_param(1).unwrap().into_int_value();

        let int_32_type: IntType = IntType::i32_type();
        let const_int: IntValue = int_32_type.const_int(10, false);
        let const_int_ref: PointerValue = builder.build_alloca(int_32_type, "int_value");
        let _: InstructionValue = builder.build_store(const_int_ref, const_int);

        builder.build_return(Some(&builder.build_int_add(x, y, "")));
    }

}
