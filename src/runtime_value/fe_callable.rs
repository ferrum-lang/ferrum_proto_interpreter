use super::*;

use interpreter::Interpreter;

pub trait FerrumCall {
    fn arity(&self) -> usize;

    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<RuntimeValue>,
    ) -> RuntimeResult;

    fn to_string(&self) -> String;
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerrumCallable {
    Struct(FerrumStruct),
    Function(FerrumFunction),
}

impl FerrumCall for FerrumCallable {
    fn arity(&self) -> usize {
        return match self {
            Self::Struct(struct_) => struct_.arity(),
            Self::Function(function) => function.arity(),
        };
    }

    fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<RuntimeValue>,
    ) -> RuntimeResult {
        return match self {
            Self::Struct(struct_) => struct_.call(interpreter, arguments),
            Self::Function(function) => function.call(interpreter, arguments),
        };
    }

    fn to_string(&self) -> String {
        return match self {
            Self::Function(function) => function.to_string(),
            Self::Struct(struct_) => struct_.to_string(),
        };
    }
}
