use super::*;

use crate::runtime_value as rt;

#[derive(Debug, Clone, PartialEq)]
pub struct FerrumStruct {
    pub name: String,
}

impl FerrumStruct {
    pub fn new(name: String) -> Self {
        return Self { name };
    }
}

impl FerrumCall for FerrumStruct {
    fn arity(&self) -> usize {
        return 0;
    }

    fn call(
        &mut self,
        interpreter: &mut interpreter::Interpreter,
        arguments: Vec<RuntimeValue>,
    ) -> RuntimeResult {
        let instance = FerrumInstance::new(self.clone());

        return Ok(rt::RuntimeValue::Instance(instance));
    }

    fn to_string(&self) -> String {
        return self.name.clone();
    }
}
