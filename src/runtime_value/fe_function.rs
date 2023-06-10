use super::*;

use crate::environment as env;
use crate::runtime_value as rt;

#[derive(Debug, Clone, PartialEq)]
pub struct FerrumFunction {
    pub decl: ast::FunctionDecl,
    pub closure: env::SharedEnvironment<rt::RuntimeValue>,
}

impl FerrumFunction {
    pub fn new(decl: ast::FunctionDecl, closure: env::SharedEnvironment<rt::RuntimeValue>) -> Self {
        return Self { decl, closure };
    }
}

impl FerrumCall for FerrumFunction {
    fn arity(&self) -> usize {
        return self.decl.params.len();
    }

    fn call(
        &mut self,
        interpreter: &mut interpreter::Interpreter,
        arguments: Vec<RuntimeValue>,
    ) -> RuntimeResult {
        let environment = self.closure.share().shared_enclosed();

        for i in 0..self.decl.params.len() {
            let param = &self.decl.params[i].name.lexeme;
            let arg = arguments[i].clone();

            environment.define(param.clone(), arg);
        }

        if let Err(e) = interpreter.execute_block(&mut self.decl.body, environment.share()) {
            match e {
                rt::RuntimeError::NonErrorReturnShortCircuit { value } => {
                    return Ok(value.expect("TODO"));
                }
                e => return Err(e),
            }
        }

        // TODO: Return empty grouping '()'
        return Ok(rt::RuntimeValue::Boolean(false));
    }

    fn to_string(&self) -> String {
        return self.decl.name.lexeme.clone();
    }
}
