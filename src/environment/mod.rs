use super::*;

use resolver;
use runtime_value as rt;
use token;

use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct SharedEnvironment {
    pub inner: Rc<RefCell<Environment>>,
}

#[derive(Debug, PartialEq)]
pub struct Environment {
    enclosing: Option<SharedEnvironment>,
    values: HashMap<String, rt::RuntimeValue>,
}

impl SharedEnvironment {
    pub fn new() -> Self {
        return Self {
            inner: Rc::new(RefCell::new(Environment::new())),
        };
    }

    pub fn share(&self) -> Self {
        return Self {
            inner: Rc::clone(&self.inner),
        };
    }

    pub fn enclosed(self) -> Environment {
        return Environment {
            enclosing: Some(self),
            values: HashMap::new(),
        };
    }

    pub fn shared_enclosed(self) -> Self {
        return Self {
            inner: Rc::new(RefCell::new(self.enclosed())),
        };
    }

    pub fn define(&self, name: String, value: rt::RuntimeValue) {
        self.inner.borrow_mut().define(name, value);
    }

    pub fn get_at(&self, distance: resolver::Distance, name: &token::Token) -> rt::RuntimeResult {
        return self
            .share()
            .ancestor(distance)
            .inner
            .borrow()
            .values
            .get(&name.lexeme)
            .cloned()
            .ok_or_else(|| rt::RuntimeError::UndefinedVariable {
                name: name.clone(),
                details: None,
            });
    }

    pub fn get(&self, name: &token::Token) -> rt::RuntimeResult {
        return self.inner.borrow().get(name);
    }

    fn ancestor(self, distance: resolver::Distance) -> Self {
        let mut env = self;

        for _ in 0..distance {
            let cloned = SharedEnvironment {
                inner: Rc::clone(
                    &env.inner
                        .borrow()
                        .enclosing
                        .as_ref()
                        .expect("Resolver shouldn't pass invalid distance")
                        .inner,
                ),
            };

            env = cloned;
        }

        return env;
    }
}

impl Environment {
    pub fn new() -> Self {
        return Self {
            enclosing: None,
            values: HashMap::new(),
        };
    }

    pub fn define(&mut self, name: String, value: rt::RuntimeValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &token::Token) -> rt::RuntimeResult {
        if let Some(value) = self.values.get(&name.lexeme) {
            return Ok(value.clone());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.inner.borrow().get(name);
        }

        return Err(rt::RuntimeError::UndefinedVariable {
            name: name.clone(),
            details: None,
        });
    }

    pub fn assign(&mut self, name: token::Token, value: rt::RuntimeValue) -> rt::RuntimeResult<()> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, value);
            return Ok(());
        }

        if let Some(enclosing) = &mut self.enclosing {
            return enclosing.inner.borrow_mut().assign(name, value);
        }

        return Err(rt::RuntimeError::UndefinedVariable {
            name: name.clone(),
            details: Some(format!("Cannot assign [{value:?}] to undefined variable")),
        });
    }
}
