use super::*;

use resolver;
use runtime_value as rt;
use token;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct SharedEnvironment<Val> {
    pub inner: Rc<RefCell<Environment<Val>>>,
}

#[derive(Debug, PartialEq)]
pub struct Environment<Val> {
    enclosing: Option<SharedEnvironment<Val>>,
    values: HashMap<String, Val>,
}

impl<Val> SharedEnvironment<Val>
where
    Val: fmt::Debug + Clone + PartialEq,
{
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

    pub fn enclosed(self) -> Environment<Val> {
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

    pub fn define(&self, name: String, value: Val) {
        self.inner.borrow_mut().define(name, value);
    }

    pub fn get_at(
        &self,
        distance: resolver::Distance,
        name: &token::Token,
    ) -> rt::RuntimeResult<Val> {
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

    pub fn get(&self, name: &token::Token) -> rt::RuntimeResult<Val> {
        return self.inner.borrow().get(name);
    }

    pub fn assign_at(
        &self,
        distance: resolver::Distance,
        name: token::Token,
        value: Val,
    ) -> rt::RuntimeResult<()> {
        let this = self.share().ancestor(distance).inner;

        if this.borrow().values.contains_key(&name.lexeme) {
            this.borrow_mut().values.insert(name.lexeme, value);
            return Ok(());
        }

        return Err(rt::RuntimeError::UndefinedVariable {
            name: name.clone(),
            details: Some(format!("Cannot assign [{value:?}] to undefined variable")),
        });
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

impl<Val> Environment<Val>
where
    Val: fmt::Debug + Clone + PartialEq,
{
    pub fn new() -> Self {
        return Self {
            enclosing: None,
            values: HashMap::new(),
        };
    }

    pub fn define(&mut self, name: String, value: Val) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &token::Token) -> rt::RuntimeResult<Val> {
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

    pub fn assign(&mut self, name: token::Token, value: Val) -> rt::RuntimeResult<()> {
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
