use super::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct FerrumInstance {
    pub struct_: FerrumStruct,
    pub fields: Rc<RefCell<HashMap<String, RuntimeValue>>>,
}

impl FerrumInstance {
    pub fn new(struct_: FerrumStruct) -> Self {
        return Self {
            struct_,
            fields: Rc::new(RefCell::new(HashMap::new())),
        };
    }
}
