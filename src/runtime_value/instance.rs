use super::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct FerrumInstance {
    pub struct_: FerrumStruct,
    pub fields: Rc<RefCell<HashMap<String, RuntimeValue>>>,
}
