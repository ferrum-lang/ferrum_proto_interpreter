use super::*;

use crate::environment as env;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Nil,
    MainFn,
    PlainString,
    Number,
    Callable(CallableTypeInfo),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallableTypeInfo {
    Function(FunctionTypeInfo),
}

impl CallableTypeInfo {
    pub fn arity(&self) -> usize {
        match self {
            Self::Function(function) => return function.params.len(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionTypeInfo {
    pub params: Vec<FnParamTypeInfo>,
    pub ret: Option<Box<TypeInfo>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParamTypeInfo {
    pub name: String,
    pub type_info: Box<TypeInfo>,
}
