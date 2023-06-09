use super::*;

use crate::ast;
use crate::environment as env;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Nil,
    MainFn,
    PlainString,
    Number,
    Callable(CallableTypeInfo),
    Ref(RefType),
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
    pub decl_id: ast::Id,
    pub method_on_type: Option<Box<TypeInfo>>,
    pub name: String,
    pub params: Vec<FnParamTypeInfo>,
    pub ret: Option<Box<TypeInfo>>,
    pub known_fn_mod: Option<ast::FnMod>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParamTypeInfo {
    pub name: String,
    pub type_info: Box<TypeInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RefType {
    pub is_mut: bool,
    pub of: Box<TypeInfo>,
}
