#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    PlainString,
    Function(FunctionTypeInfo),
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
