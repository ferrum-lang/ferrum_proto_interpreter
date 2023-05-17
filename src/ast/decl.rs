use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Struct(StructDecl),
    Function(FunctionDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: token::Token,
    pub params: Vec<FnParam>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    pub name: token::Token,
    pub type_ref: ast::StaticPath,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: token::Token,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: token::Token,
    pub type_ref: ast::StaticPath,
}
