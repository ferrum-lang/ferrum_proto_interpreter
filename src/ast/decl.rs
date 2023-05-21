use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Use(UseDecl),
    Struct(StructDecl),
    Function(FunctionDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub path: StaticUsePath,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticUsePath {
    pub name: token::Token,
    pub nexts: Vec<StaticUsePath>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub fn_mod: Option<FnMod>,
    pub name: token::Token,
    pub params: Vec<FnParam>,
    pub return_type: Option<ast::StaticPath>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnMod {
    Pure,
    Safe,
    Unsafe,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    pub name: token::Token,
    pub type_ref: ast::StaticPath,
}
