mod decl;
mod expr;
mod stmt;

use super::*;

pub use decl::*;
pub use expr::*;
pub use stmt::*;

pub type Id = usize;

pub trait AstId {
    fn id(&self) -> &Id;
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub id: Id,
    pub decls: Vec<ast::Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeRef {
    pub ref_type: Option<RefType>,
    pub static_path: StaticPath,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticPath {
    pub root: Option<Box<StaticPath>>,
    pub name: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefType {
    Shared,
    Mut,
}
