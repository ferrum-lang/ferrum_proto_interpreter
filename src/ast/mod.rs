mod decl;
mod expr;
mod stmt;

use super::*;

pub use decl::*;
pub use expr::*;
pub use stmt::*;

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub decls: Vec<ast::Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticPath {
    pub root: Option<Box<StaticPath>>,
    pub name: token::Token,
}
