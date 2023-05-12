mod expr;
mod stmt;

use super::*;

pub use expr::*;
pub use stmt::*;

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub stmts: Vec<stmt::Stmt>,
}
