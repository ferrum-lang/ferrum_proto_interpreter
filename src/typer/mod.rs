mod type_info;

pub use type_info::*;

use super::*;

use crate::ast;

#[derive(Debug, Clone, PartialEq)]
pub struct Typer {
    ast: ast::AST<Option<TypeInfo>>,

    error_ctx: ErrorContext,
}

impl Typer {
    pub fn from_context(ast: ast::AST) -> Self {
        return Self {
            ast: ast.init_types(),

            error_ctx: ErrorContext::new(),
        };
    }

    pub fn resolve_types(self) -> (ast::AST<TypeInfo>, ErrorContext) {
        return (self.ast.unwrap_types(), self.error_ctx);
    }
}
