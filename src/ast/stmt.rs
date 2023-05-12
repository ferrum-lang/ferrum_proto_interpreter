use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expression(ExpressionStmt),
    Variable(VariableStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableStmt {
    pub name: token::Token,
    pub initializer: Option<expr::Expr>,
}
