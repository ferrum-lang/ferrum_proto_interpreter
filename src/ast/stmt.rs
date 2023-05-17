use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(ast::Expr),
    Assignment(AssignmentStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStmt {
    pub lhs: AssignmentLHS,
    pub value: ast::Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentLHS {
    Identity(ast::IdentityExpr),
    Get(ast::GetExpr),
}
