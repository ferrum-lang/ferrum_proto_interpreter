use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(ast::Expr),
    VarDecl(VarDeclStmt),
    Assignment(AssignmentStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclStmt {
    pub var_decl_type: VarDeclType,
    pub lhs: VarAssignPattern,
    pub value: Option<ast::Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarDeclType {
    Const,
    Mut,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStmt {
    pub lhs: AssignmentLHS,
    pub op: (AssignOp, token::Token),
    pub value: ast::Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentLHS {
    VarAssignPattern(ast::VarAssignPattern),
    Get(ast::GetExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub assignment_pattern: ast::VarAssignPattern,
    pub iter: ast::Expr,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarAssignPattern {
    Identity(ast::IdentityExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: ast::Expr,
    pub then_branch: Vec<Stmt>,
    pub else_branch: Option<ElseBranch>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElseBranch {
    ElseIf(Box<IfStmt>),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Equal,
    PlusEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<ast::Expr>,
}
