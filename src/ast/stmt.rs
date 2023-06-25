use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(ExprStmt),
    VarDecl(VarDeclStmt),
    Assignment(AssignmentStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
}

impl AstId for Stmt {
    fn id(&self) -> &Id {
        match self {
            Self::Expr(stmt) => return &stmt.id,
            Self::VarDecl(stmt) => return &stmt.id,
            Self::Assignment(stmt) => return &stmt.id,
            Self::For(stmt) => return &stmt.id,
            Self::If(stmt) => return &stmt.id,
            Self::Return(stmt) => return &stmt.id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub id: Id,
    pub expr: ast::Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclStmt {
    pub id: Id,
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
    pub id: Id,
    pub lhs: AssignmentLHS,
    pub op: (AssignOp, token::Token),
    pub value: ast::Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentLHS {
    Var(ast::IdentityExpr),
    Get(ast::GetExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub id: Id,
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
    pub id: Id,
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
    pub id: Id,
    pub keyword: token::Token,
    pub value: Option<ast::Expr>,
}

// Visitor pattern
pub trait StmtVisitor<R = ()> {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> R;
    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> R;
    fn visit_assignment_stmt(&mut self, stmt: &AssignmentStmt) -> R;
    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> R;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> R;
    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> R;
}

pub trait StmtAccept<R, V: StmtVisitor<R>> {
    fn accept(&self, visitor: &mut V) -> R;
}

impl<R, V: StmtVisitor<R>> StmtAccept<R, V> for Stmt {
    fn accept(&self, visitor: &mut V) -> R {
        return match self {
            Self::Expr(stmt) => stmt.accept(visitor),
            Self::VarDecl(stmt) => stmt.accept(visitor),
            Self::Assignment(stmt) => stmt.accept(visitor),
            Self::For(stmt) => stmt.accept(visitor),
            Self::If(stmt) => stmt.accept(visitor),
            Self::Return(stmt) => stmt.accept(visitor),
        };
    }
}

impl<R, V: StmtVisitor<R>> StmtAccept<R, V> for ExprStmt {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_expr_stmt(self);
    }
}

impl<R, V: StmtVisitor<R>> StmtAccept<R, V> for VarDeclStmt {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_var_decl_stmt(self);
    }
}

impl<R, V: StmtVisitor<R>> StmtAccept<R, V> for AssignmentStmt {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_assignment_stmt(self);
    }
}

impl<R, V: StmtVisitor<R>> StmtAccept<R, V> for ForStmt {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_for_stmt(self);
    }
}

impl<R, V: StmtVisitor<R>> StmtAccept<R, V> for IfStmt {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_if_stmt(self);
    }
}

impl<R, V: StmtVisitor<R>> StmtAccept<R, V> for ReturnStmt {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_return_stmt(self);
    }
}
