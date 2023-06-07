use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<TypeInfo = ()> {
    Expr(ExprStmt<TypeInfo>),
    VarDecl(VarDeclStmt<TypeInfo>),
    Assignment(AssignmentStmt<TypeInfo>),
    For(ForStmt<TypeInfo>),
    If(IfStmt<TypeInfo>),
    Return(ReturnStmt<TypeInfo>),
}

impl Stmt<()> {
    pub fn init_types<T>(self) -> Stmt<Option<T>> {
        match self {
            Self::Expr(stmt) => return Stmt::Expr(stmt.init_types()),
            Self::VarDecl(stmt) => return Stmt::VarDecl(stmt.init_types()),
            Self::Assignment(stmt) => return Stmt::Assignment(stmt.init_types()),
            Self::For(stmt) => return Stmt::For(stmt.init_types()),
            Self::If(stmt) => return Stmt::If(stmt.init_types()),
            Self::Return(stmt) => return Stmt::Return(stmt.init_types()),
        }
    }
}

impl<T> Stmt<Option<T>> {
    pub fn unwrap_types(self) -> Stmt<T> {
        match self {
            Self::Expr(stmt) => return Stmt::Expr(stmt.unwrap_types()),
            Self::VarDecl(stmt) => return Stmt::VarDecl(stmt.unwrap_types()),
            Self::Assignment(stmt) => return Stmt::Assignment(stmt.unwrap_types()),
            Self::For(stmt) => return Stmt::For(stmt.unwrap_types()),
            Self::If(stmt) => return Stmt::If(stmt.unwrap_types()),
            Self::Return(stmt) => return Stmt::Return(stmt.unwrap_types()),
        }
    }
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
pub struct ExprStmt<TypeInfo = ()> {
    pub id: Id,
    pub expr: ast::Expr<TypeInfo>,
}

impl ExprStmt<()> {
    pub fn init_types<T>(self) -> ExprStmt<Option<T>> {
        return ExprStmt {
            id: self.id,
            expr: self.expr.init_types(),
        };
    }
}

impl<T> ExprStmt<Option<T>> {
    pub fn unwrap_types(self) -> ExprStmt<T> {
        return ExprStmt {
            id: self.id,
            expr: self.expr.unwrap_types(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclStmt<TypeInfo = ()> {
    pub id: Id,
    pub var_decl_type: VarDeclType,
    pub lhs: VarAssignPattern<TypeInfo>,
    pub value: Option<ast::Expr<TypeInfo>>,
}

impl VarDeclStmt<()> {
    pub fn init_types<T>(self) -> VarDeclStmt<Option<T>> {
        return VarDeclStmt {
            id: self.id,
            var_decl_type: self.var_decl_type,
            lhs: self.lhs.init_types(),
            value: self.value.map(|v| v.init_types()),
        };
    }
}

impl<T> VarDeclStmt<Option<T>> {
    pub fn unwrap_types(self) -> VarDeclStmt<T> {
        return VarDeclStmt {
            id: self.id,
            var_decl_type: self.var_decl_type,
            lhs: self.lhs.unwrap_types(),
            value: self.value.map(|v| v.unwrap_types()),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarDeclType {
    Const,
    Mut,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentStmt<TypeInfo = ()> {
    pub id: Id,
    pub lhs: AssignmentLHS<TypeInfo>,
    pub op: (AssignOp, token::Token),
    pub value: ast::Expr<TypeInfo>,
}

impl AssignmentStmt<()> {
    pub fn init_types<T>(self) -> AssignmentStmt<Option<T>> {
        return AssignmentStmt {
            id: self.id,
            lhs: self.lhs.init_types(),
            op: self.op,
            value: self.value.init_types(),
        };
    }
}

impl<T> AssignmentStmt<Option<T>> {
    pub fn unwrap_types(self) -> AssignmentStmt<T> {
        return AssignmentStmt {
            id: self.id,
            lhs: self.lhs.unwrap_types(),
            op: self.op,
            value: self.value.unwrap_types(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentLHS<TypeInfo = ()> {
    VarAssignPattern(ast::VarAssignPattern<TypeInfo>),
    Get(ast::GetExpr<TypeInfo>),
}

impl AssignmentLHS<()> {
    pub fn init_types<T>(self) -> AssignmentLHS<Option<T>> {
        match self {
            Self::VarAssignPattern(p) => return AssignmentLHS::VarAssignPattern(p.init_types()),
            Self::Get(p) => return AssignmentLHS::Get(p.init_types()),
        }
    }
}

impl<T> AssignmentLHS<Option<T>> {
    pub fn unwrap_types(self) -> AssignmentLHS<T> {
        match self {
            Self::VarAssignPattern(p) => return AssignmentLHS::VarAssignPattern(p.unwrap_types()),
            Self::Get(p) => return AssignmentLHS::Get(p.unwrap_types()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt<TypeInfo = ()> {
    pub id: Id,
    pub assignment_pattern: ast::VarAssignPattern<TypeInfo>,
    pub iter: ast::Expr<TypeInfo>,
    pub body: Vec<Stmt<TypeInfo>>,
}

impl ForStmt<()> {
    pub fn init_types<T>(self) -> ForStmt<Option<T>> {
        return ForStmt {
            id: self.id,
            assignment_pattern: self.assignment_pattern.init_types(),
            iter: self.iter.init_types(),
            body: self.body.into_iter().map(|s| s.init_types()).collect(),
        };
    }
}

impl<T> ForStmt<Option<T>> {
    pub fn unwrap_types(self) -> ForStmt<T> {
        return ForStmt {
            id: self.id,
            assignment_pattern: self.assignment_pattern.unwrap_types(),
            iter: self.iter.unwrap_types(),
            body: self.body.into_iter().map(|s| s.unwrap_types()).collect(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarAssignPattern<TypeInfo = ()> {
    Identity(ast::IdentityExpr<TypeInfo>),
}

impl VarAssignPattern<()> {
    pub fn init_types<T>(self) -> VarAssignPattern<Option<T>> {
        match self {
            Self::Identity(p) => return VarAssignPattern::Identity(p.init_types()),
        }
    }
}

impl<T> VarAssignPattern<Option<T>> {
    pub fn unwrap_types(self) -> VarAssignPattern<T> {
        match self {
            Self::Identity(p) => return VarAssignPattern::Identity(p.unwrap_types()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt<TypeInfo = ()> {
    pub id: Id,
    pub condition: ast::Expr<TypeInfo>,
    pub then_branch: Vec<Stmt<TypeInfo>>,
    pub else_branch: Option<ElseBranch<TypeInfo>>,
}

impl IfStmt<()> {
    pub fn init_types<T>(self) -> IfStmt<Option<T>> {
        return IfStmt {
            id: self.id,
            condition: self.condition.init_types(),
            then_branch: self
                .then_branch
                .into_iter()
                .map(|s| s.init_types())
                .collect(),
            else_branch: self.else_branch.map(|e| e.init_types()),
        };
    }
}

impl<T> IfStmt<Option<T>> {
    pub fn unwrap_types(self) -> IfStmt<T> {
        return IfStmt {
            id: self.id,
            condition: self.condition.unwrap_types(),
            then_branch: self
                .then_branch
                .into_iter()
                .map(|s| s.unwrap_types())
                .collect(),
            else_branch: self.else_branch.map(|e| e.unwrap_types()),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElseBranch<TypeInfo = ()> {
    ElseIf(Box<IfStmt<TypeInfo>>),
    Block(Vec<Stmt<TypeInfo>>),
}

impl ElseBranch<()> {
    pub fn init_types<T>(self) -> ElseBranch<Option<T>> {
        match self {
            Self::ElseIf(b) => return ElseBranch::ElseIf(Box::new(b.init_types())),
            Self::Block(b) => {
                return ElseBranch::Block(b.into_iter().map(|s| s.init_types()).collect())
            }
        }
    }
}

impl<T> ElseBranch<Option<T>> {
    pub fn unwrap_types(self) -> ElseBranch<T> {
        match self {
            Self::ElseIf(b) => return ElseBranch::ElseIf(Box::new(b.unwrap_types())),
            Self::Block(b) => {
                return ElseBranch::Block(b.into_iter().map(|s| s.unwrap_types()).collect())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Equal,
    PlusEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt<TypeInfo = ()> {
    pub id: Id,
    pub value: Option<ast::Expr<TypeInfo>>,
}

impl ReturnStmt<()> {
    pub fn init_types<T>(self) -> ReturnStmt<Option<T>> {
        return ReturnStmt {
            id: self.id,
            value: self.value.map(|v| v.init_types()),
        };
    }
}

impl<T> ReturnStmt<Option<T>> {
    pub fn unwrap_types(self) -> ReturnStmt<T> {
        return ReturnStmt {
            id: self.id,
            value: self.value.map(|v| v.unwrap_types()),
        };
    }
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
