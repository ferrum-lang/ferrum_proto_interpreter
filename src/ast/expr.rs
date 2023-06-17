use super::*;

use crate::token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Call(CallExpr),
    Crash(CrashExpr),
    Get(GetExpr),
    Identity(IdentityExpr),
    PlainLiteral(PlainLiteralExpr),
    FormatString(FormatStringExpr),
    Logical(LogicalExpr),
    SelfVal(SelfValExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}

impl AstId for Expr {
    fn id(&self) -> &Id {
        match self {
            Self::Call(expr) => return &expr.id,
            Self::Crash(expr) => return &expr.id,
            Self::Get(expr) => return &expr.id,
            Self::Identity(expr) => return &expr.id,
            Self::PlainLiteral(expr) => return &expr.id,
            Self::FormatString(expr) => return &expr.id,
            Self::Logical(expr) => return &expr.id,
            Self::SelfVal(expr) => return &expr.id,
            Self::Unary(expr) => return &expr.id,
            Self::Binary(expr) => return &expr.id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub id: Id,
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CrashExpr {
    pub id: Id,
    pub error: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetExpr {
    pub id: Id,
    pub object: Box<Expr>,
    pub name: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentityExpr {
    pub id: Id,
    pub name: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PlainLiteralExpr {
    pub id: Id,
    pub literal_type: PlainLiteralType,
    pub token: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PlainLiteralType {
    Number,
    Char,
    True,
    False,
    PlainString,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatStringExpr {
    pub id: Id,
    pub open: token::Token,
    pub parts: Vec<FormatStringExprPart>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatStringExprPart {
    pub left_brace: token::Token,
    pub expr: Box<Expr>,
    pub right_brace: token::Token,
    pub fmt_str_part: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    pub id: Id,
    pub left: Box<Expr>,
    pub operator: token::Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelfValExpr {
    pub id: Id,
    pub keyword: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub id: Id,
    pub op: (UnaryOp, token::Token),
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub id: Id,
    pub left: Box<Expr>,
    pub op: (BinaryOp, token::Token),
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Divide,
    Times,
    NotEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Range,
    Modulo,
}

// Visitor pattern
pub trait ExprVisitor<R = ()> {
    fn visit_call_expr(&mut self, expr: &CallExpr) -> R;
    fn visit_crash_expr(&mut self, expr: &CrashExpr) -> R;
    fn visit_get_expr(&mut self, expr: &GetExpr) -> R;
    fn visit_identity_expr(&mut self, expr: &IdentityExpr) -> R;
    fn visit_plain_literal_expr(&mut self, expr: &PlainLiteralExpr) -> R;
    fn visit_format_string_expr(&mut self, expr: &FormatStringExpr) -> R;
    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> R;
    fn visit_self_expr(&mut self, expr: &SelfValExpr) -> R;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> R;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> R;
}

pub trait ExprAccept<R, V: ExprVisitor<R>> {
    fn accept(&self, visitor: &mut V) -> R;
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for Expr {
    fn accept(&self, visitor: &mut V) -> R {
        return match self {
            Self::Call(expr) => expr.accept(visitor),
            Self::Crash(expr) => expr.accept(visitor),
            Self::Get(expr) => expr.accept(visitor),
            Self::Identity(expr) => expr.accept(visitor),
            Self::PlainLiteral(expr) => expr.accept(visitor),
            Self::FormatString(expr) => expr.accept(visitor),
            Self::Logical(expr) => expr.accept(visitor),
            Self::SelfVal(expr) => expr.accept(visitor),
            Self::Unary(expr) => expr.accept(visitor),
            Self::Binary(expr) => expr.accept(visitor),
        };
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for CallExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_call_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for CrashExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_crash_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for GetExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_get_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for IdentityExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_identity_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for PlainLiteralExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_plain_literal_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for FormatStringExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_format_string_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for LogicalExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_logical_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for SelfValExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_self_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for UnaryExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_unary_expr(self);
    }
}

impl<R, V: ExprVisitor<R>> ExprAccept<R, V> for BinaryExpr {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_binary_expr(self);
    }
}
