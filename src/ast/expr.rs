use super::*;

use crate::token;

pub type ExprId = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Call(CallExpr),
    Get(GetExpr),
    Identity(IdentityExpr),
    PlainLiteral(PlainLiteralExpr),
    FormatString(FormatStringExpr),
    Logical(LogicalExpr),
    SelfVal(SelfValExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}

impl Expr {
    pub fn id(&self) -> ExprId {
        match self {
            Self::Call(expr) => return expr.id,
            Self::Get(expr) => return expr.id,
            Self::Identity(expr) => return expr.id,
            Self::PlainLiteral(expr) => return expr.id,
            Self::FormatString(expr) => return expr.id,
            Self::Logical(expr) => return expr.id,
            Self::SelfVal(expr) => return expr.id,
            Self::Unary(expr) => return expr.id,
            Self::Binary(expr) => return expr.id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub id: ExprId,
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetExpr {
    pub id: ExprId,
    pub object: Box<Expr>,
    pub name: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentityExpr {
    pub id: ExprId,
    pub name: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PlainLiteralExpr {
    pub id: ExprId,
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
    pub id: ExprId,
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
    pub id: ExprId,
    pub left: Box<Expr>,
    pub operator: token::Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelfValExpr {
    pub id: ExprId,
    pub keyword: token::Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub id: ExprId,
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
    pub id: ExprId,
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
}
