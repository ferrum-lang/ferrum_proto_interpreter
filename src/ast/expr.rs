use super::*;

use crate::token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<TypeInfo = ()> {
    Call(CallExpr<TypeInfo>),
    Get(GetExpr<TypeInfo>),
    Identity(IdentityExpr<TypeInfo>),
    PlainLiteral(PlainLiteralExpr<TypeInfo>),
    FormatString(FormatStringExpr<TypeInfo>),
    Logical(LogicalExpr<TypeInfo>),
    SelfVal(SelfValExpr<TypeInfo>),
    Unary(UnaryExpr<TypeInfo>),
    Binary(BinaryExpr<TypeInfo>),
}

impl Expr<()> {
    pub fn init_types<T>(self) -> Expr<Option<T>> {
        match self {
            Self::Call(e) => return Expr::Call(e.init_types()),
            Self::Get(e) => return Expr::Get(e.init_types()),
            Self::Identity(e) => return Expr::Identity(e.init_types()),
            Self::PlainLiteral(e) => return Expr::PlainLiteral(e.init_types()),
            Self::FormatString(e) => return Expr::FormatString(e.init_types()),
            Self::Logical(e) => return Expr::Logical(e.init_types()),
            Self::SelfVal(e) => return Expr::SelfVal(e.init_types()),
            Self::Unary(e) => return Expr::Unary(e.init_types()),
            Self::Binary(e) => return Expr::Binary(e.init_types()),
        }
    }
}

impl<T> Expr<Option<T>> {
    pub fn unwrap_types(self) -> Expr<T> {
        match self {
            Self::Call(e) => return Expr::Call(e.unwrap_types()),
            Self::Get(e) => return Expr::Get(e.unwrap_types()),
            Self::Identity(e) => return Expr::Identity(e.unwrap_types()),
            Self::PlainLiteral(e) => return Expr::PlainLiteral(e.unwrap_types()),
            Self::FormatString(e) => return Expr::FormatString(e.unwrap_types()),
            Self::Logical(e) => return Expr::Logical(e.unwrap_types()),
            Self::SelfVal(e) => return Expr::SelfVal(e.unwrap_types()),
            Self::Unary(e) => return Expr::Unary(e.unwrap_types()),
            Self::Binary(e) => return Expr::Binary(e.unwrap_types()),
        }
    }
}

impl AstId for Expr {
    fn id(&self) -> &Id {
        match self {
            Self::Call(expr) => return &expr.id,
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
pub struct CallExpr<TypeInfo = ()> {
    pub id: Id,
    pub callee: Box<Expr<TypeInfo>>,
    pub arguments: Vec<Expr<TypeInfo>>,
    pub type_info: TypeInfo,
}

impl CallExpr<()> {
    pub fn init_types<T>(self) -> CallExpr<Option<T>> {
        return CallExpr {
            id: self.id,
            callee: Box::new(self.callee.init_types()),
            arguments: self
                .arguments
                .into_iter()
                .map(|arg| arg.init_types())
                .collect(),
            type_info: None,
        };
    }
}

impl<T> CallExpr<Option<T>> {
    pub fn unwrap_types(self) -> CallExpr<T> {
        return CallExpr {
            id: self.id,
            callee: Box::new(self.callee.unwrap_types()),
            arguments: self
                .arguments
                .into_iter()
                .map(|arg| arg.unwrap_types())
                .collect(),
            type_info: self.type_info.unwrap(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetExpr<TypeInfo = ()> {
    pub id: Id,
    pub object: Box<Expr<TypeInfo>>,
    pub name: token::Token,
    pub type_info: TypeInfo,
}

impl GetExpr<()> {
    pub fn init_types<T>(self) -> GetExpr<Option<T>> {
        return GetExpr {
            id: self.id,
            object: Box::new(self.object.init_types()),
            name: self.name,
            type_info: None,
        };
    }
}

impl<T> GetExpr<Option<T>> {
    pub fn unwrap_types(self) -> GetExpr<T> {
        return GetExpr {
            id: self.id,
            object: Box::new(self.object.unwrap_types()),
            name: self.name,
            type_info: self.type_info.unwrap(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentityExpr<TypeInfo = ()> {
    pub id: Id,
    pub name: token::Token,
    pub type_info: TypeInfo,
}

impl IdentityExpr<()> {
    pub fn init_types<T>(self) -> IdentityExpr<Option<T>> {
        return IdentityExpr {
            id: self.id,
            name: self.name,
            type_info: None,
        };
    }
}

impl<T> IdentityExpr<Option<T>> {
    pub fn unwrap_types(self) -> IdentityExpr<T> {
        return IdentityExpr {
            id: self.id,
            name: self.name,
            type_info: self.type_info.unwrap(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PlainLiteralExpr<TypeInfo = ()> {
    pub id: Id,
    pub literal_type: PlainLiteralType,
    pub token: token::Token,
    pub type_info: TypeInfo,
}

impl PlainLiteralExpr<()> {
    pub fn init_types<T>(self) -> PlainLiteralExpr<Option<T>> {
        return PlainLiteralExpr {
            id: self.id,
            literal_type: self.literal_type,
            token: self.token,
            type_info: None,
        };
    }
}

impl<T> PlainLiteralExpr<Option<T>> {
    pub fn unwrap_types(self) -> PlainLiteralExpr<T> {
        return PlainLiteralExpr {
            id: self.id,
            literal_type: self.literal_type,
            token: self.token,
            type_info: self.type_info.unwrap(),
        };
    }
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
pub struct FormatStringExpr<TypeInfo = ()> {
    pub id: Id,
    pub open: token::Token,
    pub parts: Vec<FormatStringExprPart<TypeInfo>>,
    pub type_info: TypeInfo,
}

impl FormatStringExpr<()> {
    pub fn init_types<T>(self) -> FormatStringExpr<Option<T>> {
        return FormatStringExpr {
            id: self.id,
            open: self.open,
            parts: self
                .parts
                .into_iter()
                .map(|part| part.init_types())
                .collect(),
            type_info: None,
        };
    }
}

impl<T> FormatStringExpr<Option<T>> {
    pub fn unwrap_types(self) -> FormatStringExpr<T> {
        return FormatStringExpr {
            id: self.id,
            open: self.open,
            parts: self
                .parts
                .into_iter()
                .map(|part| part.unwrap_types())
                .collect(),
            type_info: self.type_info.unwrap(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormatStringExprPart<TypeInfo = ()> {
    pub left_brace: token::Token,
    pub expr: Box<Expr<TypeInfo>>,
    pub right_brace: token::Token,
    pub fmt_str_part: token::Token,
}

impl FormatStringExprPart<()> {
    pub fn init_types<T>(self) -> FormatStringExprPart<Option<T>> {
        return FormatStringExprPart {
            left_brace: self.left_brace,
            expr: Box::new(self.expr.init_types()),
            right_brace: self.right_brace,
            fmt_str_part: self.fmt_str_part,
        };
    }
}

impl<T> FormatStringExprPart<Option<T>> {
    pub fn unwrap_types(self) -> FormatStringExprPart<T> {
        return FormatStringExprPart {
            left_brace: self.left_brace,
            expr: Box::new(self.expr.unwrap_types()),
            right_brace: self.right_brace,
            fmt_str_part: self.fmt_str_part,
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr<TypeInfo = ()> {
    pub id: Id,
    pub left: Box<Expr<TypeInfo>>,
    pub operator: token::Token,
    pub right: Box<Expr<TypeInfo>>,
    pub type_info: TypeInfo,
}

impl LogicalExpr<()> {
    pub fn init_types<T>(self) -> LogicalExpr<Option<T>> {
        return LogicalExpr {
            id: self.id,
            left: Box::new(self.left.init_types()),
            operator: self.operator,
            right: Box::new(self.right.init_types()),
            type_info: None,
        };
    }
}

impl<T> LogicalExpr<Option<T>> {
    pub fn unwrap_types(self) -> LogicalExpr<T> {
        return LogicalExpr {
            id: self.id,
            left: Box::new(self.left.unwrap_types()),
            operator: self.operator,
            right: Box::new(self.right.unwrap_types()),
            type_info: self.type_info.unwrap(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelfValExpr<TypeInfo = ()> {
    pub id: Id,
    pub keyword: token::Token,
    pub type_info: TypeInfo,
}

impl SelfValExpr<()> {
    pub fn init_types<T>(self) -> SelfValExpr<Option<T>> {
        return SelfValExpr {
            id: self.id,
            keyword: self.keyword,
            type_info: None,
        };
    }
}

impl<T> SelfValExpr<Option<T>> {
    pub fn unwrap_types(self) -> SelfValExpr<T> {
        return SelfValExpr {
            id: self.id,
            keyword: self.keyword,
            type_info: self.type_info.unwrap(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr<TypeInfo = ()> {
    pub id: Id,
    pub op: (UnaryOp, token::Token),
    pub right: Box<Expr<TypeInfo>>,
    pub type_info: TypeInfo,
}

impl UnaryExpr<()> {
    pub fn init_types<T>(self) -> UnaryExpr<Option<T>> {
        return UnaryExpr {
            id: self.id,
            op: self.op,
            right: Box::new(self.right.init_types()),
            type_info: None,
        };
    }
}

impl<T> UnaryExpr<Option<T>> {
    pub fn unwrap_types(self) -> UnaryExpr<T> {
        return UnaryExpr {
            id: self.id,
            op: self.op,
            right: Box::new(self.right.unwrap_types()),
            type_info: self.type_info.unwrap(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr<TypeInfo = ()> {
    pub id: Id,
    pub left: Box<Expr<TypeInfo>>,
    pub op: (BinaryOp, token::Token),
    pub right: Box<Expr<TypeInfo>>,
    pub type_info: TypeInfo,
}

impl BinaryExpr<()> {
    pub fn init_types<T>(self) -> BinaryExpr<Option<T>> {
        return BinaryExpr {
            id: self.id,
            left: Box::new(self.left.init_types()),
            op: self.op,
            right: Box::new(self.right.init_types()),
            type_info: None,
        };
    }
}

impl<T> BinaryExpr<Option<T>> {
    pub fn unwrap_types(self) -> BinaryExpr<T> {
        return BinaryExpr {
            id: self.id,
            left: Box::new(self.left.unwrap_types()),
            op: self.op,
            right: Box::new(self.right.unwrap_types()),
            type_info: self.type_info.unwrap(),
        };
    }
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
