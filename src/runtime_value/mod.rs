mod fe_callable;
mod fe_function;
mod fe_instance;
mod fe_struct;

pub use fe_callable::*;
pub use fe_function::*;
pub use fe_instance::*;
pub use fe_struct::*;

use super::*;

use crate::ast;
use crate::token;

use thiserror::Error;

pub type RuntimeResult<T = RuntimeValue, E = RuntimeError> = Result<T, E>;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    Boolean(bool),
    Number(f64),
    PlainString(String),
    Callable(FerrumCallable),
    Instance(FerrumInstance),
}

impl RuntimeValue {
    pub fn from_expr(value: &ast::PlainLiteralExpr) -> Self {
        match value {
            ast::PlainLiteralExpr {
                literal_type: ast::PlainLiteralType::True,
                ..
            } => return Self::Boolean(true),

            ast::PlainLiteralExpr {
                literal_type: ast::PlainLiteralType::False,
                ..
            } => return Self::Boolean(false),

            ast::PlainLiteralExpr {
                literal_type: ast::PlainLiteralType::PlainString,
                token: token::Token { lexeme, .. },
                ..
            } => return Self::PlainString(lexeme[1..lexeme.len() - 1].to_string()),

            ast::PlainLiteralExpr {
                literal_type: ast::PlainLiteralType::Number,
                token: token::Token { lexeme, .. },
                ..
            } => return Self::Number(lexeme.parse().unwrap_or_default()),

            ast::PlainLiteralExpr {
                literal_type,
                token,
                ..
            } => panic!(
                "[{}:{}] Unexpected token for literal {literal_type:?}: {token:#?}",
                file!(),
                line!()
            ),
        }
    }
}

#[derive(Error, Clone, Debug, PartialEq)]
pub enum RuntimeError {
    #[error("invalid unary expression: {expr:#?}. Details = {details:?}")]
    InvalidUnaryExpr {
        expr: ast::UnaryExpr,
        details: Option<String>,
    },

    #[error("invalid binary expression: {expr:#?}. Details = {details:?}")]
    InvalidBinaryExpr {
        expr: ast::BinaryExpr,
        details: Option<String>,
    },

    #[error("invalid get expression: {name:#?}. Details = {details:?}")]
    InvalidGetExpr {
        name: token::Token,
        details: Option<String>,
    },

    #[error("invalid set expression: {name:#?}. Details = {details:?}")]
    InvalidSetExpr {
        name: token::Token,
        details: Option<String>,
    },

    #[error("undefined variable: {name:#?}. Details = {details:?}")]
    UndefinedVariable {
        name: token::Token,
        details: Option<String>,
    },

    #[error("undefined property: {name:#?}. Details = {details:?}")]
    UndefinedProperty {
        name: token::Token,
        details: Option<String>,
    },

    #[error("invalid callable: {value:#?}. Details = {details:?}")]
    InvalidCallable {
        value: RuntimeValue,
        details: Option<String>,
    },

    #[error("invalid super class: {name:#?}. Details = {details:?}")]
    InvalidSuperClass {
        name: token::Token,
        details: Option<String>,
    },

    #[error("function expected {expected} args, but call found {found}. Details = {details:?}")]
    WrongNumberOfArgs {
        expected: usize,
        found: usize,
        details: Option<String>,
    },

    #[error("non-error return short-circuit")]
    NonErrorReturnShortCircuit { value: Option<RuntimeValue> },
}
