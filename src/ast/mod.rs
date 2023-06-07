mod decl;
mod expr;
mod stmt;

use super::*;

pub use decl::*;
pub use expr::*;
pub use stmt::*;

pub type Id = usize;

pub trait AstId {
    fn id(&self) -> &Id;
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST<TypeInfo = ()> {
    pub id: Id,
    pub decls: Vec<ast::Decl<TypeInfo>>,
}

impl AST<()> {
    pub fn init_types<T>(self) -> AST<Option<T>> {
        return AST {
            id: self.id,
            decls: self
                .decls
                .into_iter()
                .map(|decl| decl.init_types())
                .collect(),
        };
    }
}

impl<T> AST<Option<T>> {
    pub fn unwrap_types(self) -> AST<T> {
        return AST {
            id: self.id,
            decls: self
                .decls
                .into_iter()
                .map(|decl| decl.unwrap_types())
                .collect(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticPath<TypeInfo = ()> {
    pub root: Option<Box<StaticPath<TypeInfo>>>,
    pub name: token::Token,
    pub type_info: TypeInfo,
}

impl StaticPath<()> {
    pub fn init_types<T>(self) -> StaticPath<Option<T>> {
        return StaticPath {
            root: self.root.map(|r| Box::new(r.init_types())),
            name: self.name,
            type_info: None,
        };
    }
}

impl<T> StaticPath<Option<T>> {
    pub fn unwrap_types(self) -> StaticPath<T> {
        return StaticPath {
            root: self.root.map(|r| Box::new(r.unwrap_types())),
            name: self.name,
            type_info: self.type_info.unwrap(),
        };
    }
}
