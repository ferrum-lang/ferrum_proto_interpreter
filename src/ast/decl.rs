use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Use(UseDecl),
    Struct(StructDecl),
    Function(FunctionDecl),
}

impl AstId for Decl {
    fn id(&self) -> &Id {
        match self {
            Self::Use(decl) => return &decl.id,
            Self::Struct(decl) => return &decl.id,
            Self::Function(decl) => return &decl.id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclMod {
    Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub id: Id,
    pub decl_mod: Option<DeclMod>,
    pub path: StaticUsePath,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticUsePath {
    pub name: token::Token,
    pub nexts: Vec<StaticUsePath>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub id: Id,
    pub decl_mod: Option<DeclMod>,
    pub name: token::Token,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: token::Token,
    pub type_ref: ast::StaticPath,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub id: Id,
    pub decl_mod: Option<DeclMod>,
    pub fn_mod: Option<FnMod>,
    pub name: token::Token,
    pub params: Vec<FnParam>,
    pub return_type: Option<ast::StaticPath>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnMod {
    Norm,
    Pure,
    AtLeastPure,
    Risk,
    Safe,
    AtLeastSafe,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    pub name: token::Token,
    pub type_ref: ast::StaticPath,
}

// Visitor pattern
pub trait DeclVisitor<R = ()> {
    fn visit_use_decl(&mut self, decl: &UseDecl) -> R;
    fn visit_struct_decl(&mut self, decl: &StructDecl) -> R;
    fn visit_function_decl(&mut self, decl: &FunctionDecl) -> R;
}

pub trait DeclAccept<R, V: DeclVisitor<R>> {
    fn accept(&self, visitor: &mut V) -> R;
}

impl<R, V: DeclVisitor<R>> DeclAccept<R, V> for Decl {
    fn accept(&self, visitor: &mut V) -> R {
        return match self {
            Self::Use(stmt) => stmt.accept(visitor),
            Self::Struct(stmt) => stmt.accept(visitor),
            Self::Function(stmt) => stmt.accept(visitor),
        };
    }
}

impl<R, V: DeclVisitor<R>> DeclAccept<R, V> for UseDecl {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_use_decl(self);
    }
}

impl<R, V: DeclVisitor<R>> DeclAccept<R, V> for StructDecl {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_struct_decl(self);
    }
}

impl<R, V: DeclVisitor<R>> DeclAccept<R, V> for FunctionDecl {
    fn accept(&self, visitor: &mut V) -> R {
        return visitor.visit_function_decl(self);
    }
}
