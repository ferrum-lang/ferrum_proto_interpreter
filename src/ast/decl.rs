use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl<TypeInfo = ()> {
    Use(UseDecl<TypeInfo>),
    Struct(StructDecl<TypeInfo>),
    Function(FunctionDecl<TypeInfo>),
}

impl Decl<()> {
    pub fn init_types<T>(self) -> Decl<Option<T>> {
        match self {
            Self::Use(decl) => return Decl::Use(decl.init_types()),
            Self::Struct(decl) => return Decl::Struct(decl.init_types()),
            Self::Function(decl) => return Decl::Function(decl.init_types()),
        }
    }
}

impl<T> Decl<Option<T>> {
    pub fn unwrap_types(self) -> Decl<T> {
        match self {
            Self::Use(decl) => return Decl::Use(decl.unwrap_types()),
            Self::Struct(decl) => return Decl::Struct(decl.unwrap_types()),
            Self::Function(decl) => return Decl::Function(decl.unwrap_types()),
        }
    }
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
pub struct UseDecl<TypeInfo = ()> {
    pub id: Id,
    pub decl_mod: Option<DeclMod>,
    pub path: StaticUsePath<TypeInfo>,
}

impl UseDecl<()> {
    pub fn init_types<T>(self) -> UseDecl<Option<T>> {
        return UseDecl {
            id: self.id,
            decl_mod: self.decl_mod,
            path: self.path.init_types(),
        };
    }
}

impl<T> UseDecl<Option<T>> {
    pub fn unwrap_types(self) -> UseDecl<T> {
        return UseDecl {
            id: self.id,
            decl_mod: self.decl_mod,
            path: self.path.unwrap_types(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticUsePath<TypeInfo = ()> {
    pub name: token::Token,
    pub type_info: TypeInfo,
    pub nexts: Vec<StaticUsePath<TypeInfo>>,
}

impl StaticUsePath<()> {
    pub fn init_types<T>(self) -> StaticUsePath<Option<T>> {
        return StaticUsePath {
            name: self.name,
            type_info: None,
            nexts: self
                .nexts
                .into_iter()
                .map(|next| next.init_types())
                .collect(),
        };
    }
}

impl<T> StaticUsePath<Option<T>> {
    pub fn unwrap_types(self) -> StaticUsePath<T> {
        return StaticUsePath {
            name: self.name,
            type_info: self.type_info.unwrap(),
            nexts: self
                .nexts
                .into_iter()
                .map(|next| next.unwrap_types())
                .collect(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl<TypeInfo = ()> {
    pub id: Id,
    pub decl_mod: Option<DeclMod>,
    pub name: token::Token,
    pub fields: Vec<StructField<TypeInfo>>,
}

impl StructDecl<()> {
    pub fn init_types<T>(self) -> StructDecl<Option<T>> {
        return StructDecl {
            id: self.id,
            decl_mod: self.decl_mod,
            name: self.name,
            fields: self
                .fields
                .into_iter()
                .map(|field| field.init_types())
                .collect(),
        };
    }
}

impl<T> StructDecl<Option<T>> {
    pub fn unwrap_types(self) -> StructDecl<T> {
        return StructDecl {
            id: self.id,
            decl_mod: self.decl_mod,
            name: self.name,
            fields: self
                .fields
                .into_iter()
                .map(|field| field.unwrap_types())
                .collect(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField<TypeInfo = ()> {
    pub name: token::Token,
    pub type_ref: ast::StaticPath<TypeInfo>,
}

impl StructField<()> {
    pub fn init_types<T>(self) -> StructField<Option<T>> {
        return StructField {
            name: self.name,
            type_ref: self.type_ref.init_types(),
        };
    }
}

impl<T> StructField<Option<T>> {
    pub fn unwrap_types(self) -> StructField<T> {
        return StructField {
            name: self.name,
            type_ref: self.type_ref.unwrap_types(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl<TypeInfo = ()> {
    pub id: Id,
    pub decl_mod: Option<DeclMod>,
    pub fn_mod: Option<FnMod>,
    pub name: token::Token,
    pub params: Vec<FnParam<TypeInfo>>,
    pub return_type: Option<ast::StaticPath<TypeInfo>>,
    pub body: Vec<Stmt<TypeInfo>>,
}

impl FunctionDecl<()> {
    pub fn init_types<T>(self) -> FunctionDecl<Option<T>> {
        return FunctionDecl {
            id: self.id,
            decl_mod: self.decl_mod,
            fn_mod: self.fn_mod,
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|param| param.init_types())
                .collect(),
            return_type: self.return_type.map(|r| r.init_types()),
            body: self.body.into_iter().map(|s| s.init_types()).collect(),
        };
    }
}

impl<T> FunctionDecl<Option<T>> {
    pub fn unwrap_types(self) -> FunctionDecl<T> {
        return FunctionDecl {
            id: self.id,
            decl_mod: self.decl_mod,
            fn_mod: self.fn_mod,
            name: self.name,
            params: self
                .params
                .into_iter()
                .map(|param| param.unwrap_types())
                .collect(),
            return_type: self.return_type.map(|r| r.unwrap_types()),
            body: self.body.into_iter().map(|s| s.unwrap_types()).collect(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnMod {
    Pure,
    Safe,
    Unsafe,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam<TypeInfo = ()> {
    pub name: token::Token,
    pub type_ref: ast::StaticPath<TypeInfo>,
}

impl FnParam<()> {
    pub fn init_types<T>(self) -> FnParam<Option<T>> {
        return FnParam {
            name: self.name,
            type_ref: self.type_ref.init_types(),
        };
    }
}

impl<T> FnParam<Option<T>> {
    pub fn unwrap_types(self) -> FnParam<T> {
        return FnParam {
            name: self.name,
            type_ref: self.type_ref.unwrap_types(),
        };
    }
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
