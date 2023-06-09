mod type_info;

use type_info::*;

use super::*;

use crate::ast::{self, AstId, DeclAccept, ExprAccept, StmtAccept};
use crate::resolver;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeChecker<'a> {
    tree: &'a ast::AST,
    locals: &'a HashMap<ast::Id, resolver::Distance>,

    types: HashMap<ast::Id, TypeInfo>,
    error_ctx: ErrorContext,
}

impl<'a> TypeChecker<'a> {
    pub fn from_context(
        tree: &'a ast::AST,
        locals: &'a HashMap<ast::Id, resolver::Distance>,
    ) -> Self {
        return Self {
            tree,
            locals,

            types: HashMap::new(),
            error_ctx: ErrorContext::new(),
        };
    }

    pub fn resolve_types(mut self) -> (HashMap<ast::Id, TypeInfo>, ErrorContext) {
        self.resolve_decls(&self.tree.decls);

        return (self.types, self.error_ctx);
    }

    fn resolve_decls(&mut self, decls: &Vec<ast::Decl>) {
        for decl in decls {
            self.resolve_decl(decl);
        }
    }

    fn resolve_decl(&mut self, decl: &ast::Decl) {
        decl.accept(self);
    }

    fn resolve_stmts(&mut self, stmts: &Vec<ast::Stmt>) {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &ast::Stmt) {
        stmt.accept(self);
    }

    fn resolve_expr(&mut self, expr: &ast::Expr) {
        expr.accept(self);
    }

    fn static_path_type(&mut self, static_path: &ast::StaticPath) -> TypeInfo {
        todo!()
    }

    fn is_compatible(&self, val: &TypeInfo, dest: &TypeInfo) -> bool {
        return val == dest;
    }
}

impl ast::ExprVisitor for TypeChecker<'_> {
    fn visit_call_expr(&mut self, expr: &ast::CallExpr) -> () {
        if let &ast::Expr::Identity(ref ident) = &*expr.callee {
            if ident.name.lexeme.as_str() == "print" {
                self.types.insert(
                    expr.callee.id().clone(),
                    TypeInfo::Function(FunctionTypeInfo {
                        params: vec![FnParamTypeInfo {
                            name: "msg".to_string(),
                            type_info: Box::new(TypeInfo::PlainString),
                        }],
                        ret: None,
                    }),
                );
            }
        }

        for arg in &expr.arguments {
            self.resolve_expr(arg);
        }

        let fn_ti = self
            .types
            .get(expr.callee.id())
            .expect("Callee type unknown")
            .clone();

        let TypeInfo::Function(fn_ti) = fn_ti else {
            panic!("Callee not a function")
        };

        let mut idx = 0;

        for param in &fn_ti.params {
            if let Some(arg) = expr.arguments.get(idx) {
                let arg_ti = self.types.get(arg.id()).expect("Arg type unknown");

                assert!(self.is_compatible(arg_ti, &param.type_info));
            }

            idx += 1;
        }

        if let Some(ret) = &fn_ti.ret {
            self.types.insert(expr.id, (**ret).clone());
        }
    }

    fn visit_get_expr(&mut self, expr: &ast::GetExpr) -> () {}

    fn visit_identity_expr(&mut self, expr: &ast::IdentityExpr) -> () {}

    fn visit_plain_literal_expr(&mut self, expr: &ast::PlainLiteralExpr) -> () {
        self.types.insert(expr.id, TypeInfo::PlainString);
    }

    fn visit_format_string_expr(&mut self, expr: &ast::FormatStringExpr) -> () {
        self.types.insert(expr.id, TypeInfo::PlainString);
    }

    fn visit_logical_expr(&mut self, expr: &ast::LogicalExpr) -> () {}

    fn visit_self_expr(&mut self, expr: &ast::SelfValExpr) -> () {}

    fn visit_unary_expr(&mut self, expr: &ast::UnaryExpr) -> () {}

    fn visit_binary_expr(&mut self, expr: &ast::BinaryExpr) -> () {}
}

impl ast::StmtVisitor for TypeChecker<'_> {
    fn visit_expr_stmt(&mut self, stmt: &ast::ExprStmt) -> () {
        self.resolve_expr(&stmt.expr);
    }

    fn visit_var_decl_stmt(&mut self, stmt: &ast::VarDeclStmt) -> () {
        if let Some(value) = &stmt.value {
            self.resolve_expr(value);

            if let Some(ti) = self.types.get(value.id()) {
                self.types.insert(stmt.id, ti.clone());
            }
        }
    }

    fn visit_assignment_stmt(&mut self, stmt: &ast::AssignmentStmt) -> () {}

    fn visit_for_stmt(&mut self, stmt: &ast::ForStmt) -> () {}

    fn visit_if_stmt(&mut self, stmt: &ast::IfStmt) -> () {}

    fn visit_return_stmt(&mut self, stmt: &ast::ReturnStmt) -> () {}
}

impl ast::DeclVisitor for TypeChecker<'_> {
    fn visit_use_decl(&mut self, decl: &ast::UseDecl) -> () {
        // TODO
        // unimplemented!()
    }

    fn visit_struct_decl(&mut self, decl: &ast::StructDecl) -> () {
        unimplemented!()
    }

    fn visit_function_decl(&mut self, decl: &ast::FunctionDecl) -> () {
        let mut params = vec![];

        for param in &decl.params {
            let name = param.name.lexeme.clone();
            let type_info = Box::new(self.static_path_type(&param.type_ref));

            params.push(FnParamTypeInfo { name, type_info });
        }

        let ret = decl
            .return_type
            .as_ref()
            .map(|typ| Box::new(self.static_path_type(typ)));

        self.types.insert(
            decl.id,
            TypeInfo::Function(FunctionTypeInfo { params, ret }),
        );

        self.resolve_stmts(&decl.body);
    }
}
