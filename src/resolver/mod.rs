use super::*;

use crate::ast::{self, DeclAccept, ExprAccept, ExprVisitor, StmtAccept};
use crate::token;

use std::collections::HashMap;

pub type Distance = usize;

#[derive(Debug, Clone, PartialEq)]
enum FunctionType {
    None,
    Function,
    Impl,
}

#[derive(Debug, Clone, PartialEq)]
enum StructType {
    None,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Resolver<'a> {
    tree: &'a ast::AST,

    locals: HashMap<ast::Id, Distance>,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_struct: StructType,
    fns_to_resolve: Vec<(ast::FunctionDecl, FunctionType)>,
    error_ctx: ErrorContext,
}

impl<'a> Resolver<'a> {
    pub fn from_ast(tree: &'a ast::AST) -> Self {
        return Self {
            tree,

            locals: HashMap::new(),
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_struct: StructType::None,
            fns_to_resolve: vec![],
            error_ctx: ErrorContext::new(),
        };
    }

    pub fn resolve_locals(mut self) -> (HashMap<ast::Id, Distance>, ErrorContext) {
        self.resolve_decls(&self.tree.decls);

        return (self.locals, self.error_ctx);
    }

    fn resolve_decls(&mut self, decls: &Vec<ast::Decl>) {
        self.begin_scope();

        for decl in decls {
            self.resolve_decl(decl);
        }

        for (decl, function_type) in self.fns_to_resolve.clone() {
            self.resolve_function(&decl, function_type);
        }

        self.end_scope();
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

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &token::Token) {
        if let Some(scope) = self.scopes.last_mut() {
            // TODO: Handle variable shadowing
            if scope.contains_key(&name.lexeme) {
                self.error_ctx.token_error(
                    name.clone(),
                    "Already a variable with this name in this scope",
                );
            }

            scope.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &token::Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_local(&mut self, id: ast::Id, name: &token::Token) {
        if self.scopes.len() == 0 {
            return;
        }

        let mut i = self.scopes.len() - 1;

        loop {
            if self.scopes[i].contains_key(&name.lexeme) {
                self.locals.insert(id, self.scopes.len() - 1 - i);

                break;
            }

            if i == 0 {
                break;
            }

            i -= 1;
        }
    }

    fn resolve_function(&mut self, function: &ast::FunctionDecl, function_type: FunctionType) {
        let enclosing = std::mem::replace(&mut self.current_function, function_type);

        self.begin_scope();

        for param in &function.params {
            self.declare(&param.name);
            self.define(&param.name);
        }

        self.resolve_stmts(&function.body);

        self.end_scope();

        self.current_function = enclosing;
    }
}

impl ast::ExprVisitor for Resolver<'_> {
    fn visit_call_expr(&mut self, expr: &ast::CallExpr) -> () {
        self.resolve_expr(&expr.callee);

        for argument in &expr.arguments {
            self.resolve_expr(&argument);
        }
    }

    fn visit_crash_expr(&mut self, expr: &ast::CrashExpr) -> () {
        // NO-OP
    }

    fn visit_get_expr(&mut self, expr: &ast::GetExpr) -> () {
        self.resolve_expr(&expr.object);
    }

    fn visit_identity_expr(&mut self, expr: &ast::IdentityExpr) -> () {
        if let Some(scope) = self.scopes.last() {
            if let Some(false) = scope.get(&expr.name.lexeme) {
                self.error_ctx.token_error(
                    expr.name.clone(),
                    "Can't read a local variable in its own initializer",
                );
            }
        }

        self.resolve_local(expr.id, &expr.name);
    }

    fn visit_plain_literal_expr(&mut self, expr: &ast::PlainLiteralExpr) -> () {
        // NO-OP
    }

    fn visit_format_string_expr(&mut self, expr: &ast::FormatStringExpr) -> () {
        for part in &expr.parts {
            self.resolve_expr(&part.expr);
        }
    }

    fn visit_logical_expr(&mut self, expr: &ast::LogicalExpr) -> () {
        unimplemented!()
    }

    fn visit_self_expr(&mut self, expr: &ast::SelfValExpr) -> () {
        unimplemented!()
    }

    fn visit_unary_expr(&mut self, expr: &ast::UnaryExpr) -> () {
        self.resolve_expr(&expr.right);
    }

    fn visit_binary_expr(&mut self, expr: &ast::BinaryExpr) -> () {
        self.resolve_expr(&expr.left);
        self.resolve_expr(&expr.right);
    }
}

impl ast::StmtVisitor for Resolver<'_> {
    fn visit_expr_stmt(&mut self, stmt: &ast::ExprStmt) -> () {
        self.resolve_expr(&stmt.expr);
    }

    fn visit_var_decl_stmt(&mut self, stmt: &ast::VarDeclStmt) -> () {
        match &stmt.lhs {
            ast::VarAssignPattern::Identity(ident) => self.declare(&ident.name),
        }

        if let Some(value) = &stmt.value {
            self.resolve_expr(value);
        }

        match &stmt.lhs {
            ast::VarAssignPattern::Identity(ident) => self.define(&ident.name),
        }
    }

    fn visit_assignment_stmt(&mut self, stmt: &ast::AssignmentStmt) -> () {
        self.resolve_expr(&stmt.value);

        match &stmt.lhs {
            ast::AssignmentLHS::Get(expr) => self.visit_get_expr(&expr),
            ast::AssignmentLHS::Var(expr) => {
                self.visit_identity_expr(&expr);
                self.resolve_local(stmt.id, &expr.name);
            }
        }
    }

    fn visit_for_stmt(&mut self, stmt: &ast::ForStmt) -> () {
        unimplemented!()
    }

    fn visit_if_stmt(&mut self, stmt: &ast::IfStmt) -> () {
        self.resolve_expr(&stmt.condition);
        self.resolve_stmts(&stmt.then_branch);

        match &stmt.else_branch {
            Some(ast::ElseBranch::ElseIf(elif)) => self.visit_if_stmt(&elif),
            Some(ast::ElseBranch::Block(else_branch)) => self.resolve_stmts(else_branch),
            None => {}
        }
    }

    fn visit_return_stmt(&mut self, stmt: &ast::ReturnStmt) -> () {
        unimplemented!()
    }
}

impl ast::DeclVisitor for Resolver<'_> {
    fn visit_use_decl(&mut self, decl: &ast::UseDecl) -> () {
        // TODO
        // unimplemented!()
    }

    fn visit_struct_decl(&mut self, decl: &ast::StructDecl) -> () {
        unimplemented!()
    }

    fn visit_function_decl(&mut self, decl: &ast::FunctionDecl) -> () {
        self.declare(&decl.name);
        self.define(&decl.name);
        // self.resolve_function(&decl, FunctionType::Function);
        self.fns_to_resolve
            .push((decl.clone(), FunctionType::Function));
    }
}
