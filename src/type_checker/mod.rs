mod type_info;

use type_info::*;

use super::*;

use crate::ast::{self, DeclAccept, ExprAccept, StmtAccept};
use crate::environment as env;
use crate::resolver;

use std::collections::HashMap;

use thiserror::Error;

pub type TypeResult<T = TypeInfo, E = TypeError> = Result<T, E>;

#[derive(Debug, Clone)]
pub struct TypeChecker<'a> {
    tree: &'a ast::AST,
    locals: &'a HashMap<ast::Id, resolver::Distance>,

    environment: env::SharedEnvironment<TypeInfo>,
    error_ctx: ErrorContext,
    main_fn: Option<ast::FunctionDecl>,
    known_fn_mod: Option<ast::FnMod>,
    fns_to_eval: Vec<(FunctionTypeInfo, ast::FunctionDecl)>,
    fn_map: HashMap<ast::Id, (FunctionTypeInfo, ast::FunctionDecl)>,
}

impl<'a> TypeChecker<'a> {
    pub fn from_context(
        tree: &'a ast::AST,
        locals: &'a HashMap<ast::Id, resolver::Distance>,
    ) -> Self {
        return Self {
            tree,
            locals,

            environment: env::SharedEnvironment::new(),
            error_ctx: ErrorContext::new(),
            main_fn: None,
            known_fn_mod: None,
            fns_to_eval: vec![],
            fn_map: HashMap::new(),
        };
    }

    pub fn resolve_types(mut self) -> (env::SharedEnvironment<TypeInfo>, ErrorContext) {
        for decl in &self.tree.decls {
            match self.declare(&decl) {
                Err(e) => {
                    self.error_ctx.type_error(e);
                    break;
                }
                _ => {}
            }
        }

        while !self.fns_to_eval.is_empty() {
            for (mut function, decl) in std::mem::take(&mut self.fns_to_eval) {
                self.known_fn_mod = function
                    .known_fn_mod
                    .as_ref()
                    .or_else(|| decl.fn_mod.as_ref())
                    .cloned()
                    .clone();

                match self.execute_body(&decl.body, self.environment.share().shared_enclosed()) {
                    Err(e) => {
                        self.error_ctx.type_error(e);
                        break;
                    }
                    _ => {}
                }

                if function.known_fn_mod.is_none() {
                    function.known_fn_mod = self.known_fn_mod.take();
                } else {
                    self.known_fn_mod = None;
                }

                self.fn_map
                    .insert(function.decl_id, (function.clone(), decl.clone()));

                self.environment.define(
                    function.name.clone(),
                    TypeInfo::Callable(CallableTypeInfo::Function(function)),
                );
            }
        }

        if let Some(main_fn) = self.main_fn {
            if let Some(ast::DeclMod::Pub) = main_fn.decl_mod {
            } else {
                self.error_ctx.type_error(TypeError::TypeMismatch {
                    details: Some(format!("Main function must be public")),
                });
            }

            if let Some(ast::FnMod::Safe | ast::FnMod::Pure) = main_fn.fn_mod {
                self.error_ctx.type_error(TypeError::TypeMismatch {
                    details: Some(format!("Main function cannot be safe or pure")),
                })
            }
        } else {
            self.error_ctx.type_error(TypeError::TypeMismatch {
                details: Some(format!("Main function not found!")),
            })
        }

        return (self.environment, self.error_ctx);
    }

    fn declare(&mut self, decl: &ast::Decl) -> TypeResult<()> {
        return decl.accept(self);
    }

    fn execute(&mut self, stmt: &ast::Stmt) -> TypeResult<()> {
        return stmt.accept(self);
    }

    fn evaluate(&mut self, expr: &ast::Expr) -> TypeResult {
        return expr.accept(self);
    }

    fn execute_body(
        &mut self,
        body: &[ast::Stmt],
        environment: env::SharedEnvironment<TypeInfo>,
    ) -> TypeResult<()> {
        let previous = self.environment.share();

        self.environment = environment;

        let mut try_execute_block = || -> TypeResult<()> {
            for stmt in body {
                self.execute(stmt)?;
            }

            return Ok(());
        };

        let res = try_execute_block();

        self.environment = previous;

        return res;
    }

    fn static_path_type(&mut self, static_path: &ast::StaticPath) -> TypeInfo {
        todo!()
    }

    fn is_compatible(&self, val: &TypeInfo, dest: &TypeInfo) -> bool {
        if *dest == TypeInfo::Nil {
            return true;
        }

        return val == dest;
    }

    fn look_up_variable(&self, name: &token::Token, id: &ast::Id) -> TypeResult {
        if let Some(distance) = self.locals.get(id) {
            return Ok(env::SharedEnvironment::get_at(&self.environment, *distance, name).unwrap());
        }

        if name.lexeme.as_str() == "print" {
            return Ok(TypeInfo::Callable(CallableTypeInfo::Function(
                FunctionTypeInfo {
                    decl_id: ast::Id::MAX,
                    name: "print".to_string(),
                    params: vec![FnParamTypeInfo {
                        name: "msg".to_string(),
                        type_info: Box::new(TypeInfo::PlainString),
                    }],
                    ret: None,
                    known_fn_mod: Some(ast::FnMod::Norm),
                },
            )));
        }

        panic!("Couldn't find {name:?} at {id:?}");
    }

    fn define_lhs(&mut self, lhs: &ast::VarAssignPattern, value: TypeInfo) -> TypeResult<()> {
        match lhs {
            ast::VarAssignPattern::Identity(name) => {
                self.define_var(name, value)?;
            }
        }

        return Ok(());
    }

    fn define_var(&mut self, var: &ast::IdentityExpr, value: TypeInfo) -> TypeResult<()> {
        self.environment.define(var.name.lexeme.clone(), value);

        return Ok(());
    }
}

impl<'a> ast::DeclVisitor<TypeResult<()>> for TypeChecker<'a> {
    fn visit_use_decl(&mut self, decl: &ast::UseDecl) -> TypeResult<()> {
        // TODO
        // unimplemented!()

        return Ok(());
    }

    fn visit_struct_decl(&mut self, decl: &ast::StructDecl) -> TypeResult<()> {
        unimplemented!()
    }

    fn visit_function_decl(&mut self, decl: &ast::FunctionDecl) -> TypeResult<()> {
        let name = decl.name.lexeme.clone();

        if decl.name.lexeme.as_str() == "main" {
            self.main_fn = Some(decl.clone());
        }

        let known_fn_mod = decl.fn_mod.clone();
        self.known_fn_mod = known_fn_mod.clone();

        let function = FunctionTypeInfo {
            decl_id: decl.id,
            name: name.clone(),
            params: vec![],
            ret: None,
            known_fn_mod,
        };

        self.environment.define(
            name.clone(),
            TypeInfo::Callable(CallableTypeInfo::Function(function.clone())),
        );

        self.fn_map
            .insert(function.decl_id, (function.clone(), decl.clone()));
        self.fns_to_eval.push((function, decl.clone()));

        self.known_fn_mod = None;

        return Ok(());
    }
}

impl<'a> ast::StmtVisitor<TypeResult<()>> for TypeChecker<'a> {
    fn visit_expr_stmt(&mut self, stmt: &ast::ExprStmt) -> TypeResult<()> {
        self.evaluate(&stmt.expr)?;

        return Ok(());
    }

    fn visit_var_decl_stmt(&mut self, stmt: &ast::VarDeclStmt) -> TypeResult<()> {
        let value = if let Some(value) = &stmt.value {
            self.evaluate(value)?
        } else {
            TypeInfo::Nil
        };

        return self.define_lhs(&stmt.lhs, value);
    }

    fn visit_assignment_stmt(&mut self, stmt: &ast::AssignmentStmt) -> TypeResult<()> {
        let next = self.evaluate(&stmt.value)?;

        match &stmt.lhs {
            ast::AssignmentLHS::Var(v) => {
                let prev = self.look_up_variable(&v.name, &v.id)?;

                if self.is_compatible(&next, &prev) {
                    self.define_var(v, next)?;
                } else {
                    self.error_ctx.type_error(TypeError::TypeMismatch {
                        details: Some(format!(
                            "Cannot assign {:?} of type {next:?} to {:?} of type {prev:?}.",
                            stmt.value, v.name
                        )),
                    });
                }
            }
            ast::AssignmentLHS::Get(g) => {
                unimplemented!()
            }
        }

        return Ok(());
    }

    fn visit_for_stmt(&mut self, stmt: &ast::ForStmt) -> TypeResult<()> {
        unimplemented!()
    }

    fn visit_if_stmt(&mut self, stmt: &ast::IfStmt) -> TypeResult<()> {
        unimplemented!()
    }

    fn visit_return_stmt(&mut self, stmt: &ast::ReturnStmt) -> TypeResult<()> {
        unimplemented!()
    }
}

impl<'a> ast::ExprVisitor<TypeResult> for TypeChecker<'a> {
    fn visit_call_expr(&mut self, expr: &ast::CallExpr) -> TypeResult {
        let callee = self.evaluate(&expr.callee)?;

        if let TypeInfo::MainFn = callee {
            return Ok(callee);
        }

        let mut arguments = Vec::with_capacity(expr.arguments.len());
        for arg in &expr.arguments {
            arguments.push(self.evaluate(arg)?);
        }

        let TypeInfo::Callable(mut callable) = callee else {
            return Err(TypeError::InvalidCallable {
                value: callee,
                details: Some("Can only call functions and structs".to_string()),
            });
        };
        let callable = match callable {
            CallableTypeInfo::Function(function) => {
                let function = self
                    .fn_map
                    .get(&function.decl_id)
                    .map(|(callable, _)| callable)
                    .cloned()
                    .unwrap_or(function);

                CallableTypeInfo::Function(function)
            }
        };

        dbg!("TODO: compare call expr arg types");

        if arguments.len() != callable.arity() {
            return Err(TypeError::WrongNumberOfArgs {
                expected: callable.arity(),
                found: arguments.len(),
                details: Some(format!("Expr: {expr:?}")),
            });
        }

        // TODO: Allow pure fns to call safe fns only with owned data
        // TODO: safe fn calls unknown fn calls norm function doesn't get picked up.
        // need to propogate checks through until no unknown fns exist

        // dbg!(&self.known_fn_mod, &callable);
        match (&self.known_fn_mod, &callable) {
            (
                None,
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Pure),
                    ..
                }),
            ) => {
                self.known_fn_mod = Some(ast::FnMod::AtLeastPure);
            }
            (
                None,
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Safe),
                    ..
                }),
            ) => {
                self.known_fn_mod = Some(ast::FnMod::AtLeastSafe);
            }
            (
                None,
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Norm),
                    ..
                }),
            ) => {
                self.known_fn_mod = Some(ast::FnMod::Norm);
            }

            _ => {}
        }

        match (&self.known_fn_mod, &callable) {
            (
                Some(ast::FnMod::AtLeastPure),
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Safe),
                    ..
                }),
            ) => {
                self.known_fn_mod = Some(ast::FnMod::AtLeastSafe);
            }

            (
                Some(ast::FnMod::AtLeastPure | ast::FnMod::AtLeastSafe),
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Norm),
                    ..
                }),
            ) => {
                self.known_fn_mod = Some(ast::FnMod::Norm);
            }

            _ => {}
        }

        match (&self.known_fn_mod, &callable) {
            (
                Some(ast::FnMod::Norm),
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Risk),
                    ..
                }),
            ) => {
                self.error_ctx.type_error(TypeError::TypeMismatch {
                    details: Some(format!("Norm fns can only call pure, safe, and norm fns")),
                });
            }

            (
                Some(ast::FnMod::Safe | ast::FnMod::AtLeastSafe),
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Risk | ast::FnMod::Norm),
                    ..
                }),
            ) => {
                self.error_ctx.type_error(TypeError::TypeMismatch {
                    details: Some(format!("Safe fns can only call pure and safe fns")),
                });
            }

            (
                None,
                CallableTypeInfo::Function(FunctionTypeInfo {
                    known_fn_mod: Some(ast::FnMod::Risk),
                    ..
                }),
            ) => {
                self.error_ctx.type_error(TypeError::TypeMismatch {
                    details: Some(format!(
                        "Fns must be marked as risk in order to call risk fns"
                    )),
                });
            }
            (
                _,
                CallableTypeInfo::Function(
                    function @ FunctionTypeInfo {
                        decl_id,
                        known_fn_mod: None,
                        ..
                    },
                ),
            ) => {
                if let Some((_, decl)) = self.fn_map.get(decl_id) {
                    // dbg!(&function, &decl);

                    let mut function = function.clone();
                    function.known_fn_mod = self.known_fn_mod.clone();
                    // dbg!(&function);

                    self.fns_to_eval.push((function.clone(), decl.clone()));
                }
            }

            _ => {}
        };

        let ret = match callable {
            CallableTypeInfo::Function(FunctionTypeInfo { ret: Some(ret), .. }) => *ret,
            CallableTypeInfo::Function(FunctionTypeInfo { ret: None, .. }) => TypeInfo::Nil,
        };

        return Ok(ret);
    }

    fn visit_crash_expr(&mut self, _: &ast::CrashExpr) -> TypeResult {
        if let Some(ast::FnMod::Risk) = self.known_fn_mod {
            return Ok(TypeInfo::Nil);
        } else {
            return Err(TypeError::TypeMismatch {
                details: Some(format!("Function must be marked as a risk")),
            });
        }
    }

    fn visit_get_expr(&mut self, expr: &ast::GetExpr) -> TypeResult {
        unimplemented!()
    }

    fn visit_identity_expr(&mut self, expr: &ast::IdentityExpr) -> TypeResult {
        return self.look_up_variable(&expr.name, &expr.id);
    }

    fn visit_plain_literal_expr(&mut self, expr: &ast::PlainLiteralExpr) -> TypeResult {
        match expr.literal_type {
            ast::PlainLiteralType::PlainString => return Ok(TypeInfo::PlainString),
            ast::PlainLiteralType::Number => return Ok(TypeInfo::Number),
            _ => todo!(),
        }
    }

    fn visit_format_string_expr(&mut self, expr: &ast::FormatStringExpr) -> TypeResult {
        for part in &expr.parts {
            let TypeInfo::PlainString = self.evaluate(&part.expr)? else {
                return Err(TypeError::TypeMismatch { details: Some(format!("Expected part to be a string: {part:?}")) });
            };
        }

        return Ok(TypeInfo::PlainString);
    }

    fn visit_logical_expr(&mut self, expr: &ast::LogicalExpr) -> TypeResult {
        unimplemented!()
    }

    fn visit_self_expr(&mut self, expr: &ast::SelfValExpr) -> TypeResult {
        unimplemented!()
    }

    fn visit_unary_expr(&mut self, expr: &ast::UnaryExpr) -> TypeResult {
        unimplemented!()
    }

    fn visit_binary_expr(&mut self, expr: &ast::BinaryExpr) -> TypeResult {
        unimplemented!()
    }
}

#[derive(Error, Clone, Debug, PartialEq)]
pub enum TypeError {
    #[error("Type mismatch. Details = {details:?}")]
    TypeMismatch { details: Option<String> },

    #[error("invalid callable: {value:#?}. Details = {details:?}")]
    InvalidCallable {
        value: TypeInfo,
        details: Option<String>,
    },

    #[error("function expected {expected} args, but call found {found}. Details = {details:?}")]
    WrongNumberOfArgs {
        expected: usize,
        found: usize,
        details: Option<String>,
    },
}
