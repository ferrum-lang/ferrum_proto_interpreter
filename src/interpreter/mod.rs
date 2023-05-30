use super::*;

use crate::ast::{self, DeclAccept, DeclVisitor, ExprAccept, ExprVisitor, StmtAccept, StmtVisitor};

use crate::environment as env;
use crate::resolver;
use crate::runtime_value::{self as rt, FerrumCall};
use crate::token;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter {
    pub globals: env::SharedEnvironment,

    ast: ast::AST,
    locals: HashMap<ast::Id, resolver::Distance>,

    environment: env::SharedEnvironment,
    error_ctx: ErrorContext,
    main_fn: Option<rt::FerrumFunction>,
}

impl Interpreter {
    pub fn from_context(ast: ast::AST, locals: HashMap<ast::Id, resolver::Distance>) -> Self {
        let globals = env::SharedEnvironment::new();
        // globals.define("clock".to_string(), rt::RuntimeValue::LoxCallable(rt::LoxCallable::Clock(Clock)));

        let environment = globals.share();

        return Self {
            globals,

            ast,
            locals,

            environment,
            error_ctx: ErrorContext::new(),
            main_fn: None,
        };
    }

    pub fn interpret(mut self) -> ErrorContext {
        for decl in self.ast.decls.clone() {
            match self.declare(&decl) {
                Err(e) => {
                    self.error_ctx.runtime_error(e);
                    break;
                }
                _ => {}
            }
        }

        if let Some(_) = self.main_fn {
            if let Err(e) = self.evaluate(&ast::Expr::Call(ast::CallExpr {
                id: ast::Id::MAX,
                callee: Box::new(ast::Expr::Identity(ast::IdentityExpr {
                    id: ast::Id::MAX,
                    name: token::Token {
                        token_type: token::TokenType::Identifier,
                        lexeme: "main".to_string(),
                        span: Span::from_position(Position {
                            index: std::usize::MAX,
                            line: 0,
                            column: 0,
                        }),
                    },
                })),
                arguments: vec![],
            })) {
                self.error_ctx.runtime_error(e);
            }
        }

        return self.error_ctx;
    }

    pub fn declare(&mut self, decl: &ast::Decl) -> rt::RuntimeResult<()> {
        return decl.accept(self);
    }

    pub fn execute(&mut self, stmt: &ast::Stmt) -> rt::RuntimeResult<()> {
        return stmt.accept(self);
    }

    pub fn evaluate(&mut self, expr: &ast::Expr) -> rt::RuntimeResult {
        return expr.accept(self);
    }

    pub fn execute_block(
        &mut self,
        stmts: &Vec<ast::Stmt>,
        environment: env::SharedEnvironment,
    ) -> rt::RuntimeResult<()> {
        let previous = self.environment.share();

        self.environment = environment;

        let mut try_execute_block = || -> rt::RuntimeResult<()> {
            for stmt in stmts {
                self.execute(stmt)?;
            }

            return Ok(());
        };

        let res = try_execute_block();

        self.environment = previous;

        return res;
    }

    fn look_up_variable(&self, name: &token::Token, id: &ast::Id) -> rt::RuntimeResult {
        if let Some(distance) = self.locals.get(id) {
            return env::SharedEnvironment::get_at(&self.environment, *distance, name);
        } else {
            return self.globals.get(name);
        }
    }

    fn is_truthy(&self, value: &rt::RuntimeValue) -> Option<bool> {
        if let rt::RuntimeValue::Boolean(value) = value {
            return Some(*value);
        }

        return None;
    }
}

impl DeclVisitor<rt::RuntimeResult<()>> for Interpreter {
    fn visit_use_decl(&mut self, decl: &ast::UseDecl) -> rt::RuntimeResult<()> {
        if let ast::UseDecl {
            path:
                ast::StaticUsePath {
                    name:
                        token::Token {
                            token_type: token::TokenType::Identifier,
                            lexeme,
                            ..
                        },
                    nexts,
                    ..
                },
            decl_mod,
            ..
        } = decl
        {
            if lexeme.as_str() == "fe" {
                for next in nexts {
                    if next.name.token_type == token::TokenType::Identifier
                        && next.name.lexeme.as_str() == "print"
                        && next.nexts.is_empty()
                    {
                        self.declare(&ast::Decl::Function(ast::FunctionDecl {
                            id: ast::Id::MAX,
                            decl_mod: decl_mod.clone(),
                            fn_mod: None,
                            name: next.name.clone(),
                            params: vec![ast::FnParam {
                                name: token::Token {
                                    lexeme: "text".to_string(),
                                    token_type: token::TokenType::Identifier,
                                    span: Span::from_position(Position {
                                        index: std::usize::MAX,
                                        line: 0,
                                        column: 0,
                                    }),
                                },
                                type_ref: ast::StaticPath {
                                    root: None,
                                    name: token::Token {
                                        lexeme: "String".to_string(),
                                        token_type: token::TokenType::Identifier,
                                        span: Span::from_position(Position {
                                            index: std::usize::MAX,
                                            line: 0,
                                            column: 0,
                                        }),
                                    },
                                },
                            }],
                            return_type: None,
                            body: vec![],
                        }))?;

                        return Ok(());
                    }
                }
            }
        }

        println!("TODO: Handle: {decl:?}");

        return Ok(());
    }

    fn visit_struct_decl(&mut self, decl: &ast::StructDecl) -> rt::RuntimeResult<()> {
        let name = decl.name.lexeme.clone();

        // let mut fields = HashMap::new();
        // for field in &decl.fields {
        //     fields.insert(field.name.lexeme.clone(), field.clone());
        // }

        let struct_ = rt::FerrumStruct { name: name.clone() };

        self.environment.define(
            name,
            rt::RuntimeValue::Callable(rt::FerrumCallable::Struct(struct_)),
        );

        return Ok(());
    }

    fn visit_function_decl(&mut self, decl: &ast::FunctionDecl) -> rt::RuntimeResult<()> {
        let name = decl.name.lexeme.clone();

        let function = rt::FerrumFunction::new(decl.clone(), self.environment.share());

        if function.decl.name.lexeme.as_str() == "main" {
            self.main_fn = Some(function.clone());
        }

        self.environment.define(
            name,
            rt::RuntimeValue::Callable(rt::FerrumCallable::Function(function)),
        );

        return Ok(());
    }
}

impl StmtVisitor<rt::RuntimeResult<()>> for Interpreter {
    fn visit_return_stmt(&mut self, stmt: &ast::ReturnStmt) -> rt::RuntimeResult<()> {
        let value = if let Some(value) = &stmt.value {
            Some(self.evaluate(value)?)
        } else {
            None
        };

        return Err(rt::RuntimeError::NonErrorReturnShortCircuit { value });
    }

    fn visit_for_stmt(&mut self, stmt: &ast::ForStmt) -> rt::RuntimeResult<()> {
        todo!()
    }

    fn visit_if_stmt(&mut self, stmt: &ast::IfStmt) -> rt::RuntimeResult<()> {
        let mut condition = self.evaluate(&stmt.condition)?;

        if let Some(true) = self.is_truthy(&mut condition) {
            self.execute_block(
                &stmt.then_branch,
                self.environment.share().shared_enclosed(),
            )?;
        } else {
            match &stmt.else_branch {
                Some(ast::ElseBranch::Block(stmts)) => {
                    self.execute_block(stmts, self.environment.share().shared_enclosed())?;
                }
                Some(ast::ElseBranch::ElseIf(elif)) => {
                    todo!()
                }
                _ => {}
            }
        }

        return Ok(());
    }

    fn visit_assignment_stmt(&mut self, stmt: &ast::AssignmentStmt) -> rt::RuntimeResult<()> {
        todo!()
    }

    fn visit_var_decl_stmt(&mut self, stmt: &ast::VarDeclStmt) -> rt::RuntimeResult<()> {
        todo!()
    }

    fn visit_expr_stmt(&mut self, stmt: &ast::ExprStmt) -> rt::RuntimeResult<()> {
        self.evaluate(&stmt.expr)?;

        return Ok(());
    }
}

impl ExprVisitor<rt::RuntimeResult> for Interpreter {
    fn visit_self_expr(&mut self, expr: &ast::SelfValExpr) -> rt::RuntimeResult {
        todo!()
    }

    fn visit_call_expr(&mut self, expr: &ast::CallExpr) -> rt::RuntimeResult {
        let callee = self.evaluate(&expr.callee)?;

        let mut arguments = Vec::with_capacity(expr.arguments.len());
        for arg in &expr.arguments {
            arguments.push(self.evaluate(arg)?);
        }

        let rt::RuntimeValue::Callable(mut function) = callee else {
            return Err(rt::RuntimeError::InvalidCallable {
                value: callee,
                details: Some("Can only call functions and structs".to_string()),
            });
        };

        if arguments.len() != function.arity() {
            return Err(rt::RuntimeError::WrongNumberOfArgs {
                expected: function.arity(),
                found: arguments.len(),
                details: Some(format!("Expr: {expr:?}")),
            });
        }

        if let rt::FerrumCallable::Function(function) = &function {
            if function.decl.name.lexeme.as_str() == "print" {
                println!("{:?}", arguments.get(0));
            }
        }

        return function.call(self, arguments);
    }

    fn visit_get_expr(&mut self, expr: &ast::GetExpr) -> rt::RuntimeResult {
        todo!()
    }

    fn visit_identity_expr(&mut self, expr: &ast::IdentityExpr) -> rt::RuntimeResult {
        return self.look_up_variable(&expr.name, &expr.id);
    }

    fn visit_plain_literal_expr(&mut self, expr: &ast::PlainLiteralExpr) -> rt::RuntimeResult {
        return Ok(rt::RuntimeValue::from_expr(expr));
    }

    fn visit_format_string_expr(&mut self, expr: &ast::FormatStringExpr) -> rt::RuntimeResult {
        todo!()
    }

    fn visit_logical_expr(&mut self, expr: &ast::LogicalExpr) -> rt::RuntimeResult {
        todo!()
    }

    fn visit_unary_expr(&mut self, expr: &ast::UnaryExpr) -> rt::RuntimeResult {
        todo!()
    }

    fn visit_binary_expr(&mut self, expr: &ast::BinaryExpr) -> rt::RuntimeResult {
        todo!()
    }
}
