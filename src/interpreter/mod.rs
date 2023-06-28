use super::*;

use crate::ast::{self, DeclAccept, DeclVisitor, ExprAccept, ExprVisitor, StmtAccept, StmtVisitor};

use crate::environment as env;
use crate::resolver;
use crate::runtime_value::{self as rt, FerrumCall};
use crate::token;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter {
    pub globals: env::SharedEnvironment<rt::RuntimeValue>,

    ast: ast::AST,
    locals: HashMap<ast::Id, resolver::Distance>,

    environment: env::SharedEnvironment<rt::RuntimeValue>,
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

    fn declare(&mut self, decl: &ast::Decl) -> rt::RuntimeResult<()> {
        return decl.accept(self);
    }

    fn execute(&mut self, stmt: &ast::Stmt) -> rt::RuntimeResult<()> {
        return stmt.accept(self);
    }

    fn evaluate(&mut self, expr: &ast::Expr) -> rt::RuntimeResult {
        return expr.accept(self);
    }

    pub fn execute_block(
        &mut self,
        stmts: &Vec<ast::Stmt>,
        environment: env::SharedEnvironment<rt::RuntimeValue>,
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

    fn define_lhs(
        &mut self,
        lhs: &ast::VarAssignPattern,
        value: rt::RuntimeValue,
    ) -> rt::RuntimeResult<()> {
        match lhs {
            ast::VarAssignPattern::Identity(name) => {
                self.environment.define(name.name.lexeme.clone(), value)
            }
        }

        return Ok(());
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

    fn add(&self, left: rt::RuntimeValue, right: rt::RuntimeValue) -> rt::RuntimeResult {
        match (left, right) {
            (rt::RuntimeValue::Number(left), rt::RuntimeValue::Number(right)) => {
                return Ok(rt::RuntimeValue::Number(left + right));
            }
            (rt::RuntimeValue::String(left), rt::RuntimeValue::String(right)) => {
                let mut res = left.to_string();
                res.push_str(&right);
                return Ok(rt::RuntimeValue::String(res.into()));
            }
            (left, right) => {
                return Err(rt::RuntimeError::InvalidBinaryExpr {
                    left,
                    right,
                    op: ast::BinaryOp::Plus,
                    details: Some(format!(
                        "[{}:{}] Can only add 2 strings or 2 numbers.",
                        file!(),
                        line!()
                    )),
                });
            }
        }
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
                                type_ref: ast::TypeRef {
                                    ref_type: None,
                                    static_path: ast::StaticPath {
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
        let value = self.evaluate(&stmt.value)?;

        match &stmt.lhs {
            ast::AssignmentLHS::Var(v) => {
                let Some(distance) = self.locals.get(&stmt.id) else {
                    return Err(rt::RuntimeError::UndefinedVariable { name: v.name.clone(), details: None });
                };

                match &stmt.op.0 {
                    ast::AssignOp::Equal => {
                        self.environment
                            .assign_at(*distance, v.name.clone(), value)?;
                    }
                    ast::AssignOp::PlusEqual => {
                        let prev = self.environment.get_at(*distance, &v.name)?;
                        let next = self.add(prev, value)?;

                        self.environment
                            .assign_at(*distance, v.name.clone(), next)?;
                    }
                }
            }
            ast::AssignmentLHS::Get(_) => unimplemented!(),
        }

        return Ok(());
    }

    fn visit_var_decl_stmt(&mut self, stmt: &ast::VarDeclStmt) -> rt::RuntimeResult<()> {
        let value = if let Some(value) = &stmt.value {
            self.evaluate(value)?
        } else {
            rt::RuntimeValue::Unknown
        };

        return self.define_lhs(&stmt.lhs, value);
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
                let string = arguments
                    .get(0)
                    .map(|arg| arg.to_string())
                    .unwrap_or("None".to_string());

                println!("{string}");
            }
        }

        return function.call(self, arguments);
    }

    fn visit_crash_expr(&mut self, expr: &ast::CrashExpr) -> rt::RuntimeResult {
        panic!(
            r#"Program Crashed! "{}" at {:?}"#,
            expr.error
                .as_ref()
                .map(|e| self.evaluate(&e).unwrap().to_string())
                .unwrap_or(String::new()),
            expr.span
        )
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
        // Ignore opening "
        let mut string = expr.open.lexeme[1..].to_string();

        for part in expr.parts.iter() {
            let part_val = self.evaluate(&part.expr)?;
            string.push_str(&part_val.to_string());
            string.push_str(&part.fmt_str_part.lexeme);
        }

        string.pop(); // Ignore closing "

        return Ok(rt::RuntimeValue::String(string));
    }

    fn visit_logical_expr(&mut self, expr: &ast::LogicalExpr) -> rt::RuntimeResult {
        todo!()
    }

    fn visit_unary_expr(&mut self, expr: &ast::UnaryExpr) -> rt::RuntimeResult {
        let right = self.evaluate(&expr.right)?;

        match &expr.op.0 {
            ast::UnaryOp::Not => Ok(rt::RuntimeValue::Boolean(
                !self.is_truthy(&right).expect("TODO"),
            )),

            ast::UnaryOp::Minus => {
                let rt::RuntimeValue::Number(value) = right else {
                    return Err(rt::RuntimeError::InvalidUnaryExpr {
                        expr: expr.clone(),
                        details: Some(format!("[{}:{}] Can only apply minus unary operator to numbers.", file!(), line!())),
                    });
                };

                return Ok(rt::RuntimeValue::Number(-value));
            }

            ast::UnaryOp::Ref(ref_type) => {
                // TODO: Interpreter doesn't actually handle refs, just shares data
                return Ok(right);
            }
        }
    }

    fn visit_binary_expr(&mut self, expr: &ast::BinaryExpr) -> rt::RuntimeResult {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match &expr.op.0 {
            ast::BinaryOp::Plus => return self.add(left, right),

            // ast::BinaryOp::EqualEqual => {
            //     Ok(rt::RuntimeValue::Boolean(self.is_equal(&left, &right)))
            // }
            // ast::BinaryOp::NotEqual => Ok(rt::RuntimeValue::Boolean(!self.is_equal(&left, &right))),
            op => {
                let rt::RuntimeValue::Number(left_val) = left else {
                    return Err(rt::RuntimeError::InvalidBinaryExpr {
                        left,
                        right,
                        op: op.clone(),
                        details: Some(format!(
                            "[{}:{}] Expected left operand to be a number.",
                            file!(),
                            line!()
                        )),
                    });
                };

                let rt::RuntimeValue::Number(right_val) = right else {
                    return Err(rt::RuntimeError::InvalidBinaryExpr {
                        left,
                        right,
                        op: op.clone(),
                        details: Some(format!(
                            "[{}:{}] Expected right operand to be a number.",
                            file!(),
                            line!()
                        )),
                    });
                };

                return Ok(match op {
                    ast::BinaryOp::Plus | ast::BinaryOp::EqualEqual | ast::BinaryOp::NotEqual => {
                        unreachable!()
                    }

                    ast::BinaryOp::Greater => rt::RuntimeValue::Boolean(left_val > right_val),
                    ast::BinaryOp::GreaterEqual => rt::RuntimeValue::Boolean(left_val >= right_val),
                    ast::BinaryOp::Less => rt::RuntimeValue::Boolean(left_val < right_val),
                    ast::BinaryOp::LessEqual => rt::RuntimeValue::Boolean(left_val <= right_val),

                    ast::BinaryOp::Minus => rt::RuntimeValue::Number(left_val - right_val),
                    ast::BinaryOp::Divide => rt::RuntimeValue::Number(left_val / right_val),
                    ast::BinaryOp::Times => rt::RuntimeValue::Number(left_val * right_val),
                    ast::BinaryOp::Modulo => rt::RuntimeValue::Number(left_val % right_val),

                    op => todo!("Handle {op:?}"),
                });
            }
        }
    }
}
