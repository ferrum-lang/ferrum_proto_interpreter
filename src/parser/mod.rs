use super::*;

use crate::ast;
use crate::token;

#[derive(Clone, Debug, PartialEq)]
pub struct ParserError {
    pub message: String,
}

pub type Result<T = ()> = std::result::Result<T, ParserError>;

#[derive(Clone, Debug, PartialEq)]
pub struct Parser {
    tokens: Vec<token::Token>,

    current_idx: usize,
    stmts: Vec<ast::Stmt>,
    error_ctx: ErrorContext,

    next_expr_id: ast::ExprId,
}

impl Parser {
    pub fn from_tokens(tokens: Vec<token::Token>) -> Self {
        return Self {
            tokens,

            current_idx: 0,
            stmts: Vec::new(),
            error_ctx: ErrorContext::new(),

            next_expr_id: 0,
        };
    }

    pub fn parse_ast(mut self) -> (ast::AST, ErrorContext) {
        while !self.is_at_end() {
            let _ = self.match_any(&[token::TokenType::Newline, token::TokenType::EOF]);

            if let Some(statement) = self.declaration() {
                self.stmts.push(statement);
            }
        }

        return (ast::AST { stmts: self.stmts }, self.error_ctx);
    }

    fn declaration(&mut self) -> Option<ast::Stmt> {
        fn try_declaration(this: &mut Parser) -> Result<ast::Stmt> {
            /*
            if this.match_any(&[token::TokenType::Struct]) {
                return this.struct_declaration();
            }

            if this.match_any(&[token::TokenType::Fn]) {
                return Ok(ast::Stmt::Function(this.function("function".to_string())?));
            }
            */

            return this.statement();
        }

        match try_declaration(self) {
            Ok(stmt) => return Some(stmt),

            Err(_e) => {
                self.synchronize();
                return None;
            }
        }
    }

    fn statement(&mut self) -> Result<ast::Stmt> {
        return self.expression_statement();
    }

    fn expression_statement(&mut self) -> Result<ast::Stmt> {
        let expr = self.expression()?;
        self.consume(
            &token::TokenType::Newline,
            "Expect newline after expression.",
        )?;

        return Ok(ast::Stmt::Expression(ast::ExpressionStmt { expr }));
    }

    fn expression(&mut self) -> Result<ast::Expr> {
        return self.assignment();
    }

    fn assignment(&mut self) -> Result<ast::Expr> {
        let expr = self.or()?;

        if self.match_any(&[token::TokenType::Equal]) {
            let equals = self.previous().cloned().ok_or_else(|| self.eof_err())?;
            let value = self.assignment()?;

            return Err(self.error(
                format!("[{}:{}] Invalid assignment target", file!(), line!()),
                equals,
            ));
        }

        return Ok(expr);
    }

    fn or(&mut self) -> Result<ast::Expr> {
        let mut expr = self.and()?;

        while self.match_any(&[token::TokenType::Or]) {
            let operator = self.previous().cloned().ok_or_else(|| self.eof_err())?;
            let right = self.and()?;

            expr = ast::Expr::Logical(ast::LogicalExpr {
                id: self.expr_id(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        return Ok(expr);
    }

    fn and(&mut self) -> Result<ast::Expr> {
        let mut expr = self.equality()?;

        while self.match_any(&[token::TokenType::And]) {
            let operator = self.previous().cloned().ok_or_else(|| self.eof_err())?;
            let right = self.equality()?;

            expr = ast::Expr::Logical(ast::LogicalExpr {
                id: self.expr_id(),
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        return Ok(expr);
    }

    fn equality(&mut self) -> Result<ast::Expr> {
        let mut expr = self.comparison()?;

        return Ok(expr);
    }

    fn comparison(&mut self) -> Result<ast::Expr> {
        let mut expr = self.term()?;

        return Ok(expr);
    }

    fn term(&mut self) -> Result<ast::Expr> {
        let mut expr = self.factor()?;

        return Ok(expr);
    }

    fn factor(&mut self) -> Result<ast::Expr> {
        let mut expr = self.unary()?;

        return Ok(expr);
    }

    fn unary(&mut self) -> Result<ast::Expr> {
        return self.call();
    }

    fn call(&mut self) -> Result<ast::Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.match_any(&[token::TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_any(&[token::TokenType::Dot]) {
                let name =
                    self.consume(&token::TokenType::Identifier, "Expect property name '.'")?;

                expr = ast::Expr::Get(ast::GetExpr {
                    id: self.expr_id(),
                    object: Box::new(expr),
                    name,
                });
            } else {
                break;
            }
        }

        return Ok(expr);
    }

    fn finish_call(&mut self, callee: ast::Expr) -> Result<ast::Expr> {
        let mut arguments = vec![];

        if !self.check(&token::TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let t = self.peek().cloned().ok_or_else(|| self.eof_err())?;
                    return Err(self.error("Can't have more than 255 arguments".to_string(), t));
                }

                arguments.push(self.expression()?);

                if !self.match_any(&[token::TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            &token::TokenType::RightParen,
            "Expect ')' after arguments".to_string(),
        )?;

        return Ok(ast::Expr::Call(ast::CallExpr {
            id: self.expr_id(),
            callee: Box::new(callee),
            arguments,
        }));
    }

    fn primary(&mut self) -> Result<ast::Expr> {
        match self.advance().cloned() {
            Some(
                t @ token::Token {
                    token_type:
                        token::TokenType::False
                        | token::TokenType::True
                        | token::TokenType::Number
                        | token::TokenType::Char
                        | token::TokenType::PlainString,
                    ..
                },
            ) => {
                let literal_type = match t.token_type {
                    token::TokenType::False => ast::PlainLiteralType::False,
                    token::TokenType::True => ast::PlainLiteralType::True,
                    token::TokenType::Number => ast::PlainLiteralType::Number,
                    token::TokenType::Char => ast::PlainLiteralType::Char,
                    token::TokenType::PlainString => ast::PlainLiteralType::PlainString,
                    _ => unreachable!(),
                };

                return Ok(ast::Expr::PlainLiteral(ast::PlainLiteralExpr {
                    id: self.expr_id(),
                    literal_type,
                    token: t,
                }));
            }

            Some(
                open_token @ token::Token {
                    token_type: token::TokenType::FormatStringOpen,
                    ..
                },
            ) => {
                let mut parts = vec![];

                loop {
                    let left_token = self.consume(
                        &token::TokenType::LeftBrace,
                        "Expected '{' in format string",
                    )?;

                    let expr = self.expression()?;

                    let right_token = self.consume(
                        &token::TokenType::RightBrace,
                        "Expected '}' in format string",
                    )?;

                    let fmt_str_part = self.peek().cloned().ok_or_else(|| self.eof_err())?;
                    let is_done = match fmt_str_part.token_type {
                        token::TokenType::FormatStringMid => false,
                        token::TokenType::FormatStringClose => true,

                        _ => {
                            return Err(self.error(
                                "Expected part of a format string.".to_string(),
                                fmt_str_part,
                            ))
                        }
                    };
                    let _ = self.advance();

                    parts.push(ast::FormatStringExprPart {
                        left_brace: left_token,
                        expr: Box::new(expr),
                        right_brace: right_token,
                        fmt_str_part,
                    });

                    if is_done {
                        break;
                    }
                }

                return Ok(ast::Expr::FormatString(ast::FormatStringExpr {
                    id: self.expr_id(),
                    open: open_token,
                    parts,
                }));
            }

            Some(
                t @ token::Token {
                    token_type: token::TokenType::SelfVal,
                    ..
                },
            ) => {
                return Ok(ast::Expr::SelfVal(ast::SelfValExpr {
                    id: self.expr_id(),
                    keyword: t,
                }));
            }

            Some(
                t @ token::Token {
                    token_type: token::TokenType::Identifier,
                    ..
                },
            ) => {
                return Ok(ast::Expr::Identity(ast::IdentityExpr {
                    id: self.expr_id(),
                    name: t,
                }));
            }

            Some(peek) => {
                return Err(self.error(
                    format!("[{}:{}] Expected some expression.", file!(), line!()),
                    peek,
                ));
            }

            None => return Err(self.eof_err()),
        }
    }

    fn match_any(&mut self, token_types: &[token::TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    fn consume(
        &mut self,
        token_type: &token::TokenType,
        err_msg: impl Into<String>,
    ) -> Result<token::Token> {
        if self.check(token_type) {
            return self.advance().cloned().ok_or_else(|| self.eof_err());
        }

        let t = self.peek().cloned().ok_or_else(|| self.eof_err())?;

        return Err(self.error(err_msg.into(), t));
    }

    fn eof_err(&mut self) -> ParserError {
        let message = format!("[{}:{}] Unexpected end of file.", file!(), line!());
        self.error_ctx.error(Span::new(), message.clone());

        return ParserError { message };
    }

    fn error(&mut self, message: String, t: token::Token) -> ParserError {
        self.error_ctx.token_error(t, message.clone());

        return ParserError { message };
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let Some(prev) = self.previous() {
                match prev.token_type {
                    token::TokenType::Semicolon
                    | token::TokenType::Newline
                    | token::TokenType::EOF => return,
                    _ => {}
                }
            }

            if let Some(peek) = self.peek() {
                match peek.token_type {
                    token::TokenType::Const
                    | token::TokenType::For
                    | token::TokenType::Fn
                    | token::TokenType::If
                    | token::TokenType::Impl
                    | token::TokenType::Match
                    | token::TokenType::Return
                    | token::TokenType::Struct
                    | token::TokenType::Trait
                    | token::TokenType::Type
                    | token::TokenType::Use
                    | token::TokenType::While => return,

                    token::TokenType::LeftParen
                    | token::TokenType::RightParen
                    | token::TokenType::LeftBrace
                    | token::TokenType::RightBrace
                    | token::TokenType::LeftBracket
                    | token::TokenType::RightBracket
                    | token::TokenType::Comma
                    | token::TokenType::Semicolon
                    | token::TokenType::At
                    | token::TokenType::Dollar
                    | token::TokenType::Dot
                    | token::TokenType::DotDot
                    | token::TokenType::Colon
                    | token::TokenType::ColonColon
                    | token::TokenType::Minus
                    | token::TokenType::MinusEqual
                    | token::TokenType::MinusGreater
                    | token::TokenType::Plus
                    | token::TokenType::PlusEqual
                    | token::TokenType::Slash
                    | token::TokenType::SlashEqual
                    | token::TokenType::Asterisk
                    | token::TokenType::AsteriskEqual
                    | token::TokenType::Percent
                    | token::TokenType::PercentEqual
                    | token::TokenType::Carat
                    | token::TokenType::CaratEqual
                    | token::TokenType::Bang
                    | token::TokenType::BangEqual
                    | token::TokenType::Equal
                    | token::TokenType::EqualEqual
                    | token::TokenType::EqualGreater
                    | token::TokenType::Greater
                    | token::TokenType::GreaterGreater
                    | token::TokenType::GreaterEqual
                    | token::TokenType::GreaterDot
                    | token::TokenType::Less
                    | token::TokenType::LessEqual
                    | token::TokenType::Tilde
                    | token::TokenType::TildeEqual
                    | token::TokenType::Question
                    | token::TokenType::QuestionQuestion
                    | token::TokenType::Amp
                    | token::TokenType::AmpAmp
                    | token::TokenType::Hash
                    | token::TokenType::HashLeftBrace
                    | token::TokenType::HashLeftBracket
                    | token::TokenType::BarBar
                    | token::TokenType::Identifier
                    | token::TokenType::PlainString
                    | token::TokenType::FormatStringOpen
                    | token::TokenType::FormatStringMid
                    | token::TokenType::FormatStringClose
                    | token::TokenType::Char
                    | token::TokenType::Number
                    | token::TokenType::And
                    | token::TokenType::As
                    | token::TokenType::Else
                    | token::TokenType::False
                    | token::TokenType::In
                    | token::TokenType::Mut
                    | token::TokenType::Or
                    | token::TokenType::Pub
                    | token::TokenType::SelfVal
                    | token::TokenType::SelfType
                    | token::TokenType::True
                    | token::TokenType::Yield
                    | token::TokenType::Newline
                    | token::TokenType::EOF => {}
                }
            }

            self.advance();
        }
    }

    fn check(&self, token_type: &token::TokenType) -> bool {
        return self
            .peek()
            .map(|peek| peek.token_type == *token_type)
            .unwrap_or(false);
    }

    fn advance(&mut self) -> Option<&token::Token> {
        if !self.is_at_end() {
            self.current_idx += 1;
        }

        return self.previous();
    }

    fn is_at_end(&self) -> bool {
        return self
            .peek()
            .map(|peek| peek.token_type == token::TokenType::EOF)
            .unwrap_or(true);
    }

    fn peek(&self) -> Option<&token::Token> {
        return self.tokens.get(self.current_idx);
    }

    fn previous(&self) -> Option<&token::Token> {
        if self.current_idx == 0 {
            return None;
        }

        return self.tokens.get(self.current_idx - 1);
    }

    fn expr_id(&mut self) -> ast::ExprId {
        let id = self.next_expr_id;

        self.next_expr_id += 1;

        return id;
    }
}
