use super::*;

use crate::token;

use std::collections::HashMap;

use lazy_static;

lazy_static::lazy_static! {
    static ref KEYWORDS: HashMap<String, token::TokenType> = {
        let mut keywords = HashMap::new();
        keywords.insert("and".to_string(), token::TokenType::And);
        keywords.insert("as".to_string(), token::TokenType::As);
        keywords.insert("const".to_string(), token::TokenType::Const);
        keywords.insert("else".to_string(), token::TokenType::Else);
        keywords.insert("false".to_string(), token::TokenType::False);
        keywords.insert("fn".to_string(), token::TokenType::Fn);
        keywords.insert("for".to_string(), token::TokenType::For);
        keywords.insert("if".to_string(), token::TokenType::If);
        keywords.insert("impl".to_string(), token::TokenType::Impl);
        keywords.insert("in".to_string(), token::TokenType::In);
        keywords.insert("match".to_string(), token::TokenType::Match);
        keywords.insert("mut".to_string(), token::TokenType::Mut);
        keywords.insert("or".to_string(), token::TokenType::Or);
        keywords.insert("pub".to_string(), token::TokenType::Pub);
        keywords.insert("return".to_string(), token::TokenType::Return);
        keywords.insert("self".to_string(), token::TokenType::SelfVal);
        keywords.insert("Self".to_string(), token::TokenType::SelfType);
        keywords.insert("struct".to_string(), token::TokenType::Struct);
        keywords.insert("trait".to_string(), token::TokenType::Trait);
        keywords.insert("true".to_string(), token::TokenType::True);
        keywords.insert("type".to_string(), token::TokenType::Type);
        keywords.insert("use".to_string(), token::TokenType::Use);
        keywords.insert("while".to_string(), token::TokenType::While);
        keywords.insert("yield".to_string(), token::TokenType::Yield);

        keywords
    };
}

#[derive(Clone, Debug)]
pub struct Scanner {
    source: String,

    span: Span,
    tokens: Vec<token::Token>,
    error_ctx: ErrorContext,

    format_string_nest: usize,
}

impl Scanner {
    pub fn from_source(source: String) -> Self {
        return Self {
            source,

            span: Span::new(),
            tokens: Vec::new(),
            error_ctx: ErrorContext::new(),

            format_string_nest: 0,
        };
    }

    pub fn scan_tokens(mut self) -> (Vec<token::Token>, ErrorContext) {
        while !self.is_end_of_file() {
            self.span.start = self.span.end.clone();
            self.span.start.column += 1;
            self.scan_token();
        }

        self.tokens.push(token::Token {
            token_type: token::TokenType::EOF,
            lexeme: "".to_string(),
            span: Span::from_position(self.span.end),
        });

        return (self.tokens, self.error_ctx);
    }

    fn scan_token(&mut self) {
        let Some(c) = self.advance() else { return; };

        let token_type = match c {
            '}' if self.format_string_nest > 0 => {
                self.add_token(token::TokenType::RightBrace);
                self.span.start = self.span.end.clone();
                self.span.start.column += 1;

                Some(self.string(true))
            }

            '(' => Some(token::TokenType::LeftParen),
            ')' => Some(token::TokenType::RightParen),

            '{' => Some(token::TokenType::LeftBrace),
            '}' => Some(token::TokenType::RightBrace),

            '[' => Some(token::TokenType::LeftBracket),
            ']' => Some(token::TokenType::RightBracket),

            ',' => Some(token::TokenType::Comma),
            ';' => Some(token::TokenType::Semicolon),
            '@' => Some(token::TokenType::At),
            '$' => Some(token::TokenType::Dollar),

            '.' => Some(if self.match_next('.') {
                token::TokenType::DotDot
            } else {
                token::TokenType::Dot
            }),

            ':' => Some(if self.match_next(':') {
                token::TokenType::ColonColon
            } else {
                token::TokenType::Colon
            }),

            '-' => Some(if self.match_next('=') {
                token::TokenType::MinusEqual
            } else if self.match_next('>') {
                token::TokenType::MinusGreater
            } else {
                token::TokenType::Minus
            }),

            '+' => Some(if self.match_next('=') {
                token::TokenType::PlusEqual
            } else {
                token::TokenType::Plus
            }),

            '/' => {
                if self.match_next('/') {
                    self.comment_line();
                    None
                } else if self.match_next('*') {
                    self.comment_multiline();
                    None
                } else if self.match_next('=') {
                    Some(token::TokenType::SlashEqual)
                } else {
                    Some(token::TokenType::Slash)
                }
            }

            '*' => Some(if self.match_next('=') {
                token::TokenType::AsteriskEqual
            } else {
                token::TokenType::Asterisk
            }),

            '%' => Some(if self.match_next('=') {
                token::TokenType::PercentEqual
            } else {
                token::TokenType::Percent
            }),

            '^' => Some(if self.match_next('=') {
                token::TokenType::CaratEqual
            } else {
                token::TokenType::Carat
            }),

            '!' => Some(if self.match_next('=') {
                token::TokenType::BangEqual
            } else {
                token::TokenType::Bang
            }),

            '=' => Some(if self.match_next('=') {
                token::TokenType::EqualEqual
            } else if self.match_next('>') {
                token::TokenType::EqualGreater
            } else {
                token::TokenType::Equal
            }),

            '>' => Some(if self.match_next('>') {
                token::TokenType::GreaterGreater
            } else if self.match_next('=') {
                token::TokenType::GreaterEqual
            } else if self.match_next('.') {
                token::TokenType::GreaterDot
            } else {
                token::TokenType::Greater
            }),

            '<' => Some(if self.match_next('=') {
                token::TokenType::LessEqual
            } else {
                token::TokenType::Less
            }),

            '~' => Some(if self.match_next('=') {
                token::TokenType::TildeEqual
            } else {
                token::TokenType::Tilde
            }),

            '?' => Some(if self.match_next('?') {
                token::TokenType::QuestionQuestion
            } else {
                token::TokenType::Question
            }),

            '&' => Some(if self.match_next('&') {
                token::TokenType::AmpAmp
            } else {
                token::TokenType::Amp
            }),

            '#' => Some(if self.match_next('{') {
                token::TokenType::HashLeftBrace
            } else if self.match_next('[') {
                token::TokenType::HashLeftBracket
            } else {
                token::TokenType::Hash
            }),

            '|' if self.match_next('|') => Some(token::TokenType::BarBar),

            ' ' | '\r' | '\t' => None,

            '\n' => Some(token::TokenType::Newline),

            '\'' => Some(self.character()),

            '"' => Some(self.string(false)),

            c if self.is_digit(c) => Some(self.number()),
            c if self.is_alpha(c) => Some(self.identifier()),

            _ => {
                self.error_ctx
                    .error(self.span.clone(), "Unexpected character.");
                None
            }
        };

        if let Some(token_type) = token_type {
            self.add_token(token_type);
        }
    }

    fn comment_line(&mut self) {
        while self.peek() != Some('\n') && !self.is_end_of_file() {
            self.advance();
        }
    }

    fn comment_multiline(&mut self) {
        let mut nest = 0;

        while !self.is_end_of_file() {
            if self.match_next_slice(&['*', '/']) {
                if nest == 0 {
                    break;
                }

                nest -= 1;
            } else if self.match_next_slice(&['/', '*']) {
                nest += 1;
            } else {
                let is_newline = self.peek() == Some('\n');

                self.advance();

                if is_newline {
                    self.span.end.line += 1;
                    self.span.end.column = 0;
                }
            }
        }
    }

    fn character(&mut self) -> token::TokenType {
        if self.peek() == Some('\\') {
            self.advance();

            if self.is_end_of_file() {
                self.error_ctx
                    .error(self.span.clone(), "Unterminated char.");

                return token::TokenType::Char;
            } else {
                self.advance();
            }
        } else {
            match self.advance() {
                None => {
                    self.error_ctx
                        .error(self.span.clone(), "Unterminated char.");

                    return token::TokenType::Char;
                }

                Some('\'') => {
                    self.error_ctx.error(self.span.clone(), "Empty char.");

                    return token::TokenType::Char;
                }

                _ => {}
            }
        }

        match self.peek_next() {
            Some('\'') => {
                self.advance();
            }

            _ => self
                .error_ctx
                .error(self.span.clone(), "Unterminated char."),
        }

        return token::TokenType::Char;
    }

    fn string(&mut self, is_format_string_post: bool) -> token::TokenType {
        let mut is_format_string_pre = false;

        while !self.is_end_of_file() {
            match self.peek() {
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('{') => {
                    is_format_string_pre = true;
                    break;
                }
                Some('\\') => {
                    self.advance();
                }
                Some('\n') => {
                    self.span.end.newline();
                }
                _ => {}
            }

            self.advance();
        }

        if self.is_end_of_file() {
            self.error_ctx
                .error(self.span.clone(), "Unterminated string.");
        }

        match (is_format_string_post, is_format_string_pre) {
            (false, false) => return token::TokenType::PlainString,
            (false, true) => {
                self.format_string_nest += 1;
                return token::TokenType::FormatStringOpen;
            }
            (true, true) => return token::TokenType::FormatStringMid,
            (true, false) => {
                self.format_string_nest -= 1;
                return token::TokenType::FormatStringClose;
            }
        }
    }

    fn number(&mut self) -> token::TokenType {
        while let Some(peek) = self.peek() {
            if !self.is_digit(peek) {
                break;
            }

            self.advance();
        }

        // Look for a fractional part
        if self.peek() == Some('.') {
            if let Some(next) = self.peek_next() {
                if self.is_digit(next) {
                    // Consume the "."
                    self.advance();

                    while let Some(peek) = self.peek() {
                        if !self.is_digit(peek) {
                            break;
                        }

                        self.advance();
                    }
                }
            }
        }

        return token::TokenType::Number;
    }

    fn identifier(&mut self) -> token::TokenType {
        while let Some(peek) = self.peek() {
            if !self.is_alpha_numeric(peek) {
                break;
            }

            self.advance();
        }

        let text = &self.source[self.span.start.index..self.span.end.index];

        return KEYWORDS
            .get(text)
            .cloned()
            .unwrap_or(token::TokenType::Identifier);
    }

    fn match_next(&mut self, expected: char) -> bool {
        return self.match_next_slice(&[expected]);
    }

    fn match_next_slice(&mut self, expected: &[char]) -> bool {
        if self.source.len() <= self.span.end.index + expected.len() {
            return false;
        }

        for (offset, c) in expected.into_iter().enumerate() {
            match self.source.char_at(self.span.end.index + offset) {
                Some(actual) if actual == *c => {}

                _ => return false,
            }
        }

        self.span.end.index += expected.len();
        self.span.end.column += expected.len();

        return true;
    }

    fn peek(&self) -> Option<char> {
        if self.is_end_of_file() {
            return None;
        }

        return self.source.char_at(self.span.end.index);
    }

    fn peek_next(&self) -> Option<char> {
        if self.span.end.index + 1 >= self.source.len() {
            return None;
        }

        return self.source.char_at(self.span.end.index + 1);
    }

    fn is_alpha(&self, c: char) -> bool {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    fn is_digit(&self, c: char) -> bool {
        return c >= '0' && c <= '9';
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        return self.is_alpha(c) || self.is_digit(c);
    }

    fn is_end_of_file(&self) -> bool {
        return self.span.end.index >= self.source.len();
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.source.char_at(self.span.end.index) {
            self.span.end.index += 1;
            self.span.end.column += 1;

            return Some(c);
        }

        return None;
    }

    fn add_token(&mut self, token_type: token::TokenType) {
        self.span.start.index += 1;

        let text = &self.source[self.span.start.index - 1..self.span.end.index];

        let is_newline = matches!(token_type, token::TokenType::Newline);

        self.tokens.push(token::Token {
            token_type,
            lexeme: text.to_string(),
            span: self.span.clone(),
        });

        if is_newline {
            self.apply_newline();
        }
    }

    fn apply_newline(&mut self) {
        self.span.start.newline();
        self.span.end = self.span.start.clone();
    }
}
