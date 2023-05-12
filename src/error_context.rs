use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorContext {
    pub error_reports: Vec<ErrorReport>,
}

impl ErrorContext {
    pub fn new() -> Self {
        return Self {
            error_reports: Vec::new(),
        };
    }

    pub fn merge(contexts: Vec<Self>) -> Self {
        let capacity = contexts.iter().map(|ctx| ctx.error_reports.len()).sum();

        let mut merged = Self {
            error_reports: Vec::with_capacity(capacity),
        };

        for ctx in contexts.into_iter() {
            merged.error_reports.extend(ctx.error_reports);
        }

        return merged;
    }

    pub fn error(&mut self, span: Span, message: impl Into<String>) {
        self.report(span, String::new(), message.into());
    }

    pub fn token_error(&mut self, t: token::Token, message: impl Into<String>) {
        if t.token_type == token::TokenType::EOF {
            self.report(t.span, " at end".to_string(), message.into());
        } else {
            self.report(t.span, format!(" at '{}'", t.lexeme), message.into());
        }
    }

    fn report(&mut self, span: Span, where_: String, message: String) {
        eprintln!("[{span:?}] Error{where_}: {message}");

        self.error_reports.push(ErrorReport {
            span,
            where_,
            message,
        });
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorReport {
    pub span: Span,
    pub where_: String,
    pub message: String,
}
