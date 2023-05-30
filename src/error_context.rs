use super::*;

use runtime_value as rt;

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorContext {
    pub error_reports: Vec<ErrorReport>,
    pub runtime_errors: Vec<rt::RuntimeError>,
}

impl ErrorContext {
    pub fn new() -> Self {
        return Self {
            error_reports: Vec::new(),
            runtime_errors: Vec::new(),
        };
    }

    pub fn merge(contexts: Vec<Self>) -> Self {
        let report_capacity = contexts.iter().map(|ctx| ctx.error_reports.len()).sum();
        let rt_capacity = contexts.iter().map(|ctx| ctx.runtime_errors.len()).sum();

        let mut merged = Self {
            error_reports: Vec::with_capacity(report_capacity),
            runtime_errors: Vec::with_capacity(rt_capacity),
        };

        for ctx in contexts.into_iter() {
            merged.error_reports.extend(ctx.error_reports);
            merged.runtime_errors.extend(ctx.runtime_errors);
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

    pub fn runtime_error(&mut self, err: rt::RuntimeError) {
        eprintln!("{err}");
        self.runtime_errors.push(err);
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
