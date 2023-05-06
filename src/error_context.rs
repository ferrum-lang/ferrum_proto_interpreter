use super::*;

#[derive(Clone, Debug)]
pub struct ErrorContext {
    pub errors_occured: usize,
}

impl ErrorContext {
    pub fn new() -> Self {
        return Self { errors_occured: 0 };
    }

    pub fn error(&mut self, span: Span, message: &str) {
        self.report(span, "", message);
    }

    fn report(&mut self, span: Span, where_: &str, message: &str) {
        eprintln!("[{span:?}] Error{where_}: {message}");

        self.errors_occured += 1;
    }
}
