use super::*;

use crate::interpreter;
use crate::parser;
use crate::resolver;
use crate::scanner;
use crate::type_checker;

use std::fs;
use std::path;

pub fn run(config: &Config) -> Result {
    // dbg!(&config);

    let entry_file = config
        .entry_file
        .clone()
        .unwrap_or_else(|| String::from("./src/main.fe"));

    let content = fs::read_to_string(path::PathBuf::from(entry_file))?;
    // dbg!(&content);

    let (tokens, scan_err_ctx) = scanner::Scanner::from_source(content).scan_tokens();
    // dbg!(&tokens);

    let (ast, parse_err_ctx) = parser::Parser::from_tokens(tokens).parse_ast();
    // dbg!(&ast);

    let (locals, resolve_err_ctx) = resolver::Resolver::from_ast(&ast).resolve_locals();
    // dbg!(&locals);

    let (types, type_check_err_ctx) =
        type_checker::TypeChecker::from_context(&ast, &locals).resolve_types();
    // dbg!(&types);

    let error_ctx = ErrorContext::merge(vec![
        scan_err_ctx,
        parse_err_ctx,
        resolve_err_ctx,
        type_check_err_ctx,
    ]);
    // dbg!(&error_ctx);

    if error_ctx.error_reports.is_empty() {
        let _runtime_err_ctx = interpreter::Interpreter::from_context(ast, locals).interpret();
    }

    return ok();
}
