use super::*;

use crate::parser;
use crate::scanner;

use std::fs;
use std::path;

pub fn run(config: &Config) -> Result {
    dbg!(&config);

    let entry_file = config
        .entry_file
        .clone()
        .unwrap_or_else(|| String::from("./src/main.fe"));

    let content = fs::read_to_string(path::PathBuf::from(entry_file))?;
    // dbg!(&content);

    let (tokens, scan_error_ctx) = scanner::Scanner::from_source(content).scan_tokens();
    dbg!(&tokens);

    let (ast, parse_error_ctx) = parser::Parser::from_tokens(tokens).parse_ast();
    dbg!(&ast);

    let error_ctx = ErrorContext::merge(vec![scan_error_ctx, parse_error_ctx]);
    dbg!(&error_ctx);

    return ok();
}
