use super::*;

use crate::interpreter::Interpreter;
use crate::scanner::Scanner;

use std::fs;
use std::path;

pub fn run(config: &Config) -> Result {
    dbg!(&config);

    let entry_file = config
        .entry_file
        .clone()
        .unwrap_or_else(|| String::from("./src/main.fe"));

    let mut interpreter = Interpreter::new();

    let content = fs::read_to_string(path::PathBuf::from(entry_file))?;
    dbg!(&content);

    let scanner = Scanner::new(content);
    let (tokens, error_ctx) = scanner.scan_tokens();

    dbg!(&tokens);
    dbg!(&error_ctx);

    return ok();
}
