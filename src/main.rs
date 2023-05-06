mod ast;
mod config;
mod error_context;
mod ferrum;
mod interpreter;
mod parser;
mod resolver;
mod result;
mod scanner;
mod span;
mod token;
mod utils;

pub use config::Config;
pub use error_context::ErrorContext;
pub use result::{ok, Result};
pub use span::{Position, Span};
pub use utils::*;

use config::parse_config;

fn main() -> Result {
    let config = parse_config()?;

    ferrum::run(&config)?;

    return ok();
}
