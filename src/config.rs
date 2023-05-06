use std::env;

use super::*;

#[derive(Debug, Clone)]
pub struct Config {
    pub entry_file: Option<String>,
}

pub fn parse_config() -> Result<Config> {
    let mut cli_args: Vec<String> = env::args().collect();

    println!("{cli_args:#?}");

    let entry_file = if cli_args.len() == 1 {
        None
    } else {
        Some(cli_args.remove(1))
    };

    return Ok(Config { entry_file });
}
