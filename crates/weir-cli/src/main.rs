use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "weir", about = "The Weir programming language")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Parse a .weir file and dump the AST
    Parse {
        /// Path to the .weir source file
        file: PathBuf,
    },
    /// Type-check a .weir file without running it
    Check {
        /// Path to the .weir source file
        file: PathBuf,
    },
    /// Run a .weir file
    Run {
        /// Path to the .weir source file
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Parse { file } => {
            let source = match std::fs::read_to_string(&file) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("error: could not read {}: {}", file.display(), e);
                    std::process::exit(1);
                }
            };

            let (module, errors) = weir_parser::parse(&source);

            if !errors.is_empty() {
                for error in &errors {
                    eprintln!(
                        "{}:{}:{}: {}",
                        file.display(),
                        error.span.start,
                        error.span.end,
                        error.message
                    );
                }
            }

            print!("{}", weir_ast::pretty_print(&module));

            if !errors.is_empty() {
                std::process::exit(1);
            }
        }
        Command::Check { file } => {
            let source = match std::fs::read_to_string(&file) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("error: could not read {}: {}", file.display(), e);
                    std::process::exit(1);
                }
            };

            let (module, parse_errors) = weir_parser::parse(&source);

            if !parse_errors.is_empty() {
                for error in &parse_errors {
                    eprintln!(
                        "{}:{}:{}: parse error: {}",
                        file.display(),
                        error.span.start,
                        error.span.end,
                        error.message
                    );
                }
                std::process::exit(1);
            }

            let result = weir_typeck::check(&module);

            if result.errors.is_empty() {
                println!("OK — no type errors");
            } else {
                for error in &result.errors {
                    eprintln!(
                        "{}:{}:{}: type error: {}",
                        file.display(),
                        error.span.start,
                        error.span.end,
                        error.message
                    );
                }
                std::process::exit(1);
            }
        }
        Command::Run { file } => {
            let source = match std::fs::read_to_string(&file) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("error: could not read {}: {}", file.display(), e);
                    std::process::exit(1);
                }
            };

            let (module, errors) = weir_parser::parse(&source);

            if !errors.is_empty() {
                for error in &errors {
                    eprintln!(
                        "{}:{}:{}: {}",
                        file.display(),
                        error.span.start,
                        error.span.end,
                        error.message
                    );
                }
                std::process::exit(1);
            }

            match weir_interp::interpret(&module) {
                Ok(output) => print!("{}", output),
                Err(e) => {
                    eprintln!("{}:{}: runtime error: {}", file.display(),
                        e.span.map_or(0, |s| s.start), e.message);
                    std::process::exit(1);
                }
            }
        }
    }
}
