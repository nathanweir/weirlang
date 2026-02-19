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
    /// Compile and run a .weir file natively via Cranelift JIT
    Run {
        /// Path to the .weir source file
        file: PathBuf,
    },
    /// Run a .weir file via the tree-walking interpreter
    Interp {
        /// Path to the .weir source file
        file: PathBuf,
    },
    /// Expand macros and print the result (like cargo expand)
    Expand {
        /// Path to the .weir source file
        file: PathBuf,
    },
    /// Compile a .weir file to a standalone native binary
    Build {
        /// Path to the .weir source file
        file: PathBuf,
        /// Output binary path (defaults to source file stem)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Link against a C library (e.g. -l glfw3 -l GL)
        #[arg(short = 'l', long = "link")]
        link: Vec<String>,
    },
    /// Run a .weir file with live reload — watches for changes and hot-swaps functions
    Dev {
        /// Path to the .weir source file
        file: PathBuf,
    },
    /// Start the Weir Language Server Protocol server
    Lsp,
}

const PRELUDE_SOURCE: &str = include_str!("../../../std/prelude.weir");

fn with_prelude(source: &str) -> String {
    format!("{}\n{}", PRELUDE_SOURCE, source)
}

fn read_file(file: &std::path::Path) -> String {
    match std::fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read {}: {}", file.display(), e);
            std::process::exit(1);
        }
    }
}

/// Run macro expansion on source, returning the expanded source string.
/// Exits on expansion errors.
fn expand_source(source: &str, file: &std::path::Path) -> String {
    let result = weir_macros::expand(source);
    if !result.errors.is_empty() {
        for error in &result.errors {
            eprintln!(
                "{}:{}:{}: macro error: {}",
                file.display(),
                error.span.start,
                error.span.end,
                error.message
            );
        }
        std::process::exit(1);
    }
    result.source
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Parse { file } => {
            let source = read_file(&file);
            let expanded = expand_source(&source, &file);

            let (module, errors) = weir_parser::parse(&expanded);

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
            let source = with_prelude(&read_file(&file));
            let expanded = expand_source(&source, &file);

            let (module, parse_errors) = weir_parser::parse(&expanded);

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
            let source = with_prelude(&read_file(&file));
            let expanded = expand_source(&source, &file);

            let (module, parse_errors) = weir_parser::parse(&expanded);

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

            let type_info = weir_typeck::check(&module);

            if !type_info.errors.is_empty() {
                for error in &type_info.errors {
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

            match weir_codegen::compile_and_run(&module, &type_info) {
                Ok(output) => print!("{}", output),
                Err(e) => {
                    eprintln!("{}: codegen error: {}", file.display(), e);
                    std::process::exit(1);
                }
            }
        }
        Command::Build { file, output, link } => {
            let source = with_prelude(&read_file(&file));
            let expanded = expand_source(&source, &file);

            let (module, parse_errors) = weir_parser::parse(&expanded);

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

            let type_info = weir_typeck::check(&module);

            if !type_info.errors.is_empty() {
                for error in &type_info.errors {
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

            let output_path =
                output.unwrap_or_else(|| PathBuf::from(file.file_stem().unwrap_or_default()));

            let link_flags: Vec<String> = link.iter().map(|l| format!("-l{}", l)).collect();

            match weir_codegen::build_executable(&module, &type_info, &output_path, &link_flags) {
                Ok(()) => {}
                Err(e) => {
                    eprintln!("{}: build error: {}", file.display(), e);
                    std::process::exit(1);
                }
            }
        }
        Command::Dev { file } => {
            let source = with_prelude(&read_file(&file));
            let expanded = expand_source(&source, &file);

            let canonical = std::fs::canonicalize(&file).unwrap_or_else(|_| file.clone());

            let session = match weir_codegen::DevSession::new(&expanded) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("{}: {}", file.display(), e);
                    std::process::exit(1);
                }
            };

            let transform = |raw: &str| -> Result<String, String> {
                let with_prelude = format!("{}\n{}", PRELUDE_SOURCE, raw);
                let result = weir_macros::expand(&with_prelude);
                if !result.errors.is_empty() {
                    return Err(result
                        .errors
                        .iter()
                        .map(|e| e.message.clone())
                        .collect::<Vec<_>>()
                        .join("; "));
                }
                Ok(result.source)
            };

            if let Err(e) = session.run_dev_loop(&canonical, transform) {
                eprintln!("{}: dev error: {}", file.display(), e);
                std::process::exit(1);
            }
        }
        Command::Interp { file } => {
            let source = with_prelude(&read_file(&file));
            let expanded = expand_source(&source, &file);

            let (module, errors) = weir_parser::parse(&expanded);

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
                    eprintln!(
                        "{}:{}: runtime error: {}",
                        file.display(),
                        e.span.map_or(0, |s| s.start),
                        e.message
                    );
                    std::process::exit(1);
                }
            }
        }
        Command::Expand { file } => {
            let source = read_file(&file);
            let expanded = expand_source(&source, &file);
            print!("{}", expanded);
        }
        Command::Lsp => {
            let rt = tokio::runtime::Runtime::new().expect("failed to create tokio runtime");
            rt.block_on(weir_lsp::run_lsp());
        }
    }
}
