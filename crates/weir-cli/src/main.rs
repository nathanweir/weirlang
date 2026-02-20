use clap::{Parser, Subcommand};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

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
        /// Path to the .weir source file (omit to use weir.pkg)
        file: Option<PathBuf>,
        /// Pre-load a shared library before JIT (makes symbols available via dlsym)
        #[arg(long = "load")]
        load: Vec<PathBuf>,
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
        /// Path to the .weir source file (omit to use weir.pkg)
        file: Option<PathBuf>,
        /// Output binary path (defaults to source file stem or package name)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Link against a C library (e.g. -l glfw3 -l GL)
        #[arg(short = 'l', long = "link")]
        link: Vec<String>,
        /// Pass raw arguments to the C compiler (e.g. --cc-arg libs/helper.c)
        #[arg(long = "cc-arg")]
        cc_args: Vec<String>,
    },
    /// Run a .weir file with live reload — watches for changes and hot-swaps functions
    Dev {
        /// Path to the .weir source file (omit to use weir.pkg)
        file: Option<PathBuf>,
        /// Pre-load a shared library before JIT (makes symbols available via dlsym)
        #[arg(long = "load")]
        load: Vec<PathBuf>,
    },
    /// Compile a .weir file to a WASM binary for the browser
    Wasm {
        /// Path to the .weir source file (omit to use weir.pkg)
        file: Option<PathBuf>,
        /// Output directory (default: ./web/)
        #[arg(short, long)]
        output: Option<PathBuf>,
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

/// Result of resolving and concatenating a package project.
struct PackageSource {
    /// The concatenated, macro-expanded source (prelude + deps + entry).
    expanded_source: String,
    /// Native C source files to compile.
    native_sources: Vec<PathBuf>,
    /// Libraries to link (-l flags).
    link_libs: Vec<String>,
    /// JS bridge files for WASM builds.
    wasm_bridge_files: Vec<PathBuf>,
    /// Path to display in error messages.
    display_path: PathBuf,
    /// The entry source file path (for dev mode watching).
    entry_path: Option<PathBuf>,
    /// Package name (for default output binary name).
    package_name: String,
}

/// Resolve a weir.pkg project and concatenate all sources.
///
/// 1. Look for `weir.pkg` in CWD
/// 2. Resolve dependencies
/// 3. Read, expand, and parse each module to extract exports
/// 4. Validate imports
/// 5. Concatenate: prelude + dep sources (topo order) + entry source
/// 6. Return the expanded concatenated source
fn resolve_and_concatenate() -> PackageSource {
    let manifest_path = Path::new("weir.pkg");
    if !manifest_path.exists() {
        eprintln!("error: no weir.pkg manifest found in current directory");
        std::process::exit(1);
    }

    let project = match weir_pkg::resolve_project(manifest_path) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(1);
        }
    };

    // Read and expand each module source, extract exports
    let mut module_sources: Vec<(smol_str::SmolStr, String)> = Vec::new();
    let mut module_exports: HashMap<smol_str::SmolStr, std::collections::HashSet<smol_str::SmolStr>> =
        HashMap::new();
    let mut parsed_modules: HashMap<smol_str::SmolStr, weir_ast::Module> = HashMap::new();

    for resolved_mod in &project.modules {
        let source = match std::fs::read_to_string(&resolved_mod.source_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "error: could not read {}: {}",
                    resolved_mod.source_path.display(),
                    e
                );
                std::process::exit(1);
            }
        };

        // Expand macros on each module individually (without prelude — prelude is prepended once)
        let expand_result = weir_macros::expand(&source);
        if !expand_result.errors.is_empty() {
            for error in &expand_result.errors {
                eprintln!(
                    "{}:{}:{}: macro error: {}",
                    resolved_mod.source_path.display(),
                    error.span.start,
                    error.span.end,
                    error.message
                );
            }
            std::process::exit(1);
        }

        // Parse to extract exports
        let (module_ast, parse_errors) = weir_parser::parse(&expand_result.source);
        if !parse_errors.is_empty() {
            for error in &parse_errors {
                eprintln!(
                    "{}:{}:{}: parse error: {}",
                    resolved_mod.source_path.display(),
                    error.span.start,
                    error.span.end,
                    error.message
                );
            }
            std::process::exit(1);
        }

        let exports = weir_pkg::extract_exports(&module_ast);
        module_exports.insert(resolved_mod.module_name.clone(), exports);
        parsed_modules.insert(resolved_mod.module_name.clone(), module_ast);
        module_sources.push((resolved_mod.module_name.clone(), expand_result.source));
    }

    // Validate imports
    let import_errors = weir_pkg::validate_imports(&module_exports, &parsed_modules);
    if !import_errors.is_empty() {
        for error in &import_errors {
            eprintln!("error: {}", error);
        }
        std::process::exit(1);
    }

    // Concatenate: prelude first, then all module sources in topo order
    // (modules are already in topo order from resolve_project)
    let mut concatenated = String::from(PRELUDE_SOURCE);
    concatenated.push('\n');
    for (_mod_name, source) in &module_sources {
        concatenated.push_str(source);
        concatenated.push('\n');
    }

    // Determine display path and package name from manifest
    let manifest_source = std::fs::read_to_string("weir.pkg").unwrap_or_default();
    let manifest = weir_pkg::PackageManifest {
        name: "unknown".into(),
        version: "0.0.0".into(),
        sources: vec![],
        main: None,
        deps: vec![],
        native_sources: vec![],
        link_libs: vec![],
        wasm_bridge_files: vec![],
    };
    let manifest = weir_pkg::manifest::parse_manifest(&manifest_source, Path::new("weir.pkg"))
        .unwrap_or(manifest);

    let display_path = project
        .entry_source
        .clone()
        .unwrap_or_else(|| PathBuf::from("weir.pkg"));

    PackageSource {
        expanded_source: concatenated,
        native_sources: project.native_sources,
        link_libs: project.link_libs,
        wasm_bridge_files: project.wasm_bridge_files,
        display_path,
        entry_path: project.entry_source,
        package_name: manifest.name,
    }
}

/// Pre-load shared libraries via dlopen so symbols are available via dlsym.
fn preload_libraries(libs: &[PathBuf]) {
    for lib_path in libs {
        let c_path =
            std::ffi::CString::new(lib_path.to_str().unwrap_or_default()).unwrap();
        let handle = unsafe {
            libc::dlopen(c_path.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL)
        };
        if handle.is_null() {
            let err = unsafe { std::ffi::CStr::from_ptr(libc::dlerror()) };
            eprintln!(
                "error: could not load {}: {}",
                lib_path.display(),
                err.to_string_lossy()
            );
            std::process::exit(1);
        }
    }
}

/// Compile native C sources into a shared library and dlopen it.
fn load_native_sources(sources: &[PathBuf], libs: &[String]) {
    if sources.is_empty() {
        return;
    }

    // Ensure tmp/ directory exists
    let _ = std::fs::create_dir_all("tmp");
    let so_path = PathBuf::from("tmp/libweir_native.so");

    let mut cmd = std::process::Command::new("cc");
    cmd.arg("-shared").arg("-fPIC").arg("-o").arg(&so_path);
    for source in sources {
        cmd.arg(source);
    }
    for lib in libs {
        cmd.arg(format!("-l{}", lib));
    }

    let output = match cmd.output() {
        Ok(o) => o,
        Err(e) => {
            eprintln!("error: failed to run cc: {}", e);
            std::process::exit(1);
        }
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("error: native compilation failed:\n{}", stderr);
        std::process::exit(1);
    }

    // dlopen the compiled library
    preload_libraries(&[so_path]);
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
        Command::Run { file, load } => {
            preload_libraries(&load);

            if let Some(file) = file {
                // Single-file mode
                let source = with_prelude(&read_file(&file));
                let expanded = expand_source(&source, &file);
                run_jit(&expanded, &file);
            } else {
                // Package mode
                let pkg = resolve_and_concatenate();
                load_native_sources(&pkg.native_sources, &pkg.link_libs);
                run_jit(&pkg.expanded_source, &pkg.display_path);
            }
        }
        Command::Build {
            file,
            output,
            link,
            cc_args,
        } => {
            if let Some(file) = file {
                // Single-file mode
                let source = with_prelude(&read_file(&file));
                let expanded = expand_source(&source, &file);
                let output_path = output
                    .unwrap_or_else(|| PathBuf::from(file.file_stem().unwrap_or_default()));
                let link_flags: Vec<String> =
                    link.iter().map(|l| format!("-l{}", l)).collect();
                build_binary(&expanded, &file, &output_path, &cc_args, &link_flags);
            } else {
                // Package mode
                let pkg = resolve_and_concatenate();
                let output_path = output.unwrap_or_else(|| PathBuf::from(&pkg.package_name));

                let mut all_cc_args = cc_args;
                for ns in &pkg.native_sources {
                    all_cc_args.push(ns.to_string_lossy().into_owned());
                }
                let mut all_link_flags: Vec<String> =
                    link.iter().map(|l| format!("-l{}", l)).collect();
                for lib in &pkg.link_libs {
                    all_link_flags.push(format!("-l{}", lib));
                }

                build_binary(
                    &pkg.expanded_source,
                    &pkg.display_path,
                    &output_path,
                    &all_cc_args,
                    &all_link_flags,
                );
            }
        }
        Command::Dev { file, load } => {
            preload_libraries(&load);

            if let Some(file) = file {
                // Single-file mode
                let source = with_prelude(&read_file(&file));
                let expanded = expand_source(&source, &file);
                run_dev(&expanded, &file);
            } else {
                // Package mode
                let pkg = resolve_and_concatenate();
                load_native_sources(&pkg.native_sources, &pkg.link_libs);
                let watch_path = pkg
                    .entry_path
                    .unwrap_or_else(|| PathBuf::from("weir.pkg"));
                run_dev_pkg(&pkg.expanded_source, &watch_path);
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
        Command::Wasm { file, output } => {
            if let Some(file) = file {
                // Single-file mode
                let source = with_prelude(&read_file(&file));
                let expanded = expand_source(&source, &file);
                let output_dir = output.unwrap_or_else(|| PathBuf::from("web"));
                build_wasm(&expanded, &file, &output_dir, &[]);
            } else {
                // Package mode
                let pkg = resolve_and_concatenate();
                let output_dir = output.unwrap_or_else(|| PathBuf::from("web"));
                build_wasm(
                    &pkg.expanded_source,
                    &pkg.display_path,
                    &output_dir,
                    &pkg.wasm_bridge_files,
                );
            }
        }
        Command::Lsp => {
            let rt =
                tokio::runtime::Runtime::new().expect("failed to create tokio runtime");
            rt.block_on(weir_lsp::run_lsp());
        }
    }
}

/// Parse, typecheck, and JIT-run source code.
fn run_jit(expanded_source: &str, display_path: &Path) {
    let (module, parse_errors) = weir_parser::parse(expanded_source);

    if !parse_errors.is_empty() {
        for error in &parse_errors {
            eprintln!(
                "{}:{}:{}: parse error: {}",
                display_path.display(),
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
                display_path.display(),
                error.span.start,
                error.span.end,
                error.message
            );
        }
        std::process::exit(1);
    }

    weir_codegen::set_direct_output(true);
    match weir_codegen::compile_and_run(&module, &type_info) {
        Ok(output) => print!("{}", output),
        Err(e) => {
            eprintln!("{}: codegen error: {}", display_path.display(), e);
            std::process::exit(1);
        }
    }
}

/// Parse, typecheck, and AOT-compile source code to a binary.
fn build_binary(
    expanded_source: &str,
    display_path: &Path,
    output_path: &Path,
    cc_args: &[String],
    link_flags: &[String],
) {
    let (module, parse_errors) = weir_parser::parse(expanded_source);

    if !parse_errors.is_empty() {
        for error in &parse_errors {
            eprintln!(
                "{}:{}:{}: parse error: {}",
                display_path.display(),
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
                display_path.display(),
                error.span.start,
                error.span.end,
                error.message
            );
        }
        std::process::exit(1);
    }

    match weir_codegen::build_executable(&module, &type_info, output_path, cc_args, link_flags) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}: build error: {}", display_path.display(), e);
            std::process::exit(1);
        }
    }
}

/// Run dev mode for a single-file project.
fn run_dev(expanded_source: &str, file: &Path) {
    let canonical = std::fs::canonicalize(file).unwrap_or_else(|_| file.to_path_buf());

    let session = match weir_codegen::DevSession::new(expanded_source) {
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

/// Run dev mode for a package project.
fn run_dev_pkg(expanded_source: &str, watch_path: &Path) {
    let canonical =
        std::fs::canonicalize(watch_path).unwrap_or_else(|_| watch_path.to_path_buf());

    let session = match weir_codegen::DevSession::new(expanded_source) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}: {}", watch_path.display(), e);
            std::process::exit(1);
        }
    };

    // In package mode, re-read all sources on each reload
    let transform = |_raw: &str| -> Result<String, String> {
        let pkg = resolve_and_concatenate_result()
            .map_err(|e| format!("{}", e))?;
        Ok(pkg.expanded_source)
    };

    if let Err(e) = session.run_dev_loop(&canonical, transform) {
        eprintln!("{}: dev error: {}", watch_path.display(), e);
        std::process::exit(1);
    }
}

/// Non-exiting version of resolve_and_concatenate for use in dev mode transform.
fn resolve_and_concatenate_result() -> Result<PackageSource, String> {
    let manifest_path = Path::new("weir.pkg");
    if !manifest_path.exists() {
        return Err("no weir.pkg manifest found in current directory".into());
    }

    let project = weir_pkg::resolve_project(manifest_path)
        .map_err(|e| format!("{}", e))?;

    let mut module_sources: Vec<(smol_str::SmolStr, String)> = Vec::new();
    let mut module_exports: HashMap<smol_str::SmolStr, std::collections::HashSet<smol_str::SmolStr>> =
        HashMap::new();
    let mut parsed_modules: HashMap<smol_str::SmolStr, weir_ast::Module> = HashMap::new();

    for resolved_mod in &project.modules {
        let source = std::fs::read_to_string(&resolved_mod.source_path)
            .map_err(|e| format!("could not read {}: {}", resolved_mod.source_path.display(), e))?;

        let expand_result = weir_macros::expand(&source);
        if !expand_result.errors.is_empty() {
            let msgs: Vec<_> = expand_result.errors.iter().map(|e| e.message.clone()).collect();
            return Err(format!("macro errors: {}", msgs.join("; ")));
        }

        let (module_ast, parse_errors) = weir_parser::parse(&expand_result.source);
        if !parse_errors.is_empty() {
            let msgs: Vec<_> = parse_errors.iter().map(|e| e.message.clone()).collect();
            return Err(format!("parse errors: {}", msgs.join("; ")));
        }

        let exports = weir_pkg::extract_exports(&module_ast);
        module_exports.insert(resolved_mod.module_name.clone(), exports);
        parsed_modules.insert(resolved_mod.module_name.clone(), module_ast);
        module_sources.push((resolved_mod.module_name.clone(), expand_result.source));
    }

    let import_errors = weir_pkg::validate_imports(&module_exports, &parsed_modules);
    if !import_errors.is_empty() {
        let msgs: Vec<_> = import_errors.iter().map(|e| format!("{}", e)).collect();
        return Err(msgs.join("; "));
    }

    let mut concatenated = String::from(PRELUDE_SOURCE);
    concatenated.push('\n');
    for (_mod_name, source) in &module_sources {
        concatenated.push_str(source);
        concatenated.push('\n');
    }

    let manifest_source = std::fs::read_to_string("weir.pkg").unwrap_or_default();
    let manifest = weir_pkg::manifest::parse_manifest(&manifest_source, Path::new("weir.pkg"))
        .unwrap_or(weir_pkg::PackageManifest {
            name: "unknown".into(),
            version: "0.0.0".into(),
            sources: vec![],
            main: None,
            deps: vec![],
            native_sources: vec![],
            link_libs: vec![],
            wasm_bridge_files: vec![],
        });

    let display_path = project
        .entry_source
        .clone()
        .unwrap_or_else(|| PathBuf::from("weir.pkg"));

    Ok(PackageSource {
        expanded_source: concatenated,
        native_sources: project.native_sources,
        link_libs: project.link_libs,
        wasm_bridge_files: project.wasm_bridge_files,
        display_path,
        entry_path: project.entry_source,
        package_name: manifest.name,
    })
}

/// Parse, typecheck, and compile source code to a WASM package.
fn build_wasm(
    expanded_source: &str,
    display_path: &Path,
    output_dir: &Path,
    bridge_files: &[PathBuf],
) {
    let (module, parse_errors) = weir_parser::parse(expanded_source);

    if !parse_errors.is_empty() {
        for error in &parse_errors {
            eprintln!(
                "{}:{}:{}: parse error: {}",
                display_path.display(),
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
                display_path.display(),
                error.span.start,
                error.span.end,
                error.message
            );
        }
        std::process::exit(1);
    }

    match weir_wasm::build_wasm_package(&module, &type_info, bridge_files, output_dir) {
        Ok(()) => {
            eprintln!(
                "WASM package built: {}/",
                output_dir.display()
            );
        }
        Err(e) => {
            eprintln!("{}: wasm error: {}", display_path.display(), e);
            std::process::exit(1);
        }
    }
}
