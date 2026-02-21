// ── Dev Session (live reload) ────────────────────────────────────

use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::atomic::{AtomicPtr, Ordering};

use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use smol_str::SmolStr;
use weir_ast::Item;
use weir_typeck::{Ty, TypeCheckResult};

use super::{make_isa, CodegenError, Compiler};
use crate::jit_helpers::{output_reset, output_take, register_jit_symbols};

/// What changed between two versions of the source.
struct ChangeSet {
    body_changed: HashSet<SmolStr>,
    sig_changed: HashSet<SmolStr>,
    added: HashSet<SmolStr>,
    removed: HashSet<SmolStr>,
    types_changed: HashSet<SmolStr>,
}

/// Result of a selective reload.
pub struct ReloadResult {
    pub recompiled: Vec<SmolStr>,
    pub skipped: usize,
    pub type_warnings: Vec<SmolStr>,
}

/// Hash the body text of each function from source using its Defn span.
fn compute_body_hashes(ast_module: &weir_ast::Module, source: &str) -> HashMap<SmolStr, u64> {
    use std::hash::{Hash, Hasher};
    let mut hashes = HashMap::new();
    for (item, _) in &ast_module.items {
        if let Item::Defn(defn) = item {
            let start = defn.span.start as usize;
            let end = defn.span.end as usize;
            if end <= source.len() {
                let body_text = &source[start..end];
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                body_text.hash(&mut hasher);
                hashes.insert(defn.name.clone(), hasher.finish());
            }
        }
    }
    hashes
}

/// Extract type->constructor signatures for type change detection.
fn compute_type_sigs(
    ast_module: &weir_ast::Module,
    type_info: &TypeCheckResult,
) -> HashMap<SmolStr, Vec<(SmolStr, Vec<Ty>)>> {
    let mut sigs: HashMap<SmolStr, Vec<(SmolStr, Vec<Ty>)>> = HashMap::new();
    for (item, _) in &ast_module.items {
        match item {
            Item::Deftype(dt) => {
                let mut con_sigs = Vec::new();
                for variant in &dt.variants {
                    if let Some(ft) = type_info.fn_types.get(&variant.name) {
                        con_sigs.push((variant.name.clone(), ft.param_types.clone()));
                    } else {
                        con_sigs.push((variant.name.clone(), vec![]));
                    }
                }
                sigs.insert(dt.name.clone(), con_sigs);
            }
            Item::Defstruct(ds) => {
                if let Some(ft) = type_info.fn_types.get(&ds.name) {
                    sigs.insert(
                        ds.name.clone(),
                        vec![(ds.name.clone(), ft.param_types.clone())],
                    );
                }
            }
            _ => {}
        }
    }
    sigs
}

pub struct DevSession {
    ast_module: weir_ast::Module,
    type_info: TypeCheckResult,
    /// Stable function pointer table -- indirect calls load from here.
    /// Boxed slice with stable address; AtomicPtr for thread-safe swaps.
    fn_table: Box<[AtomicPtr<u8>]>,
    /// Maps function name -> slot index in fn_table.
    fn_slots: HashMap<SmolStr, usize>,
    /// Maps function name -> (param types, return type) for signature reconstruction.
    fn_sigs: HashMap<SmolStr, (Vec<Ty>, Ty)>,
    /// Current JIT module (holds live code pages).
    current_module: JITModule,
    /// Old modules kept alive to prevent code page deallocation.
    _old_modules: Vec<JITModule>,
    /// Body hashes from the previous source for change detection.
    body_hashes: HashMap<SmolStr, u64>,
    /// Type constructor signatures from previous source for type change detection.
    type_sigs: HashMap<SmolStr, Vec<(SmolStr, Vec<Ty>)>>,
    /// Previous expanded source for short-circuit.
    prev_source: String,
}

type UserFnMap = HashMap<SmolStr, (cranelift_module::FuncId, Vec<Ty>, Ty)>;
type FnSigMap = HashMap<SmolStr, (Vec<Ty>, Ty)>;

/// Helper: compile functions with indirect dispatch into a JITModule.
/// When `dirty_set` is `Some`, only compile functions in the set (others are declared but not defined).
/// Returns (jit_module, user_fns_map, fn_sigs_map).
fn compile_dev_module(
    ast_module: &weir_ast::Module,
    type_info: &TypeCheckResult,
    fn_table_ptr: *const AtomicPtr<u8>,
    fn_slots: &HashMap<SmolStr, usize>,
    dirty_set: Option<&HashSet<SmolStr>>,
) -> Result<(JITModule, UserFnMap, FnSigMap), CodegenError> {
    let isa = make_isa(false)?;
    let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    register_jit_symbols(&mut builder);
    builder.symbol("weir_fn_table", fn_table_ptr as *const u8);

    // Register extern "C" symbols via dlsym (libraries must already be loaded)
    for (item, _) in &ast_module.items {
        if let Item::ExternC(ext) = item {
            for decl in &ext.declarations {
                let c_name = std::ffi::CString::new(decl.name.as_str()).unwrap();
                let ptr = unsafe { libc::dlsym(libc::RTLD_DEFAULT, c_name.as_ptr()) };
                if ptr.is_null() {
                    return Err(CodegenError::new(format!(
                        "extern function '{}' not found — is the library loaded? (use --load)",
                        decl.name
                    )));
                }
                builder.symbol(decl.name.as_str(), ptr as *const u8);
            }
        }
    }

    let jit_module = JITModule::new(builder);
    let mut compiler = Compiler::new(ast_module, type_info, jit_module);
    compiler.declare_runtime_helpers()?;
    compiler.declare_globals()?;
    compiler.declare_user_functions(Linkage::Local)?;

    let table_data_id = compiler
        .module
        .declare_data("weir_fn_table", Linkage::Import, false, false)
        .map_err(|e| CodegenError::new(format!("declare weir_fn_table data: {}", e)))?;

    let mut fn_sigs = HashMap::new();
    for (name, (_fid, param_tys, ret_ty)) in &compiler.user_fns {
        fn_sigs.insert(name.clone(), (param_tys.clone(), ret_ty.clone()));
    }

    compiler.compile_user_functions_selective(
        Some(fn_slots),
        Some(table_data_id),
        dirty_set,
    )?;
    compiler.compile_global_inits()?;

    compiler
        .module
        .finalize_definitions()
        .map_err(|e| CodegenError::new(format!("finalize: {}", e)))?;

    let user_fns = compiler.user_fns.clone();
    Ok((compiler.module, user_fns, fn_sigs))
}

impl DevSession {
    /// Create a new dev session: parse, typecheck, compile with indirect dispatch.
    pub fn new(source: &str) -> Result<Self, CodegenError> {
        let (ast_module, parse_errors) = weir_parser::parse(source);
        if !parse_errors.is_empty() {
            return Err(CodegenError::new(format!(
                "parse error: {}",
                parse_errors[0].message
            )));
        }

        let type_info = weir_typeck::check(&ast_module);
        if !type_info.errors.is_empty() {
            return Err(CodegenError::new(format!(
                "type error: {}",
                type_info.errors[0].message
            )));
        }

        // Assign slots to all user functions
        let mut fn_slots = HashMap::new();
        let mut slot = 0usize;
        for (item, _) in &ast_module.items {
            if let Item::Defn(defn) = item {
                fn_slots.insert(defn.name.clone(), slot);
                slot += 1;
            }
        }

        // Create function table (all null initially)
        let fn_table: Box<[AtomicPtr<u8>]> = (0..slot)
            .map(|_| AtomicPtr::new(std::ptr::null_mut()))
            .collect::<Vec<_>>()
            .into_boxed_slice();

        let (jit_module, user_fns, fn_sigs) =
            compile_dev_module(&ast_module, &type_info, fn_table.as_ptr(), &fn_slots, None)?;

        // Populate fn_table with finalized function pointers
        for (name, &slot_idx) in &fn_slots {
            let (func_id, _, _) = &user_fns[name];
            let ptr = jit_module.get_finalized_function(*func_id);
            fn_table[slot_idx].store(ptr as *mut u8, Ordering::Release);
        }

        let body_hashes = compute_body_hashes(&ast_module, source);
        let type_sigs = compute_type_sigs(&ast_module, &type_info);

        Ok(DevSession {
            ast_module,
            type_info,
            fn_table,
            fn_slots,
            fn_sigs,
            current_module: jit_module,
            _old_modules: Vec::new(),
            body_hashes,
            type_sigs,
            prev_source: source.to_string(),
        })
    }

    /// Selectively recompile only dirty functions from new source and swap pointers.
    /// Returns a `ReloadResult` with recompilation stats and type warnings.
    /// On parse/type error, returns Err but keeps old code running.
    pub fn reload(&mut self, new_source: &str) -> Result<ReloadResult, CodegenError> {
        // Short-circuit: if expanded source is identical, skip everything
        if new_source == self.prev_source {
            return Ok(ReloadResult {
                recompiled: vec![],
                skipped: self.fn_slots.len(),
                type_warnings: vec![],
            });
        }

        let (ast_module, parse_errors) = weir_parser::parse(new_source);
        if !parse_errors.is_empty() {
            return Err(CodegenError::new(format!(
                "parse error: {}",
                parse_errors[0].message
            )));
        }

        let type_info = weir_typeck::check(&ast_module);
        if !type_info.errors.is_empty() {
            return Err(CodegenError::new(format!(
                "type error: {}",
                type_info.errors[0].message
            )));
        }

        // Compute change set
        let new_body_hashes = compute_body_hashes(&ast_module, new_source);
        let new_type_sigs = compute_type_sigs(&ast_module, &type_info);

        let old_fn_names: HashSet<SmolStr> = self.fn_slots.keys().cloned().collect();
        let new_fn_names: HashSet<SmolStr> = ast_module
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Defn(defn) = item {
                    Some(defn.name.clone())
                } else {
                    None
                }
            })
            .collect();

        let mut change_set = ChangeSet {
            body_changed: HashSet::new(),
            sig_changed: HashSet::new(),
            added: HashSet::new(),
            removed: HashSet::new(),
            types_changed: HashSet::new(),
        };

        // Detect added/removed functions
        for name in &new_fn_names {
            if !old_fn_names.contains(name) {
                change_set.added.insert(name.clone());
            }
        }
        for name in &old_fn_names {
            if !new_fn_names.contains(name) {
                change_set.removed.insert(name.clone());
            }
        }

        // Detect body and signature changes for existing functions
        for name in new_fn_names.intersection(&old_fn_names) {
            // Signature change?
            let old_sig = self.fn_sigs.get(name);
            let new_sig = type_info.fn_types.get(name).map(|ft| {
                (ft.param_types.clone(), ft.return_type.clone())
            });
            let sig_changed = match (old_sig, &new_sig) {
                (Some(old), Some(new)) => old != new,
                _ => true,
            };
            if sig_changed {
                change_set.sig_changed.insert(name.clone());
                continue; // sig change subsumes body change
            }

            // Body change?
            let old_hash = self.body_hashes.get(name);
            let new_hash = new_body_hashes.get(name);
            if old_hash != new_hash {
                change_set.body_changed.insert(name.clone());
            }
        }

        // Detect type changes
        for (type_name, new_cons) in &new_type_sigs {
            match self.type_sigs.get(type_name) {
                Some(old_cons) if old_cons == new_cons => {}
                _ => {
                    change_set.types_changed.insert(type_name.clone());
                }
            }
        }
        // Check for removed types
        for type_name in self.type_sigs.keys() {
            if !new_type_sigs.contains_key(type_name) {
                change_set.types_changed.insert(type_name.clone());
            }
        }

        // Compute dirty set using dependency graph + change set
        let deps = &type_info.deps;
        let mut dirty: HashSet<SmolStr> = HashSet::new();

        // Body-only changes: just the function itself
        for name in &change_set.body_changed {
            dirty.insert(name.clone());
        }

        // Signature changes: function + transitive callers
        for name in &change_set.sig_changed {
            dirty.insert(name.clone());
            self.add_transitive_callers(name, deps, &mut dirty);
        }

        // Type changes: all functions using the type + their transitive callers
        for type_name in &change_set.types_changed {
            if let Some(users) = deps.type_users.get(type_name) {
                let users_snapshot: Vec<SmolStr> = users.iter().cloned().collect();
                for user in &users_snapshot {
                    dirty.insert(user.clone());
                    self.add_transitive_callers(user, deps, &mut dirty);
                }
            }
        }

        // Added functions
        for name in &change_set.added {
            dirty.insert(name.clone());
        }

        // Expand dirty set to include specializations of dirty functions
        let mut spec_dirty: Vec<SmolStr> = Vec::new();
        for spec in &type_info.specializations {
            if dirty.contains(&spec.original_name) {
                spec_dirty.push(spec.mangled_name.clone());
            }
        }
        dirty.extend(spec_dirty);

        // Type warnings
        let type_warnings: Vec<SmolStr> = change_set.types_changed.iter().cloned().collect();

        // If nothing is dirty, skip compilation
        if dirty.is_empty() {
            self.prev_source = new_source.to_string();
            return Ok(ReloadResult {
                recompiled: vec![],
                skipped: self.fn_slots.len(),
                type_warnings,
            });
        }

        // Reassign slots for the new source
        let mut new_fn_slots = HashMap::new();
        let mut slot = 0usize;
        for (item, _) in &ast_module.items {
            if let Item::Defn(defn) = item {
                new_fn_slots.insert(defn.name.clone(), slot);
                slot += 1;
            }
        }

        // If function count changed, grow the table
        if slot > self.fn_table.len() {
            let mut new_table: Vec<AtomicPtr<u8>> = Vec::with_capacity(slot);
            for i in 0..slot {
                if i < self.fn_table.len() {
                    let old_ptr = self.fn_table[i].load(Ordering::Acquire);
                    new_table.push(AtomicPtr::new(old_ptr));
                } else {
                    new_table.push(AtomicPtr::new(std::ptr::null_mut()));
                }
            }
            self.fn_table = new_table.into_boxed_slice();
        }

        let (jit_module, user_fns, new_fn_sigs) = compile_dev_module(
            &ast_module,
            &type_info,
            self.fn_table.as_ptr(),
            &new_fn_slots,
            Some(&dirty),
        )?;

        // Swap only dirty function pointers in the table
        let mut recompiled = Vec::new();
        for (name, &slot_idx) in &new_fn_slots {
            if !dirty.contains(name) {
                continue;
            }
            if let Some((func_id, _, _)) = user_fns.get(name) {
                let ptr = jit_module.get_finalized_function(*func_id);
                self.fn_table[slot_idx].store(ptr as *mut u8, Ordering::Release);
                recompiled.push(name.clone());
            }
        }

        let skipped = new_fn_slots.len() - recompiled.len();

        // Keep old module alive
        let old_module = std::mem::replace(&mut self.current_module, jit_module);
        self._old_modules.push(old_module);

        // Update session state
        self.ast_module = ast_module;
        self.type_info = type_info;
        self.fn_slots = new_fn_slots;
        self.fn_sigs = new_fn_sigs;
        self.body_hashes = new_body_hashes;
        self.type_sigs = new_type_sigs;
        self.prev_source = new_source.to_string();

        Ok(ReloadResult {
            recompiled,
            skipped,
            type_warnings,
        })
    }

    /// BFS to find all transitive callers of a function.
    fn add_transitive_callers(
        &self,
        name: &SmolStr,
        deps: &weir_typeck::DependencyGraph,
        dirty: &mut HashSet<SmolStr>,
    ) {
        let mut queue = std::collections::VecDeque::new();
        queue.push_back(name.clone());
        while let Some(current) = queue.pop_front() {
            if let Some(callers) = deps.callers.get(&current) {
                for caller in callers {
                    if dirty.insert(caller.clone()) {
                        queue.push_back(caller.clone());
                    }
                }
            }
        }
    }

    /// Get a callable function pointer for main() (reads through the fn_table).
    fn get_main_fn_ptr(&self) -> Result<*const u8, CodegenError> {
        let slot = self
            .fn_slots
            .get("main")
            .ok_or_else(|| CodegenError::new("no 'main' function defined"))?;
        let ptr = self.fn_table[*slot].load(Ordering::Acquire);
        if ptr.is_null() {
            return Err(CodegenError::new("main function pointer is null"));
        }
        Ok(ptr as *const u8)
    }

    /// Run main() and capture output (for testing).
    pub fn run_main(&self) -> Result<String, CodegenError> {
        let ptr = self.get_main_fn_ptr()?;
        output_reset();
        unsafe {
            let main_fn: fn() = std::mem::transmute(ptr);
            main_fn();
        }
        Ok(output_take())
    }

    /// Run the dev loop: spawn main() on a background thread, watch for file changes.
    /// `transform_source` is called on the raw file contents before reload (e.g. prepend prelude + expand macros).
    pub fn run_dev_loop<F>(
        mut self,
        source_path: &Path,
        transform_source: F,
    ) -> Result<(), CodegenError>
    where
        F: Fn(&str) -> Result<String, String>,
    {
        use notify::{RecursiveMode, Watcher};
        use std::sync::mpsc;
        use std::time::Duration;

        let main_ptr = self.get_main_fn_ptr()?;

        // Cast to usize (which is Send) to transfer across thread boundary.
        // Safety: the fn_table and _old_modules keep the code pages alive.
        let main_ptr_int = main_ptr as usize;

        // Spawn main() on a background thread.
        // main_fn calls through the fn_table, so pointer swaps take effect automatically.
        let main_handle = std::thread::spawn(move || unsafe {
            let main_fn: fn() = std::mem::transmute(main_ptr_int);
            main_fn();
        });

        eprintln!(
            "[weir dev] running \u{2014} watching {} for changes",
            source_path.display()
        );

        // Set up file watcher
        let (tx, rx) = mpsc::channel();
        let mut watcher = notify::recommended_watcher(move |res: notify::Result<notify::Event>| {
            if let Ok(event) = res {
                if event.kind.is_modify() {
                    let _ = tx.send(());
                }
            }
        })
        .map_err(|e| CodegenError::new(format!("watcher setup: {}", e)))?;

        let watch_path = source_path.parent().unwrap_or(Path::new("."));
        watcher
            .watch(watch_path, RecursiveMode::NonRecursive)
            .map_err(|e| CodegenError::new(format!("watch: {}", e)))?;

        let source_path = source_path.to_path_buf();

        loop {
            // Check if main thread has exited
            if main_handle.is_finished() {
                eprintln!("[weir dev] main() exited");
                break;
            }

            // Wait for a file change event (with timeout to check main thread)
            match rx.recv_timeout(Duration::from_millis(200)) {
                Ok(()) => {
                    // Debounce: drain any queued events
                    std::thread::sleep(Duration::from_millis(50));
                    while rx.try_recv().is_ok() {}

                    // Read new source and apply transform (prelude + macro expansion)
                    let raw_source = match std::fs::read_to_string(&source_path) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!("[weir dev] error reading file: {}", e);
                            continue;
                        }
                    };

                    let new_source = match transform_source(&raw_source) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!("[weir dev] transform error: {}", e);
                            continue;
                        }
                    };

                    match self.reload(&new_source) {
                        Ok(result) => {
                            for tw in &result.type_warnings {
                                eprintln!(
                                    "[weir dev] type '{}' was redefined \u{2014} live instances have stale layout",
                                    tw
                                );
                            }
                            if result.recompiled.is_empty() {
                                eprintln!("[weir dev] no changes detected");
                            } else {
                                let names: Vec<&str> =
                                    result.recompiled.iter().map(|s| s.as_str()).collect();
                                eprintln!(
                                    "[weir dev] reloaded {} fn(s): {} (skipped {})",
                                    result.recompiled.len(),
                                    names.join(", "),
                                    result.skipped
                                );
                            }
                        }
                        Err(e) => {
                            eprintln!("[weir dev] reload error (old code still running): {}", e);
                        }
                    }
                }
                Err(mpsc::RecvTimeoutError::Timeout) => continue,
                Err(mpsc::RecvTimeoutError::Disconnected) => break,
            }
        }

        Ok(())
    }
}
