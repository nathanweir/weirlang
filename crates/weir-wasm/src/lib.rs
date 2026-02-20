#![allow(dead_code)]

mod emit;
pub(crate) mod runtime;
pub(crate) mod types;

pub mod glue;

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use smol_str::SmolStr;
use wasm_encoder::{
    CodeSection, DataSection, ElementSection, Elements, ExportKind, ExportSection,
    FunctionSection, GlobalSection, GlobalType, ImportSection, MemorySection, MemoryType, Module,
    TableSection, TableType, TypeSection, ValType,
};
use weir_ast::{ExprId, Item};
use weir_typeck::Ty;
use weir_typeck::TypeCheckResult;

use runtime::{
    emit_gc_alloc, emit_gc_str_alloc, emit_gc_str_dup, emit_gc_vec_alloc, emit_i64_to_str,
    emit_noop, emit_shadow_pop, emit_shadow_push, emit_str_concat, emit_str_eq,
    emit_string_length, emit_vec_get, emit_vec_len, emit_vec_set, GC_HEAP_START, INITIAL_PAGES,
    JS_IMPORTS,
};
use types::{ty_to_wasm, wasm_val_size, TaggedAdt};

// ── Error ────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct WasmError {
    pub message: String,
}

impl WasmError {
    fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

impl std::fmt::Display for WasmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "wasm error: {}", self.message)
    }
}

impl std::error::Error for WasmError {}

// ── Runtime function definitions (compiled into the WASM module) ─

/// Each internal runtime function: (name, params, results, locals, emitter).
struct RuntimeFn {
    name: &'static str,
    params: &'static [ValType],
    results: &'static [ValType],
    extra_locals: &'static [ValType],
    emitter: fn(&mut wasm_encoder::Function),
}

const RUNTIME_FNS: &[RuntimeFn] = &[
    RuntimeFn {
        name: "weir_gc_alloc",
        params: &[ValType::I32, ValType::I64],
        results: &[ValType::I32],
        extra_locals: &[ValType::I32],
        emitter: emit_gc_alloc,
    },
    RuntimeFn {
        name: "weir_gc_vec_alloc",
        params: &[ValType::I64, ValType::I64],
        results: &[ValType::I32],
        extra_locals: &[ValType::I32, ValType::I32],
        emitter: emit_gc_vec_alloc,
    },
    RuntimeFn {
        name: "weir_gc_str_alloc",
        params: &[ValType::I64],
        results: &[ValType::I32],
        extra_locals: &[ValType::I32, ValType::I32],
        emitter: emit_gc_str_alloc,
    },
    RuntimeFn {
        name: "weir_gc_str_dup",
        params: &[ValType::I32],
        results: &[ValType::I32],
        extra_locals: &[ValType::I32, ValType::I32, ValType::I32],
        emitter: emit_gc_str_dup,
    },
    RuntimeFn {
        name: "weir_vec_get",
        params: &[ValType::I32, ValType::I64],
        results: &[ValType::I64],
        extra_locals: &[],
        emitter: emit_vec_get,
    },
    RuntimeFn {
        name: "weir_vec_len",
        params: &[ValType::I32],
        results: &[ValType::I64],
        extra_locals: &[],
        emitter: emit_vec_len,
    },
    RuntimeFn {
        name: "weir_vec_set",
        params: &[ValType::I32, ValType::I64, ValType::I64],
        results: &[],
        extra_locals: &[],
        emitter: emit_vec_set,
    },
    RuntimeFn {
        name: "weir_str_eq",
        params: &[ValType::I32, ValType::I32],
        results: &[ValType::I64],
        extra_locals: &[ValType::I32, ValType::I32, ValType::I32],
        emitter: emit_str_eq,
    },
    RuntimeFn {
        name: "weir_str_concat",
        params: &[ValType::I32, ValType::I32],
        results: &[ValType::I32],
        extra_locals: &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        emitter: emit_str_concat,
    },
    RuntimeFn {
        name: "weir_string_length",
        params: &[ValType::I32],
        results: &[ValType::I64],
        extra_locals: &[ValType::I32],
        emitter: emit_string_length,
    },
    RuntimeFn {
        name: "weir_i64_to_str",
        params: &[ValType::I64],
        results: &[ValType::I32],
        extra_locals: &[],
        emitter: emit_i64_to_str,
    },
    RuntimeFn {
        name: "weir_shadow_push",
        params: &[ValType::I32],
        results: &[],
        extra_locals: &[],
        emitter: emit_shadow_push,
    },
    RuntimeFn {
        name: "weir_shadow_pop",
        params: &[],
        results: &[],
        extra_locals: &[],
        emitter: emit_shadow_pop,
    },
];

// ── WasmCompiler ─────────────────────────────────────────────────

pub struct WasmCompiler<'a> {
    ast_module: &'a weir_ast::Module,
    type_info: &'a TypeCheckResult,

    // Function index tracking
    func_indices: HashMap<SmolStr, u32>,
    runtime_fn_indices: HashMap<&'static str, u32>,
    next_func_index: u32,

    // Type section
    type_index_cache: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    next_type_index: u32,

    // Tagged ADTs and constructors
    tagged_adts: HashMap<SmolStr, TaggedAdt>,
    constructor_tags: HashMap<SmolStr, u32>,
    struct_names: HashSet<SmolStr>,

    // Per-function compilation state
    locals: HashMap<SmolStr, u32>,
    next_local: u32,
    local_types: Vec<ValType>,

    // String interning
    string_data: Vec<u8>,
    string_offsets: HashMap<String, u32>,
    data_offset: u32,
}

impl<'a> WasmCompiler<'a> {
    fn new(ast_module: &'a weir_ast::Module, type_info: &'a TypeCheckResult) -> Self {
        let mut tagged_adts = HashMap::new();
        let mut constructor_tags = HashMap::new();
        let mut struct_names = HashSet::new();

        // Collect tagged ADTs from deftype definitions
        for (item, _span) in &ast_module.items {
            if let Item::Deftype(d) = item {
                let all_supported =
                    !d.variants.is_empty() && d.variants.iter().all(|v| v.fields.len() <= 1);
                if all_supported {
                    let variants: Vec<(SmolStr, Vec<Ty>)> = d
                        .variants
                        .iter()
                        .map(|v| (v.name.clone(), Vec::new()))
                        .collect();
                    for (i, v) in d.variants.iter().enumerate() {
                        constructor_tags.insert(v.name.clone(), i as u32);
                    }
                    tagged_adts.insert(
                        d.name.clone(),
                        TaggedAdt {
                            type_name: d.name.clone(),
                            variants,
                        },
                    );
                }
            }
            if let Item::Defstruct(d) = item {
                struct_names.insert(d.name.clone());
            }
        }

        Self {
            ast_module,
            type_info,
            func_indices: HashMap::new(),
            runtime_fn_indices: HashMap::new(),
            next_func_index: 0,
            type_index_cache: HashMap::new(),
            next_type_index: 0,
            tagged_adts,
            constructor_tags,
            struct_names,
            locals: HashMap::new(),
            next_local: 0,
            local_types: Vec::new(),
            string_data: Vec::new(),
            string_offsets: HashMap::new(),
            data_offset: runtime::STATIC_DATA_START,
        }
    }

    /// Look up the resolved type for an expression.
    fn expr_ty(&self, expr_id: ExprId) -> Ty {
        self.type_info
            .expr_types
            .get(expr_id)
            .cloned()
            .unwrap_or(Ty::Unit)
    }

    /// Intern a string literal in the data section. Returns its linear memory offset.
    fn intern_string(&mut self, s: &str) -> u32 {
        if let Some(&offset) = self.string_offsets.get(s) {
            return offset;
        }
        let offset = self.data_offset;
        // Store null-terminated
        self.string_data.extend_from_slice(s.as_bytes());
        self.string_data.push(0);
        // Align to 8
        while !self.string_data.len().is_multiple_of(8) {
            self.string_data.push(0);
        }
        self.data_offset = runtime::STATIC_DATA_START + self.string_data.len() as u32;
        self.string_offsets.insert(s.to_owned(), offset);
        offset
    }

    /// Get the byte offset of a struct field in linear memory.
    fn get_field_offset(&self, struct_ty: &Ty, field_name: &SmolStr) -> Option<u32> {
        let struct_name = match struct_ty {
            Ty::Con(name, _) => name,
            _ => return None,
        };
        let info = self.type_info.struct_defs.get(struct_name)?;
        let mut offset = 0u32;
        for (fname, fty) in &info.fields {
            if fname == field_name {
                return Some(offset);
            }
            let wt = self.wasm_ty(fty).unwrap_or(ValType::I32);
            offset += wasm_val_size(wt);
        }
        None
    }

    /// Register or retrieve a function type index.
    fn func_type_index(
        &mut self,
        types: &mut TypeSection,
        params: Vec<ValType>,
        results: Vec<ValType>,
    ) -> u32 {
        let key = (params.clone(), results.clone());
        if let Some(&idx) = self.type_index_cache.get(&key) {
            return idx;
        }
        let idx = self.next_type_index;
        self.next_type_index += 1;
        types.ty().function(params, results);
        self.type_index_cache.insert(key, idx);
        idx
    }

    /// Convert a Weir type to a WASM value type, using this compiler's ADT/struct info.
    fn wasm_ty(&self, ty: &Ty) -> Option<ValType> {
        ty_to_wasm(ty, Some(&self.tagged_adts), Some(&self.struct_names))
    }

    /// Build a mangled name for an instance method: ClassName#TypeArg#MethodName
    fn mangled_instance_method(
        ast_module: &weir_ast::Module,
        inst: &weir_ast::InstanceDef,
        method_name: &SmolStr,
    ) -> SmolStr {
        let type_arg_name = if let Some(&type_arg_id) = inst.type_args.first() {
            match &ast_module.type_exprs[type_arg_id].kind {
                weir_ast::TypeExprKind::Named(name) => name.as_str().to_string(),
                _ => "unknown".to_string(),
            }
        } else {
            "unknown".to_string()
        };
        SmolStr::new(format!("{}#{}#{}", inst.class_name, type_arg_name, method_name))
    }

    /// Reset per-function local tracking.
    fn reset_locals(&mut self) {
        self.locals.clear();
        self.next_local = 0;
        self.local_types.clear();
    }

    // ── Three-pass compilation ───────────────────────────────────

    fn compile(mut self) -> Result<Vec<u8>, WasmError> {
        let mut types = TypeSection::new();
        let mut imports = ImportSection::new();
        let mut functions = FunctionSection::new();
        let mut tables = TableSection::new();
        let mut memory = MemorySection::new();
        let mut globals = GlobalSection::new();
        let mut exports = ExportSection::new();
        let mut elements = ElementSection::new();
        let mut data = DataSection::new();
        let mut code = CodeSection::new();

        // ── Pass 0: Memory, globals, table ───────────────────────

        // Linear memory: initial 256 pages (16MB), maximum 65536 pages (4GB)
        memory.memory(MemoryType {
            minimum: INITIAL_PAGES as u64,
            maximum: Some(65536),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        exports.export("memory", ExportKind::Memory, 0);

        // Globals: heap pointer, shadow stack pointer, wasm state
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(GC_HEAP_START as i32),
        );
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(runtime::SHADOW_STACK_START as i32),
        );
        globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );

        // Function table (for closures / call_indirect)
        tables.table(TableType {
            element_type: wasm_encoder::RefType::FUNCREF,
            minimum: 128,
            maximum: Some(1024),
            table64: false,
            shared: false,
        });

        // ── Pass 1: Declare JS imports ───────────────────────────

        for &(name, params, results) in JS_IMPORTS {
            let type_idx =
                self.func_type_index(&mut types, params.to_vec(), results.to_vec());
            imports.import("env", name, wasm_encoder::EntityType::Function(type_idx));
            self.runtime_fn_indices.insert(name, self.next_func_index);
            self.next_func_index += 1;
        }

        // Declare extern "C" functions as imports too
        for (item, _) in &self.ast_module.items {
            if let Item::ExternC(ext) = item {
                for decl in &ext.declarations {
                    if self.func_indices.contains_key(&decl.name) {
                        continue;
                    }
                    let fn_ty = self.type_info.fn_types.get(&decl.name);
                    let (param_vts, result_vts) = if let Some(ft) = fn_ty {
                        let params: Vec<ValType> = ft
                            .param_types
                            .iter()
                            .filter_map(|t| self.wasm_ty(t))
                            .collect();
                        let results: Vec<ValType> =
                            self.wasm_ty(&ft.return_type).into_iter().collect();
                        (params, results)
                    } else {
                        (vec![], vec![])
                    };
                    let type_idx =
                        self.func_type_index(&mut types, param_vts, result_vts);
                    imports.import(
                        "env",
                        decl.name.as_str(),
                        wasm_encoder::EntityType::Function(type_idx),
                    );
                    self.func_indices
                        .insert(decl.name.clone(), self.next_func_index);
                    self.next_func_index += 1;
                }
            }
        }

        // ── Pass 2: Declare internal runtime functions ───────────

        let runtime_start_index = self.next_func_index;
        for rt in RUNTIME_FNS {
            let type_idx =
                self.func_type_index(&mut types, rt.params.to_vec(), rt.results.to_vec());
            functions.function(type_idx);
            self.runtime_fn_indices.insert(rt.name, self.next_func_index);
            self.next_func_index += 1;
        }

        // ── Pass 3: Declare user functions ───────────────────────

        let mut user_fn_order: Vec<SmolStr> = Vec::new();

        for (item, _) in &self.ast_module.items {
            if let Item::Defn(d) = item {
                let fn_ty = self.type_info.fn_types.get(&d.name);
                let (param_vts, result_vts) = if let Some(ft) = fn_ty {
                    let params: Vec<ValType> = ft
                        .param_types
                        .iter()
                        .filter_map(|t| {
                            ty_to_wasm(
                                t,
                                Some(&self.tagged_adts),
                                Some(&self.struct_names),
                            )
                        })
                        .collect();
                    let results: Vec<ValType> = ty_to_wasm(
                        &ft.return_type,
                        Some(&self.tagged_adts),
                        Some(&self.struct_names),
                    )
                    .into_iter()
                    .collect();
                    (params, results)
                } else {
                    (vec![], vec![])
                };
                let type_idx = self.func_type_index(&mut types, param_vts, result_vts);
                functions.function(type_idx);
                self.func_indices
                    .insert(d.name.clone(), self.next_func_index);
                self.next_func_index += 1;
                user_fn_order.push(d.name.clone());
            }
        }

        // Instance methods
        for (item, _) in &self.ast_module.items {
            if let Item::Instance(inst) = item {
                for method in &inst.methods {
                    let mangled = Self::mangled_instance_method(
                        self.ast_module, inst, &method.name,
                    );
                    let fn_ty = self.type_info.fn_types.get(&mangled)
                        .or_else(|| self.type_info.fn_types.get(&method.name));
                    let (param_vts, result_vts) = if let Some(ft) = fn_ty {
                        let params: Vec<ValType> = ft
                            .param_types
                            .iter()
                            .filter_map(|t| self.wasm_ty(t))
                            .collect();
                        let results: Vec<ValType> =
                            self.wasm_ty(&ft.return_type).into_iter().collect();
                        (params, results)
                    } else {
                        (vec![], vec![])
                    };
                    let type_idx =
                        self.func_type_index(&mut types, param_vts, result_vts);
                    functions.function(type_idx);
                    self.func_indices
                        .insert(mangled.clone(), self.next_func_index);
                    self.next_func_index += 1;
                    user_fn_order.push(mangled);
                }
            }
        }

        // Export weir_main and optionally weir_frame
        if let Some(&main_idx) = self.func_indices.get("main") {
            exports.export("weir_main", ExportKind::Func, main_idx);
        }
        if let Some(&frame_idx) = self.func_indices.get("weir-frame") {
            exports.export("weir_frame", ExportKind::Func, frame_idx);
        }

        // ── Pass 4: Compile runtime function bodies ──────────────

        for rt in RUNTIME_FNS {
            let mut func = wasm_encoder::Function::new(
                rt.extra_locals.iter().map(|vt| (1, *vt)).collect::<Vec<_>>(),
            );
            (rt.emitter)(&mut func);
            code.function(&func);
        }

        // ── Pass 5: Compile user function bodies ─────────────────
        //
        // Two-pass per function:
        //   1. Compile to discover which extra locals are needed
        //   2. Re-compile into a Function that declares those locals up front

        for name in &user_fn_order {
            let defn = self.find_defn(name);
            if let Some(d) = defn {
                let fn_ty = self.type_info.fn_types.get(name);
                let param_count = d.params.len() as u32;
                let body = d.body.clone();

                let expected_result = fn_ty.and_then(|ft| {
                    ty_to_wasm(
                        &ft.return_type,
                        Some(&self.tagged_adts),
                        Some(&self.struct_names),
                    )
                });

                // Helper: set up parameter locals
                let setup_params = |compiler: &mut Self| {
                    compiler.reset_locals();
                    for (i, param) in d.params.iter().enumerate() {
                        compiler.locals.insert(param.name.clone(), i as u32);
                    }
                    compiler.next_local = param_count;
                };

                // Pass 1: discover locals
                setup_params(&mut self);
                let mut dummy = wasm_encoder::Function::new(vec![]);
                self.compile_body(&body, &mut dummy);

                // Capture the extra locals discovered
                let extra_locals: Vec<(u32, ValType)> = if self.local_types.len() > param_count as usize {
                    self.local_types[param_count as usize..]
                        .iter()
                        .map(|vt| (1, *vt))
                        .collect()
                } else {
                    vec![]
                };

                // Pass 2: compile for real with locals declared
                setup_params(&mut self);
                let mut func = wasm_encoder::Function::new(extra_locals);
                let result_wt = self.compile_body(&body, &mut func);

                // Fixup return value
                if let (Some(expected), None) = (expected_result, result_wt) {
                    match expected {
                        ValType::I32 => { func.instruction(&wasm_encoder::Instruction::I32Const(0)); }
                        ValType::I64 => { func.instruction(&wasm_encoder::Instruction::I64Const(0)); }
                        ValType::F32 => { func.instruction(&wasm_encoder::Instruction::F32Const(0.0)); }
                        ValType::F64 => { func.instruction(&wasm_encoder::Instruction::F64Const(0.0)); }
                        _ => {}
                    }
                } else if expected_result.is_none() && result_wt.is_some() {
                    func.instruction(&wasm_encoder::Instruction::Drop);
                }
                func.instruction(&wasm_encoder::Instruction::End);

                code.function(&func);
            } else {
                // No definition found — emit an empty function
                let mut func = wasm_encoder::Function::new(vec![]);
                emit_noop(&mut func);
                code.function(&func);
            }
        }

        // ── Pass 6: Data section (string literals) ───────────────

        if !self.string_data.is_empty() {
            data.active(
                0,
                &wasm_encoder::ConstExpr::i32_const(runtime::STATIC_DATA_START as i32),
                self.string_data.iter().copied(),
            );
        }

        // ── Pass 7: Element section (function table entries) ─────
        // For now, populate with all internal functions for call_indirect support
        let func_table_entries: Vec<u32> = (runtime_start_index..self.next_func_index).collect();
        if !func_table_entries.is_empty() {
            elements.active(
                Some(0),
                &wasm_encoder::ConstExpr::i32_const(0),
                Elements::Functions(std::borrow::Cow::Borrowed(&func_table_entries)),
            );
        }

        // ── Assemble module ──────────────────────────────────────

        let mut module = Module::new();
        module.section(&types);
        module.section(&imports);
        module.section(&functions);
        module.section(&tables);
        module.section(&memory);
        module.section(&globals);
        module.section(&exports);
        module.section(&elements);
        module.section(&code);
        module.section(&data);

        Ok(module.finish())
    }

    /// Find a function definition by name (top-level defn or instance method).
    /// Handles mangled instance names like "Ord#i64#compare".
    fn find_defn(&self, name: &SmolStr) -> Option<weir_ast::Defn> {
        // Check top-level defns first
        for (item, _) in &self.ast_module.items {
            if let Item::Defn(d) = item {
                if d.name == *name {
                    return Some(d.clone());
                }
            }
        }

        // Check instance methods — handle mangled names
        if let Some((_class_type_part, method_name)) = name.rsplit_once('#') {
            // Mangled: "ClassName#TypeArg#method"
            for (item, _) in &self.ast_module.items {
                if let Item::Instance(inst) = item {
                    let mangled = Self::mangled_instance_method(
                        self.ast_module, inst, &SmolStr::new(method_name),
                    );
                    if mangled == *name {
                        for m in &inst.methods {
                            if m.name == method_name {
                                return Some(m.clone());
                            }
                        }
                    }
                }
            }
        } else {
            // Unmangled — search all instances
            for (item, _) in &self.ast_module.items {
                if let Item::Instance(inst) = item {
                    for m in &inst.methods {
                        if m.name == *name {
                            return Some(m.clone());
                        }
                    }
                }
            }
        }

        None
    }
}

// ── Public API ───────────────────────────────────────────────────

/// Compile a Weir module to a .wasm binary.
pub fn compile_to_wasm(
    module: &weir_ast::Module,
    type_info: &TypeCheckResult,
) -> Result<Vec<u8>, WasmError> {
    let compiler = WasmCompiler::new(module, type_info);
    compiler.compile()
}

/// Build a complete WASM deployment package (wasm + JS glue + HTML).
pub fn build_wasm_package(
    module: &weir_ast::Module,
    type_info: &TypeCheckResult,
    bridge_files: &[PathBuf],
    output_dir: &Path,
) -> Result<(), WasmError> {
    let wasm_bytes = compile_to_wasm(module, type_info)?;

    std::fs::create_dir_all(output_dir)
        .map_err(|e| WasmError::new(format!("creating output dir: {}", e)))?;

    // Write app.wasm
    std::fs::write(output_dir.join("app.wasm"), &wasm_bytes)
        .map_err(|e| WasmError::new(format!("writing app.wasm: {}", e)))?;

    // Write runtime.js
    std::fs::write(output_dir.join("runtime.js"), glue::RUNTIME_JS)
        .map_err(|e| WasmError::new(format!("writing runtime.js: {}", e)))?;

    // Write loader.js
    let has_gl = bridge_files.iter().any(|p| {
        p.file_name()
            .map(|n| n.to_string_lossy().contains("gl_bridge"))
            .unwrap_or(false)
    });
    let loader = glue::generate_loader(has_gl);
    std::fs::write(output_dir.join("loader.js"), &loader)
        .map_err(|e| WasmError::new(format!("writing loader.js: {}", e)))?;

    // Write index.html
    let html = glue::generate_html("Weir App");
    std::fs::write(output_dir.join("index.html"), &html)
        .map_err(|e| WasmError::new(format!("writing index.html: {}", e)))?;

    // Copy bridge files
    for bridge in bridge_files {
        if bridge.exists() {
            let dest = output_dir.join(bridge.file_name().unwrap());
            std::fs::copy(bridge, &dest)
                .map_err(|e| WasmError::new(format!("copying bridge file: {}", e)))?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile_and_validate_with_prelude(source: &str) -> Vec<u8> {
        let prelude = include_str!("../../../std/prelude.weir");
        let full = format!("{}\n{}", prelude, source);
        compile_and_validate(&full)
    }

    fn compile_and_validate(source: &str) -> Vec<u8> {
        let expanded = weir_macros::expand(source);
        assert!(expanded.errors.is_empty(), "macro errors: {:?}", expanded.errors);

        let (module, parse_errors) = weir_parser::parse(&expanded.source);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);

        let type_info = weir_typeck::check(&module);

        let wasm_bytes = compile_to_wasm(&module, &type_info).expect("WASM compilation failed");

        let mut validator = wasmparser::Validator::new();
        match validator.validate_all(&wasm_bytes) {
            Ok(_) => {}
            Err(e) => {
                panic!("WASM validation failed: {} (offset: {:?})", e, e.offset());
            }
        }

        wasm_bytes
    }

    #[test]
    fn empty_main_validates() {
        compile_and_validate("(defn main () : Unit)");
    }

    #[test]
    fn hello_world_validates() {
        compile_and_validate(r#"(defn main () : Unit (println "Hello from WASM!"))"#);
    }

    #[test]
    fn arithmetic_validates() {
        compile_and_validate(r#"
            (defn add ((a : i64) (b : i64)) : i64 (+ a b))
            (defn main () : Unit (println (add 10 20)))
        "#);
    }

    #[test]
    fn if_expression_validates() {
        compile_and_validate(r#"
            (defn main () : Unit (if true (println "yes") (println "no")))
        "#);
    }

    #[test]
    fn with_prelude_validates() {
        compile_and_validate_with_prelude(r#"(defn main () : Unit (println "Hello"))"#);
    }

    #[test]
    fn adt_and_instance_validates() {
        compile_and_validate(r#"
            (deftype Ordering LT EQ GT)
            (deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
            (defclass (Ord 'a)
              (compare : (Fn ['a 'a] Ordering)))
            (instance (Ord i64)
              (defn compare ((x : i64) (y : i64)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (instance (Ord f32)
              (defn compare ((x : f32) (y : f32)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (defn main () : Unit
              (println "ok"))
        "#);
    }

    #[test]
    fn many_instances_validates() {
        compile_and_validate(r#"
            (deftype Ordering LT EQ GT)
            (defclass (Ord 'a)
              (compare : (Fn ['a 'a] Ordering)))
            (instance (Ord i8)
              (defn compare ((x : i8) (y : i8)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (instance (Ord i32)
              (defn compare ((x : i32) (y : i32)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (instance (Ord i64)
              (defn compare ((x : i64) (y : i64)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (instance (Ord f32)
              (defn compare ((x : f32) (y : f32)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (instance (Ord f64)
              (defn compare ((x : f64) (y : f64)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (instance (Ord String)
              (defn compare ((x : String) (y : String)) : Ordering
                (if (< x y) LT (if (> x y) GT EQ))))
            (defn main () : Unit
              (println "ok"))
        "#);
    }
}
