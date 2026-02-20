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

use weir_ast::tco;

use runtime::{
    emit_bool_to_str, emit_f64_to_str, emit_gc_alloc, emit_gc_str_alloc, emit_gc_str_dup,
    emit_gc_vec_alloc, emit_i64_to_str, emit_noop, emit_random_int, emit_random_seed,
    emit_shadow_pop, emit_shadow_push, emit_str_cmp, emit_str_concat, emit_str_eq,
    emit_string_contains, emit_string_length, emit_string_ref, emit_substring, emit_vec_append,
    emit_vec_get, emit_vec_len, emit_vec_set, emit_vec_set_nth, GC_HEAP_START, INITIAL_PAGES,
    JS_IMPORTS,
};
use types::{ty_to_wasm, TaggedAdt};

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
        extra_locals: &[
            ValType::I32, // buf
            ValType::I32, // pos
            ValType::I32, // is_neg
            ValType::I64, // digit
            ValType::I32, // dst
            ValType::I32, // len
            ValType::I32, // i
        ],
        emitter: emit_i64_to_str,
    },
    RuntimeFn {
        name: "weir_vec_set_nth",
        params: &[ValType::I32, ValType::I64, ValType::I64],
        results: &[ValType::I32],
        extra_locals: &[ValType::I64, ValType::I32, ValType::I64],
        emitter: emit_vec_set_nth,
    },
    RuntimeFn {
        name: "weir_vec_append",
        params: &[ValType::I32, ValType::I64],
        results: &[ValType::I32],
        extra_locals: &[ValType::I64, ValType::I32, ValType::I64],
        emitter: emit_vec_append,
    },
    RuntimeFn {
        name: "weir_random_seed",
        params: &[ValType::I64],
        results: &[],
        extra_locals: &[],
        emitter: emit_random_seed,
    },
    RuntimeFn {
        name: "weir_random_int",
        params: &[ValType::I64],
        results: &[ValType::I64],
        extra_locals: &[ValType::I64],
        emitter: emit_random_int,
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
    RuntimeFn {
        name: "weir_f64_to_str",
        params: &[ValType::F64],
        results: &[ValType::I32],
        extra_locals: &[
            ValType::I32, // buf
            ValType::I32, // pos
            ValType::I32, // is_neg
            ValType::I64, // int_part
            ValType::I64, // frac_part
            ValType::I64, // digit
            ValType::I32, // i
            ValType::F64, // temp_f64
        ],
        emitter: emit_f64_to_str,
    },
    RuntimeFn {
        name: "weir_bool_to_str",
        params: &[ValType::I32],
        results: &[ValType::I32],
        extra_locals: &[ValType::I32],
        emitter: emit_bool_to_str,
    },
    RuntimeFn {
        name: "weir_str_cmp",
        params: &[ValType::I32, ValType::I32],
        results: &[ValType::I64],
        extra_locals: &[ValType::I32, ValType::I32, ValType::I32],
        emitter: emit_str_cmp,
    },
    RuntimeFn {
        name: "weir_substring",
        params: &[ValType::I32, ValType::I64, ValType::I64],
        results: &[ValType::I32],
        extra_locals: &[ValType::I32, ValType::I32, ValType::I32],
        emitter: emit_substring,
    },
    RuntimeFn {
        name: "weir_string_ref",
        params: &[ValType::I32, ValType::I64],
        results: &[ValType::I64],
        extra_locals: &[],
        emitter: emit_string_ref,
    },
    RuntimeFn {
        name: "weir_string_contains",
        params: &[ValType::I32, ValType::I32],
        results: &[ValType::I64],
        extra_locals: &[ValType::I32, ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        emitter: emit_string_contains,
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

    // TCO (tail call optimization) state
    current_fn_name: Option<SmolStr>,
    tail_positions: HashSet<ExprId>,
    tco_block_depth: u32,

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
            current_fn_name: None,
            tail_positions: HashSet::new(),
            tco_block_depth: 0,
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
    /// All fields are stored in 8-byte slots (matching the constructor's `i * 8` layout).
    fn get_field_offset(&self, struct_ty: &Ty, field_name: &SmolStr) -> Option<u32> {
        let struct_name = match struct_ty {
            Ty::Con(name, _) => name,
            _ => return None,
        };
        let info = self.type_info.struct_defs.get(struct_name)?;
        for (i, (fname, _fty)) in info.fields.iter().enumerate() {
            if fname == field_name {
                return Some((i * 8) as u32);
            }
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

                // Detect TCO: check if this function has self-tail-calls
                let tail_positions = tco::mark_tail_positions(&d.body, &self.ast_module.exprs);
                let needs_tco = tco::has_self_tail_calls(&d.body, &d.name, &self.ast_module.exprs);

                // Helper: set up parameter locals and TCO state
                let setup = |compiler: &mut Self| {
                    compiler.reset_locals();
                    for (i, param) in d.params.iter().enumerate() {
                        compiler.locals.insert(param.name.clone(), i as u32);
                    }
                    compiler.next_local = param_count;
                    if needs_tco {
                        compiler.current_fn_name = Some(d.name.clone());
                        compiler.tail_positions = tail_positions.clone();
                        compiler.tco_block_depth = 0;
                    } else {
                        compiler.current_fn_name = None;
                        compiler.tail_positions.clear();
                        compiler.tco_block_depth = 0;
                    }
                };

                // Pass 1: discover locals
                setup(&mut self);
                let mut dummy = wasm_encoder::Function::new(vec![]);
                self.compile_body(&body, &mut dummy);

                // Capture the extra locals discovered
                // Note: local_types does NOT include params (they're implicit),
                // so we use all entries — each one corresponds to a local beyond
                // the param_count offset.
                let extra_locals: Vec<(u32, ValType)> = self.local_types
                    .iter()
                    .map(|vt| (1, *vt))
                    .collect();

                // Pass 2: compile for real with locals declared
                setup(&mut self);
                let mut func = wasm_encoder::Function::new(extra_locals);

                if needs_tco {
                    // Wrap body in a loop for tail-call optimization
                    let block_ty = match expected_result {
                        Some(vt) => wasm_encoder::BlockType::Result(vt),
                        None => wasm_encoder::BlockType::Empty,
                    };
                    func.instruction(&wasm_encoder::Instruction::Loop(block_ty));
                    self.tco_block_depth = 0;
                    let result_wt = self.compile_body(&body, &mut func);

                    // If the body fell through (no tail jump on all paths),
                    // provide a result for the loop block
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
                    func.instruction(&wasm_encoder::Instruction::End); // end loop
                } else {
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
                }
                func.instruction(&wasm_encoder::Instruction::End); // end function

                // Clear TCO state
                self.current_fn_name = None;
                self.tail_positions.clear();

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
                // Dump function bodies with byte ranges to identify the error location
                let mut fn_idx = 0;
                for payload in wasmparser::Parser::new(0).parse_all(&wasm_bytes) {
                    if let Ok(wasmparser::Payload::CodeSectionEntry(body)) = payload {
                        let range = body.range();
                        let reader = body.get_operators_reader().unwrap();
                        let ops: Vec<_> = reader.into_iter().collect();
                        if e.offset() >= range.start && e.offset() <= range.end {
                            eprintln!("--- func {} ({} ops, bytes {:#x}..{:#x}) ** ERROR HERE ** ---",
                                fn_idx, ops.len(), range.start, range.end);
                            for (i, op) in ops.iter().enumerate() {
                                if let Ok(op) = op {
                                    eprintln!("  [{:3}] {:?}", i, op);
                                }
                            }
                        }
                        fn_idx += 1;
                    }
                }
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
    fn struct_construction_validates() {
        // Start simple: single-field struct, inline construction
        compile_and_validate(r#"
            (defstruct Point (x : i64))
            (defn main () : Unit
              (let ((p (Point 42)))
                (println (.x p))))
        "#);
    }

    #[test]
    fn struct_two_fields_no_access_validates() {
        compile_and_validate(r#"
            (defstruct Point (x : i64) (y : i64))
            (defn main () : Unit
              (let ((p (Point 10 20)))
                (println 0)))
        "#);
    }

    #[test]
    fn struct_two_fields_with_access_validates() {
        compile_and_validate(r#"
            (defstruct Point (x : i64) (y : i64))
            (defn main () : Unit
              (let ((p (Point 10 20)))
                (println (.x p))))
        "#);
    }

    #[test]
    fn struct_two_fields_with_fn_validates() {
        compile_and_validate(r#"
            (defstruct Point (x : i64) (y : i64))
            (defn make-point ((x : i64) (y : i64)) : Point
              (Point x y))
            (defn main () : Unit
              (let ((p (make-point 10 20)))
                (println 0)))
        "#);
    }

    #[test]
    fn struct_with_many_fields_validates() {
        compile_and_validate(r#"
            (defstruct GameState
              (score : i64)
              (level : i64)
              (lines : i64)
              (game-over : i64))
            (defn make-state () : GameState
              (GameState 0 1 0 0))
            (defn add-score ((state : GameState) (pts : i64)) : GameState
              (GameState
                (+ (.score state) pts)
                (.level state)
                (.lines state)
                (.game-over state)))
            (defn main () : Unit
              (let ((s (make-state))
                    (s2 (add-score s 100)))
                (println (.score s2))))
        "#);
    }

    #[test]
    fn vector_builtins_validate() {
        compile_and_validate(r#"
            (defn main () : Unit
              (let ((v [10 20 30 40 50])
                    (x (nth v 2))
                    (v2 (set-nth v 1 99))
                    (v3 (append v 60))
                    (len (length v)))
                (println x)
                (println len)))
        "#);
    }

    #[test]
    fn math_builtins_validate() {
        compile_and_validate(r#"
            (defn main () : Unit
              (let ((a (mod 17 5))
                    (b (min 3 7))
                    (c (max 3 7))
                    (d (random-int 10)))
                (random-seed 42)
                (println a)
                (println b)
                (println c)
                (println d)))
        "#);
    }

    #[test]
    fn str_multi_arg_validates() {
        compile_and_validate(r#"
            (defn main () : Unit
              (let ((s (str "Score: " 42))
                    (s2 (str "" 100))
                    (s3 (str "Level " 5 " Lines " 20)))
                (println s)
                (println s2)
                (println s3)))
        "#);
    }

    #[test]
    fn mini_tetris_validates() {
        // A mini version of Tetris patterns to validate struct + vector + builtins together
        compile_and_validate(r#"
            (defstruct GameState
              (board : (Vector i64))
              (piece : i64)
              (score : i64)
              (level : i64)
              (lines : i64)
              (game-over : i64))

            (defn make-row ((v : (Vector i64)) (i : i64) (n : i64)) : (Vector i64)
              (if (= i n) v (make-row (append v 0) (+ i 1) n)))

            (defn make-empty-board () : (Vector i64) (make-row [] 0 100))

            (defn board-get ((board : (Vector i64)) (row : i64) (col : i64)) : i64
              (nth board (+ (* row 10) col)))

            (defn board-set ((board : (Vector i64)) (row : i64) (col : i64) (val : i64)) : (Vector i64)
              (set-nth board (+ (* row 10) col) val))

            (defn drop-speed ((level : i64)) : i64
              (let ((speed (- 500 (* (min level 10) 40)))) (max speed 100)))

            (defn main () : Unit
              (random-seed 42)
              (let ((board (make-empty-board))
                    (state (GameState board (random-int 7) 0 1 0 0))
                    (b2 (board-set (.board state) 0 0 1))
                    (val (board-get b2 0 0))
                    (speed (drop-speed (.level state))))
                (println (str "Score: " (.score state)))
                (println (str "Value: " val))
                (println (str "Speed: " speed))))
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

    #[test]
    fn tco_simple_countdown_validates() {
        // Simple tail-recursive countdown
        compile_and_validate(r#"
            (defn countdown ((n : i64)) : i64
              (if (= n 0) 0 (countdown (- n 1))))
            (defn main () : Unit
              (println (countdown 100)))
        "#);
    }

    #[test]
    fn tco_accumulator_validates() {
        // Tail-recursive sum with accumulator
        compile_and_validate(r#"
            (defn sum-to ((n : i64) (acc : i64)) : i64
              (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
            (defn main () : Unit
              (println (sum-to 100 0)))
        "#);
    }

    #[test]
    fn tco_nested_if_validates() {
        // TCO with nested if (like Tetris's row-full pattern)
        compile_and_validate(r#"
            (defn row-full ((board : (Vector i64)) (col : i64)) : i64
              (if (= col 10)
                1
                (if (= (nth board col) 0) 0 (row-full board (+ col 1)))))
            (defn main () : Unit
              (let ((v [1 2 3 4 5 6 7 8 9 10]))
                (println (row-full v 0))))
        "#);
    }

    #[test]
    fn tco_with_let_validates() {
        // TCO inside a let body
        compile_and_validate(r#"
            (defn make-row ((v : (Vector i64)) (i : i64) (n : i64)) : (Vector i64)
              (if (= i n) v (make-row (append v 0) (+ i 1) n)))
            (defn main () : Unit
              (let ((row (make-row [] 0 10)))
                (println (nth row 0))))
        "#);
    }

    #[test]
    fn tetris_full_validates() {
        // End-to-end: compile the full Tetris demo and validate the WASM binary
        let prelude = include_str!("../../../std/prelude.weir");
        let opengl_lib = include_str!("/home/nathan/dev/weir-opengl/lib.weir");
        let tetris_src = include_str!("../../../demos/tetris/tetris.weir");
        let full = format!("{}\n{}\n{}", prelude, opengl_lib, tetris_src);

        let expanded = weir_macros::expand_for_target(&full, Some(weir_ast::CompileTarget::Wasm));
        assert!(expanded.errors.is_empty(), "expand errors: {:?}", expanded.errors);
        let (module, parse_errors) = weir_parser::parse(&expanded.source);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);

        let type_info = weir_typeck::check(&module);
        // Allow type errors from import resolution (we just want WASM validation)
        let wasm_bytes = compile_to_wasm(&module, &type_info).expect("WASM compilation failed");

        let mut validator = wasmparser::Validator::new();
        if let Err(e) = validator.validate_all(&wasm_bytes) {
            // Dump error location
            let mut fn_idx = 0;
            for payload in wasmparser::Parser::new(0).parse_all(&wasm_bytes) {
                if let Ok(wasmparser::Payload::CodeSectionEntry(body)) = payload {
                    let range = body.range();
                    if e.offset() >= range.start && e.offset() <= range.end {
                        let reader = body.get_operators_reader().unwrap();
                        let ops: Vec<_> = reader.into_iter().collect();
                        eprintln!("--- func {} ({} ops, bytes {:#x}..{:#x}) ** ERROR ** ---",
                            fn_idx, ops.len(), range.start, range.end);
                        for (i, op) in ops.iter().enumerate() {
                            if let Ok(op) = op {
                                eprintln!("  [{:3}] {:?}", i, op);
                            }
                        }
                    }
                    fn_idx += 1;
                }
            }
            panic!("Tetris WASM validation failed: {} (offset: {:?})", e, e.offset());
        }

        // Basic sanity checks
        assert!(wasm_bytes.len() > 5000, "WASM binary too small: {} bytes", wasm_bytes.len());
        assert_eq!(&wasm_bytes[..4], b"\x00asm", "Missing WASM magic header");
    }
}
