use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{AbiParam, BlockArg, Function, InstBuilder, Type, Value};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use smol_str::SmolStr;
use std::collections::HashMap;
use std::fmt;
use weir_ast::*;
use weir_typeck::{Ty, TypeCheckResult};

// ── Error ────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
}

impl CodegenError {
    fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "codegen error: {}", self.message)
    }
}

impl std::error::Error for CodegenError {}

// ── Runtime print helpers ────────────────────────────────────────
//
// These are extern "C" functions that compiled code calls to print.
// They write to a thread-local String buffer for testability.

std::thread_local! {
    static OUTPUT_BUF: std::cell::RefCell<String> = const { std::cell::RefCell::new(String::new()) };
}

fn output_reset() {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().clear());
}

fn output_take() -> String {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().split_off(0))
}

extern "C" fn weir_print_i64(val: i64) {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        write!(buf.borrow_mut(), "{}", val).unwrap();
    });
}

extern "C" fn weir_print_f64(val: f64) {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        if val.fract() == 0.0 {
            write!(buf.borrow_mut(), "{:.1}", val).unwrap();
        } else {
            write!(buf.borrow_mut(), "{}", val).unwrap();
        }
    });
}

extern "C" fn weir_print_bool(val: i8) {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        write!(buf.borrow_mut(), "{}", if val != 0 { "true" } else { "false" }).unwrap();
    });
}

extern "C" fn weir_print_unit() {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        write!(buf.borrow_mut(), "()").unwrap();
    });
}

extern "C" fn weir_print_newline() {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().push('\n'));
}

// ── Type mapping ─────────────────────────────────────────────────

fn ty_to_cl(ty: &Ty) -> Option<Type> {
    match ty {
        Ty::I8 | Ty::U8 | Ty::Bool => Some(types::I8),
        Ty::I16 | Ty::U16 => Some(types::I16),
        Ty::I32 | Ty::U32 => Some(types::I32),
        Ty::I64 | Ty::U64 => Some(types::I64),
        Ty::F32 => Some(types::F32),
        Ty::F64 => Some(types::F64),
        Ty::Unit => None, // void — no value
        _ => None,
    }
}

fn is_float(ty: &Ty) -> bool {
    matches!(ty, Ty::F32 | Ty::F64)
}

fn is_signed(ty: &Ty) -> bool {
    matches!(ty, Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64)
}

// ── Compiler ─────────────────────────────────────────────────────

pub fn compile_and_run(
    module: &weir_ast::Module,
    type_info: &TypeCheckResult,
) -> Result<String, CodegenError> {
    let mut compiler = Compiler::new(module, type_info)?;
    compiler.compile()?;
    compiler.run_main()
}

struct Compiler<'a> {
    ast_module: &'a weir_ast::Module,
    type_info: &'a TypeCheckResult,
    jit_module: JITModule,

    /// Maps user function name → (FuncId, param types, return type)
    user_fns: HashMap<SmolStr, (FuncId, Vec<Ty>, Ty)>,
    /// Maps runtime helper name → FuncId
    runtime_fns: HashMap<&'static str, FuncId>,
}

impl<'a> Compiler<'a> {
    fn new(
        ast_module: &'a weir_ast::Module,
        type_info: &'a TypeCheckResult,
    ) -> Result<Self, CodegenError> {
        let mut flag_builder = settings::builder();
        flag_builder
            .set("opt_level", "speed")
            .map_err(|e| CodegenError::new(format!("setting opt_level: {}", e)))?;

        let isa_builder = cranelift_native::builder()
            .map_err(|e| CodegenError::new(format!("native ISA: {}", e)))?;

        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| CodegenError::new(format!("ISA finish: {}", e)))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // Register runtime print helpers
        builder.symbol("weir_print_i64", weir_print_i64 as *const u8);
        builder.symbol("weir_print_f64", weir_print_f64 as *const u8);
        builder.symbol("weir_print_bool", weir_print_bool as *const u8);
        builder.symbol("weir_print_unit", weir_print_unit as *const u8);
        builder.symbol("weir_print_newline", weir_print_newline as *const u8);

        let jit_module = JITModule::new(builder);

        Ok(Self {
            ast_module,
            type_info,
            jit_module,
            user_fns: HashMap::new(),
            runtime_fns: HashMap::new(),
        })
    }

    fn compile(&mut self) -> Result<(), CodegenError> {
        self.declare_runtime_helpers()?;
        self.declare_user_functions()?;
        self.compile_user_functions()?;
        self.jit_module.finalize_definitions()
            .map_err(|e| CodegenError::new(format!("finalize: {}", e)))?;
        Ok(())
    }

    // ── Runtime helper declarations ─────────────────────────────

    fn declare_runtime_helpers(&mut self) -> Result<(), CodegenError> {
        let ptr_ty = self.jit_module.target_config().pointer_type();
        let _ = ptr_ty;

        // weir_print_i64(i64) -> void
        let mut sig = self.jit_module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self.jit_module
            .declare_function("weir_print_i64", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_i64: {}", e)))?;
        self.runtime_fns.insert("print_i64", id);

        // weir_print_f64(f64) -> void
        let mut sig = self.jit_module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
        sig.call_conv = CallConv::SystemV;
        let id = self.jit_module
            .declare_function("weir_print_f64", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_f64: {}", e)))?;
        self.runtime_fns.insert("print_f64", id);

        // weir_print_bool(i8) -> void
        let mut sig = self.jit_module.make_signature();
        sig.params.push(AbiParam::new(types::I8));
        sig.call_conv = CallConv::SystemV;
        let id = self.jit_module
            .declare_function("weir_print_bool", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_bool: {}", e)))?;
        self.runtime_fns.insert("print_bool", id);

        // weir_print_unit() -> void
        let mut sig = self.jit_module.make_signature();
        sig.call_conv = CallConv::SystemV;
        let id = self.jit_module
            .declare_function("weir_print_unit", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_unit: {}", e)))?;
        self.runtime_fns.insert("print_unit", id);

        // weir_print_newline() -> void
        let mut sig = self.jit_module.make_signature();
        sig.call_conv = CallConv::SystemV;
        let id = self.jit_module
            .declare_function("weir_print_newline", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_newline: {}", e)))?;
        self.runtime_fns.insert("print_newline", id);

        Ok(())
    }

    // ── Pass 1: declare all user function signatures ────────────

    fn declare_user_functions(&mut self) -> Result<(), CodegenError> {
        for (item, _span) in &self.ast_module.items {
            if let Item::Defn(defn) = item {
                let (param_tys, ret_ty) = self.resolve_fn_signature(defn)?;

                let mut sig = self.jit_module.make_signature();
                sig.call_conv = CallConv::SystemV;
                for pty in &param_tys {
                    if let Some(cl_ty) = ty_to_cl(pty) {
                        sig.params.push(AbiParam::new(cl_ty));
                    }
                }
                if let Some(cl_ty) = ty_to_cl(&ret_ty) {
                    sig.returns.push(AbiParam::new(cl_ty));
                }

                let func_id = self.jit_module
                    .declare_function(&defn.name, Linkage::Local, &sig)
                    .map_err(|e| CodegenError::new(format!("declare fn '{}': {}", defn.name, e)))?;

                self.user_fns
                    .insert(defn.name.clone(), (func_id, param_tys, ret_ty));
            }
        }
        Ok(())
    }

    /// Resolve function parameter types and return type from the type checker results.
    fn resolve_fn_signature(&self, defn: &Defn) -> Result<(Vec<Ty>, Ty), CodegenError> {
        let mut param_tys = Vec::new();
        for param in &defn.params {
            if let Some(type_ann_id) = param.type_ann {
                // Look through the body expressions to find a typed usage,
                // but we can resolve from the type_info expr_types map.
                // The simplest approach: if the function has type annotations,
                // resolve from the AST directly. For the codegen subset,
                // all functions must have explicit type annotations.
                let ty = self.resolve_type_expr_to_ty(type_ann_id)?;
                param_tys.push(ty);
            } else {
                return Err(CodegenError::new(format!(
                    "function '{}' parameter '{}' requires a type annotation for codegen",
                    defn.name, param.name
                )));
            }
        }

        let ret_ty = if let Some(ret_id) = defn.return_type {
            self.resolve_type_expr_to_ty(ret_id)?
        } else {
            // Default to Unit if no return type annotation
            // (but check the body type from type_info if available)
            if !defn.body.is_empty() {
                let last_expr_id = defn.body[defn.body.len() - 1];
                if let Some(ty) = self.type_info.expr_types.get(last_expr_id) {
                    ty.clone()
                } else {
                    Ty::Unit
                }
            } else {
                Ty::Unit
            }
        };

        Ok((param_tys, ret_ty))
    }

    fn resolve_type_expr_to_ty(&self, id: TypeExprId) -> Result<Ty, CodegenError> {
        let texpr = &self.ast_module.type_exprs[id];
        match &texpr.kind {
            TypeExprKind::Named(name) => match name.as_str() {
                "i8" => Ok(Ty::I8),
                "i16" => Ok(Ty::I16),
                "i32" => Ok(Ty::I32),
                "i64" => Ok(Ty::I64),
                "u8" => Ok(Ty::U8),
                "u16" => Ok(Ty::U16),
                "u32" => Ok(Ty::U32),
                "u64" => Ok(Ty::U64),
                "f32" => Ok(Ty::F32),
                "f64" => Ok(Ty::F64),
                "Bool" | "bool" => Ok(Ty::Bool),
                "Unit" => Ok(Ty::Unit),
                other => Err(CodegenError::new(format!(
                    "unsupported type '{}' for codegen",
                    other
                ))),
            },
            _ => Err(CodegenError::new(
                "complex type expressions not yet supported in codegen",
            )),
        }
    }

    // ── Pass 2: compile each function body ──────────────────────

    fn compile_user_functions(&mut self) -> Result<(), CodegenError> {
        let defns: Vec<Defn> = self
            .ast_module
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Defn(d) = item {
                    Some(d.clone())
                } else {
                    None
                }
            })
            .collect();

        for defn in &defns {
            self.compile_function(defn)?;
        }
        Ok(())
    }

    fn compile_function(&mut self, defn: &Defn) -> Result<(), CodegenError> {
        let (func_id, param_tys, ret_ty) = self
            .user_fns
            .get(&defn.name)
            .ok_or_else(|| CodegenError::new(format!("unknown function '{}'", defn.name)))?
            .clone();

        let mut sig = self.jit_module.make_signature();
        sig.call_conv = CallConv::SystemV;
        for pty in &param_tys {
            if let Some(cl_ty) = ty_to_cl(pty) {
                sig.params.push(AbiParam::new(cl_ty));
            }
        }
        if let Some(cl_ty) = ty_to_cl(&ret_ty) {
            sig.returns.push(AbiParam::new(cl_ty));
        }

        let mut func = Function::with_name_signature(
            cranelift_codegen::ir::UserFuncName::default(),
            sig,
        );

        let mut fb_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut fb_ctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        {
            let mut fn_ctx = FnCompileCtx {
                builder: &mut builder,
                ast_module: self.ast_module,
                type_info: self.type_info,
                user_fns: &self.user_fns,
                runtime_fns: &self.runtime_fns,
                jit_module: &mut self.jit_module,
                vars: HashMap::new(),
            };

            // Bind parameters to variables
            let block_params: Vec<Value> = fn_ctx.builder.block_params(entry_block).to_vec();
            let mut param_idx = 0;
            for (i, param) in defn.params.iter().enumerate() {
                if let Some(cl_ty) = ty_to_cl(&param_tys[i]) {
                    let var = fn_ctx.declare_variable(cl_ty);
                    fn_ctx.builder.def_var(var, block_params[param_idx]);
                    fn_ctx.vars.insert(param.name.clone(), (var, param_tys[i].clone()));
                    param_idx += 1;
                }
            }

            // Compile body
            let body_val = fn_ctx.compile_body(&defn.body, &ret_ty)?;

            // Return
            if ty_to_cl(&ret_ty).is_some() {
                if let Some(val) = body_val {
                    fn_ctx.builder.ins().return_(&[val]);
                } else {
                    let cl_ty = ty_to_cl(&ret_ty).unwrap();
                    let zero = if cl_ty.is_float() {
                        fn_ctx.builder.ins().f64const(0.0)
                    } else {
                        fn_ctx.builder.ins().iconst(cl_ty, 0)
                    };
                    fn_ctx.builder.ins().return_(&[zero]);
                }
            } else {
                fn_ctx.builder.ins().return_(&[]);
            }
        } // fn_ctx dropped here, releasing borrows

        builder.finalize();

        let mut ctx = Context::for_function(func);
        self.jit_module
            .define_function(func_id, &mut ctx)
            .map_err(|e| CodegenError::new(format!("define fn '{}': {}", defn.name, e)))?;

        Ok(())
    }

    fn run_main(&self) -> Result<String, CodegenError> {
        let (func_id, _, _) = self
            .user_fns
            .get("main")
            .ok_or_else(|| CodegenError::new("no 'main' function defined"))?;

        let code_ptr = self.jit_module.get_finalized_function(*func_id);

        output_reset();

        unsafe {
            let main_fn: fn() = std::mem::transmute(code_ptr);
            main_fn();
        }

        Ok(output_take())
    }
}

// ── Per-function compilation context ─────────────────────────────

struct FnCompileCtx<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    ast_module: &'a weir_ast::Module,
    type_info: &'a TypeCheckResult,
    user_fns: &'a HashMap<SmolStr, (FuncId, Vec<Ty>, Ty)>,
    runtime_fns: &'a HashMap<&'static str, FuncId>,
    jit_module: &'a mut JITModule,
    vars: HashMap<SmolStr, (Variable, Ty)>,
}

impl<'a, 'b> FnCompileCtx<'a, 'b> {
    fn declare_variable(&mut self, cl_ty: Type) -> Variable {
        self.builder.declare_var(cl_ty)
    }

    fn expr_ty(&self, expr_id: ExprId) -> Ty {
        self.type_info
            .expr_types
            .get(expr_id)
            .cloned()
            .unwrap_or(Ty::Unit)
    }

    /// Compile a body (sequence of expressions), returning the value of the last one.
    fn compile_body(
        &mut self,
        body: &[ExprId],
        expected_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        if body.is_empty() {
            return Ok(None);
        }
        let mut val = None;
        for (i, &expr_id) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            let ty = if is_last {
                expected_ty.clone()
            } else {
                self.expr_ty(expr_id)
            };
            val = self.compile_expr(expr_id, &ty)?;
        }
        Ok(val)
    }

    /// Compile an expression, returning its Cranelift Value (or None for Unit).
    fn compile_expr(
        &mut self,
        expr_id: ExprId,
        _hint_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let expr = &self.ast_module.exprs[expr_id];
        let resolved_ty = self.expr_ty(expr_id);

        match &expr.kind {
            ExprKind::Lit(lit) => self.compile_lit(lit, &resolved_ty),

            ExprKind::Var(name) => {
                if let Some((var, _ty)) = self.vars.get(name) {
                    Ok(Some(self.builder.use_var(*var)))
                } else {
                    Err(CodegenError::new(format!(
                        "undefined variable '{}' in codegen",
                        name
                    )))
                }
            }

            ExprKind::Call { func, args } => self.compile_call(*func, args, &resolved_ty),

            ExprKind::Let { bindings, body } => {
                for b in bindings {
                    let val_ty = self.expr_ty(b.value);
                    let val = self.compile_expr(b.value, &val_ty)?;
                    if let Some(cl_ty) = ty_to_cl(&val_ty) {
                        let var = self.declare_variable(cl_ty);
                        if let Some(v) = val {
                            self.builder.def_var(var, v);
                        }
                        self.vars.insert(b.name.clone(), (var, val_ty));
                    }
                }
                self.compile_body(body, &resolved_ty)
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.compile_if(*condition, *then_branch, *else_branch, &resolved_ty),

            ExprKind::Cond {
                clauses,
                else_clause,
            } => self.compile_cond(clauses, *else_clause, &resolved_ty),

            ExprKind::When { condition, body } => {
                self.compile_when_unless(*condition, body, false)?;
                Ok(None)
            }

            ExprKind::Unless { condition, body } => {
                self.compile_when_unless(*condition, body, true)?;
                Ok(None)
            }

            ExprKind::Do { body } => self.compile_body(body, &resolved_ty),

            ExprKind::SetBang { place, value } => {
                let place_expr = &self.ast_module.exprs[*place];
                if let ExprKind::Var(name) = &place_expr.kind {
                    let name = name.clone();
                    let val_ty = self.expr_ty(*value);
                    let val = self.compile_expr(*value, &val_ty)?;
                    if let Some(v) = val {
                        if let Some((var, _)) = self.vars.get(&name) {
                            let var = *var;
                            self.builder.def_var(var, v);
                        } else {
                            return Err(CodegenError::new(format!(
                                "set! on undefined variable '{}'",
                                name
                            )));
                        }
                    }
                    Ok(None) // set! returns Unit
                } else {
                    Err(CodegenError::new("set! target must be a variable"))
                }
            }

            ExprKind::Ann { expr, .. } => {
                self.compile_expr(*expr, &resolved_ty)
            }

            _ => Err(CodegenError::new(format!(
                "unsupported expression kind in codegen: {:?}",
                std::mem::discriminant(&expr.kind)
            ))),
        }
    }

    // ── Literals ────────────────────────────────────────────────

    fn compile_lit(
        &mut self,
        lit: &Literal,
        ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        match lit {
            Literal::Int(n) => {
                let cl_ty = ty_to_cl(ty)
                    .unwrap_or(types::I64);
                let val = self.builder.ins().iconst(cl_ty, *n);
                Ok(Some(val))
            }
            Literal::Float(n) => match ty {
                Ty::F32 => Ok(Some(self.builder.ins().f32const(*n as f32))),
                _ => Ok(Some(self.builder.ins().f64const(*n))),
            },
            Literal::Bool(b) => {
                let val = self.builder.ins().iconst(types::I8, if *b { 1 } else { 0 });
                Ok(Some(val))
            }
            Literal::String(_) => Err(CodegenError::new(
                "string literals not yet supported in codegen",
            )),
        }
    }

    // ── Function calls / builtins ───────────────────────────────

    fn compile_call(
        &mut self,
        func_id: ExprId,
        args: &[Arg],
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let func_expr = &self.ast_module.exprs[func_id];

        if let ExprKind::Var(name) = &func_expr.kind {
            match name.as_str() {
                "+" | "-" | "*" | "/" | "mod" => {
                    return self.compile_arith(name, args, result_ty);
                }
                "<" | ">" | "<=" | ">=" | "=" | "!=" => {
                    return self.compile_comparison(name, args);
                }
                "not" => {
                    return self.compile_not(args);
                }
                "and" => {
                    return self.compile_and(args);
                }
                "or" => {
                    return self.compile_or(args);
                }
                "println" => {
                    return self.compile_println(args);
                }
                "print" => {
                    return self.compile_print(args);
                }
                _ => {}
            }

            // User function call
            if let Some((callee_id, param_tys, ret_ty)) = self.user_fns.get(name).cloned() {
                let func_ref = self
                    .jit_module
                    .declare_func_in_func(callee_id, self.builder.func);

                let mut arg_vals = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let pty = if i < param_tys.len() {
                        &param_tys[i]
                    } else {
                        return Err(CodegenError::new(format!(
                            "too many arguments for function '{}'",
                            name
                        )));
                    };
                    if let Some(val) = self.compile_expr(arg.value, pty)? {
                        arg_vals.push(val);
                    }
                }

                let call_inst = self.builder.ins().call(func_ref, &arg_vals);
                if ty_to_cl(&ret_ty).is_some() {
                    let results = self.builder.inst_results(call_inst);
                    Ok(Some(results[0]))
                } else {
                    Ok(None)
                }
            } else {
                Err(CodegenError::new(format!(
                    "unknown function '{}' in codegen",
                    name
                )))
            }
        } else {
            Err(CodegenError::new(
                "non-variable function calls not supported in codegen",
            ))
        }
    }

    // ── Arithmetic ──────────────────────────────────────────────

    fn compile_arith(
        &mut self,
        op: &str,
        args: &[Arg],
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        if args.is_empty() {
            return match op {
                "+" => {
                    let cl_ty = ty_to_cl(result_ty).unwrap_or(types::I64);
                    Ok(Some(self.builder.ins().iconst(cl_ty, 0)))
                }
                "*" => {
                    let cl_ty = ty_to_cl(result_ty).unwrap_or(types::I64);
                    Ok(Some(self.builder.ins().iconst(cl_ty, 1)))
                }
                _ => Err(CodegenError::new(format!("{} requires arguments", op))),
            };
        }

        // Unary minus
        if op == "-" && args.len() == 1 {
            let arg_ty = self.expr_ty(args[0].value);
            let val = self.compile_expr(args[0].value, &arg_ty)?.unwrap();
            if is_float(&arg_ty) {
                return Ok(Some(self.builder.ins().fneg(val)));
            } else {
                // ineg = 0 - val
                let cl_ty = ty_to_cl(&arg_ty).unwrap();
                let zero = self.builder.ins().iconst(cl_ty, 0);
                return Ok(Some(self.builder.ins().isub(zero, val)));
            }
        }

        let float = is_float(result_ty);

        // Compile first argument
        let first_ty = self.expr_ty(args[0].value);
        let mut acc = self.compile_expr(args[0].value, &first_ty)?.unwrap();

        // Fold remaining arguments
        for arg in &args[1..] {
            let arg_ty = self.expr_ty(arg.value);
            let val = self.compile_expr(arg.value, &arg_ty)?.unwrap();

            acc = match op {
                "+" if float => self.builder.ins().fadd(acc, val),
                "+" => self.builder.ins().iadd(acc, val),
                "-" if float => self.builder.ins().fsub(acc, val),
                "-" => self.builder.ins().isub(acc, val),
                "*" if float => self.builder.ins().fmul(acc, val),
                "*" => self.builder.ins().imul(acc, val),
                "/" if float => self.builder.ins().fdiv(acc, val),
                "/" if is_signed(result_ty) => self.builder.ins().sdiv(acc, val),
                "/" => self.builder.ins().udiv(acc, val),
                "mod" if float => {
                    // f % g = f - trunc(f/g)*g — but for simplicity, use libc fmod
                    // Actually Cranelift doesn't have frem, so approximate:
                    // a - trunc(a/b)*b
                    let div = self.builder.ins().fdiv(acc, val);
                    let trunc = self.builder.ins().trunc(div);
                    let prod = self.builder.ins().fmul(trunc, val);
                    self.builder.ins().fsub(acc, prod)
                }
                "mod" if is_signed(result_ty) => self.builder.ins().srem(acc, val),
                "mod" => self.builder.ins().urem(acc, val),
                _ => unreachable!(),
            };
        }

        Ok(Some(acc))
    }

    // ── Comparisons ─────────────────────────────────────────────

    fn compile_comparison(
        &mut self,
        op: &str,
        args: &[Arg],
    ) -> Result<Option<Value>, CodegenError> {
        if args.len() != 2 {
            return Err(CodegenError::new(format!(
                "{} requires exactly 2 arguments",
                op
            )));
        }

        let lhs_ty = self.expr_ty(args[0].value);
        let lhs = self.compile_expr(args[0].value, &lhs_ty)?.unwrap();
        let rhs_ty = self.expr_ty(args[1].value);
        let rhs = self.compile_expr(args[1].value, &rhs_ty)?.unwrap();

        let float = is_float(&lhs_ty);

        let result = if float {
            let cc = match op {
                "<" => FloatCC::LessThan,
                ">" => FloatCC::GreaterThan,
                "<=" => FloatCC::LessThanOrEqual,
                ">=" => FloatCC::GreaterThanOrEqual,
                "=" => FloatCC::Equal,
                "!=" => FloatCC::NotEqual,
                _ => unreachable!(),
            };
            self.builder.ins().fcmp(cc, lhs, rhs)
        } else {
            let signed = is_signed(&lhs_ty);
            let cc = match (op, signed) {
                ("<", true) => IntCC::SignedLessThan,
                ("<", false) => IntCC::UnsignedLessThan,
                (">", true) => IntCC::SignedGreaterThan,
                (">", false) => IntCC::UnsignedGreaterThan,
                ("<=", true) => IntCC::SignedLessThanOrEqual,
                ("<=", false) => IntCC::UnsignedLessThanOrEqual,
                (">=", true) => IntCC::SignedGreaterThanOrEqual,
                (">=", false) => IntCC::UnsignedGreaterThanOrEqual,
                ("=", _) => IntCC::Equal,
                ("!=", _) => IntCC::NotEqual,
                _ => unreachable!(),
            };
            self.builder.ins().icmp(cc, lhs, rhs)
        };

        Ok(Some(result))
    }

    // ── Boolean ops ─────────────────────────────────────────────

    fn compile_not(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::new("not requires exactly 1 argument"));
        }
        let val = self.compile_expr(args[0].value, &Ty::Bool)?.unwrap();
        let one = self.builder.ins().iconst(types::I8, 1);
        let result = self.builder.ins().bxor(val, one);
        Ok(Some(result))
    }

    fn compile_and(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.is_empty() {
            return Ok(Some(self.builder.ins().iconst(types::I8, 1)));
        }
        let mut acc = self.compile_expr(args[0].value, &Ty::Bool)?.unwrap();
        for arg in &args[1..] {
            let val = self.compile_expr(arg.value, &Ty::Bool)?.unwrap();
            acc = self.builder.ins().band(acc, val);
        }
        Ok(Some(acc))
    }

    fn compile_or(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.is_empty() {
            return Ok(Some(self.builder.ins().iconst(types::I8, 0)));
        }
        let mut acc = self.compile_expr(args[0].value, &Ty::Bool)?.unwrap();
        for arg in &args[1..] {
            let val = self.compile_expr(arg.value, &Ty::Bool)?.unwrap();
            acc = self.builder.ins().bor(acc, val);
        }
        Ok(Some(acc))
    }

    // ── Print ───────────────────────────────────────────────────

    fn compile_println(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.len() > 1 {
            return Err(CodegenError::new("println expects 0 or 1 arguments"));
        }

        if args.len() == 1 {
            self.compile_print(args)?;
        }

        // Print newline
        let newline_id = self.runtime_fns["print_newline"];
        let newline_ref = self
            .jit_module
            .declare_func_in_func(newline_id, self.builder.func);
        self.builder.ins().call(newline_ref, &[]);

        Ok(None) // println returns Unit
    }

    fn compile_print(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::new("print expects exactly 1 argument"));
        }

        let arg_ty = self.expr_ty(args[0].value);
        let val = self.compile_expr(args[0].value, &arg_ty)?;

        let (helper_name, need_extend) = match &arg_ty {
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 |
            Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 => ("print_i64", true),
            Ty::F32 => ("print_f64", true),
            Ty::F64 => ("print_f64", false),
            Ty::Bool => ("print_bool", false),
            Ty::Unit => ("print_unit", false),
            _ => return Err(CodegenError::new(format!(
                "cannot print type {} in codegen",
                arg_ty
            ))),
        };

        let func_id = self.runtime_fns[helper_name];
        let func_ref = self.jit_module.declare_func_in_func(func_id, self.builder.func);

        match &arg_ty {
            Ty::Unit => {
                self.builder.ins().call(func_ref, &[]);
            }
            _ => {
                let val = val.unwrap();
                let arg_val = if need_extend {
                    match &arg_ty {
                        Ty::I8 | Ty::I16 | Ty::I32 => {
                            self.builder.ins().sextend(types::I64, val)
                        }
                        Ty::U8 | Ty::U16 | Ty::U32 => {
                            self.builder.ins().uextend(types::I64, val)
                        }
                        Ty::F32 => {
                            self.builder.ins().fpromote(types::F64, val)
                        }
                        _ => val, // I64, U64 already right size
                    }
                } else {
                    val
                };
                self.builder.ins().call(func_ref, &[arg_val]);
            }
        }

        Ok(None)
    }

    // ── If/else ─────────────────────────────────────────────────

    fn compile_if(
        &mut self,
        condition: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let cond_val = self.compile_expr(condition, &Ty::Bool)?.unwrap();

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // Add block param for the result value if the if-expr produces a value
        let has_value = ty_to_cl(result_ty).is_some() && else_branch.is_some();
        if has_value {
            let cl_ty = ty_to_cl(result_ty).unwrap();
            self.builder.append_block_param(merge_block, cl_ty);
        }

        self.builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

        // Then branch
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_val = self.compile_expr(then_branch, result_ty)?;
        if has_value {
            let val = then_val.unwrap_or_else(|| self.zero_value(result_ty));
            self.builder.ins().jump(merge_block, &[BlockArg::Value(val)]);
        } else {
            self.builder.ins().jump(merge_block, &[]);
        }

        // Else branch
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        if let Some(else_id) = else_branch {
            let else_val = self.compile_expr(else_id, result_ty)?;
            if has_value {
                let val = else_val.unwrap_or_else(|| self.zero_value(result_ty));
                self.builder.ins().jump(merge_block, &[BlockArg::Value(val)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }
        } else {
            // No else branch — Unit result
            self.builder.ins().jump(merge_block, &[]);
        }

        // Merge
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        if has_value {
            let result = self.builder.block_params(merge_block)[0];
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    // ── Cond ────────────────────────────────────────────────────

    fn compile_cond(
        &mut self,
        clauses: &[(ExprId, ExprId)],
        else_clause: Option<ExprId>,
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let has_value = ty_to_cl(result_ty).is_some();
        let merge_block = self.builder.create_block();
        if has_value {
            let cl_ty = ty_to_cl(result_ty).unwrap();
            self.builder.append_block_param(merge_block, cl_ty);
        }

        for (test_id, body_id) in clauses {
            let then_block = self.builder.create_block();
            let next_block = self.builder.create_block();

            let cond_val = self.compile_expr(*test_id, &Ty::Bool)?.unwrap();
            self.builder.ins().brif(cond_val, then_block, &[], next_block, &[]);

            self.builder.switch_to_block(then_block);
            self.builder.seal_block(then_block);
            let body_val = self.compile_expr(*body_id, result_ty)?;
            if has_value {
                let val = body_val.unwrap_or_else(|| self.zero_value(result_ty));
                self.builder.ins().jump(merge_block, &[BlockArg::Value(val)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }

            self.builder.switch_to_block(next_block);
            self.builder.seal_block(next_block);
        }

        // Else clause (or default)
        if let Some(else_id) = else_clause {
            let else_val = self.compile_expr(else_id, result_ty)?;
            if has_value {
                let val = else_val.unwrap_or_else(|| self.zero_value(result_ty));
                self.builder.ins().jump(merge_block, &[BlockArg::Value(val)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }
        } else {
            // No else — jump to merge with default
            if has_value {
                let zero = self.zero_value(result_ty);
                self.builder.ins().jump(merge_block, &[BlockArg::Value(zero)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        if has_value {
            let result = self.builder.block_params(merge_block)[0];
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    // ── When / Unless ───────────────────────────────────────────

    fn compile_when_unless(
        &mut self,
        condition: ExprId,
        body: &[ExprId],
        invert: bool,
    ) -> Result<(), CodegenError> {
        let cond_val = self.compile_expr(condition, &Ty::Bool)?.unwrap();

        let body_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        if invert {
            // unless: run body when condition is false
            self.builder.ins().brif(cond_val, merge_block, &[], body_block, &[]);
        } else {
            // when: run body when condition is true
            self.builder.ins().brif(cond_val, body_block, &[], merge_block, &[]);
        }

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);
        self.compile_body(body, &Ty::Unit)?;
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        Ok(())
    }

    // ── Helpers ─────────────────────────────────────────────────

    fn zero_value(&mut self, ty: &Ty) -> Value {
        match ty {
            Ty::F32 => self.builder.ins().f32const(0.0),
            Ty::F64 => self.builder.ins().f64const(0.0),
            _ => {
                let cl_ty = ty_to_cl(ty).unwrap_or(types::I64);
                self.builder.ins().iconst(cl_ty, 0)
            }
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn compile_run(source: &str) -> String {
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(
            parse_errors.is_empty(),
            "parse errors: {:?}",
            parse_errors
        );
        let type_info = weir_typeck::check(&module);
        assert!(
            type_info.errors.is_empty(),
            "type errors: {:?}",
            type_info.errors
        );
        compile_and_run(&module, &type_info).expect("codegen error")
    }

    fn interp_run(source: &str) -> String {
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(
            parse_errors.is_empty(),
            "parse errors: {:?}",
            parse_errors
        );
        weir_interp::interpret(&module).expect("interpreter error")
    }

    /// Run through both interpreter and compiler, assert same output.
    fn oracle_test(source: &str) {
        let compiled = compile_run(source);
        let interpreted = interp_run(source);
        assert_eq!(
            compiled, interpreted,
            "\n--- COMPILED ---\n{}\n--- INTERPRETED ---\n{}\n--- SOURCE ---\n{}",
            compiled, interpreted, source
        );
    }

    // ── Basic arithmetic ────────────────────────────────────────

    #[test]
    fn test_integer_add() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ 1 2)))",
        );
        assert_eq!(out, "3\n");
    }

    #[test]
    fn test_integer_sub() {
        let out = compile_run(
            "(defn main () : Unit
               (println (- 10 3)))",
        );
        assert_eq!(out, "7\n");
    }

    #[test]
    fn test_integer_mul() {
        let out = compile_run(
            "(defn main () : Unit
               (println (* 4 5)))",
        );
        assert_eq!(out, "20\n");
    }

    #[test]
    fn test_integer_div() {
        let out = compile_run(
            "(defn main () : Unit
               (println (/ 15 4)))",
        );
        assert_eq!(out, "3\n");
    }

    #[test]
    fn test_integer_mod() {
        let out = compile_run(
            "(defn main () : Unit
               (println (mod 17 5)))",
        );
        assert_eq!(out, "2\n");
    }

    #[test]
    fn test_variadic_add() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ 1 2 3 4 5)))",
        );
        assert_eq!(out, "15\n");
    }

    #[test]
    fn test_nested_arithmetic() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ (* 2 3) (- 10 4))))",
        );
        assert_eq!(out, "12\n");
    }

    #[test]
    fn test_negation() {
        let out = compile_run(
            "(defn main () : Unit
               (println (- 5)))",
        );
        assert_eq!(out, "-5\n");
    }

    #[test]
    fn test_float_arithmetic() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ 1.5 2.5)))",
        );
        assert_eq!(out, "4.0\n");
    }

    // ── Comparisons ─────────────────────────────────────────────

    #[test]
    fn test_comparisons() {
        let out = compile_run(
            "(defn main () : Unit
               (println (< 1 2))
               (println (> 1 2))
               (println (= 3 3))
               (println (!= 3 4))
               (println (<= 5 5))
               (println (>= 5 6)))",
        );
        assert_eq!(out, "true\nfalse\ntrue\ntrue\ntrue\nfalse\n");
    }

    // ── Boolean ops ─────────────────────────────────────────────

    #[test]
    fn test_boolean_ops() {
        let out = compile_run(
            "(defn main () : Unit
               (println (not true))
               (println (not false))
               (println (and true true))
               (println (and true false))
               (println (or false false))
               (println (or false true)))",
        );
        assert_eq!(out, "false\ntrue\ntrue\nfalse\nfalse\ntrue\n");
    }

    // ── If/else ─────────────────────────────────────────────────

    #[test]
    fn test_if_else() {
        let out = compile_run(
            "(defn main () : Unit
               (println (if (> 5 3) 1 0))
               (println (if (< 5 3) 1 0)))",
        );
        assert_eq!(out, "1\n0\n");
    }

    #[test]
    fn test_when_unless() {
        let out = compile_run(
            "(defn main () : Unit
               (when true (println 42))
               (when false (println 99))
               (unless false (println 7))
               (unless true (println 88)))",
        );
        assert_eq!(out, "42\n7\n");
    }

    #[test]
    fn test_cond() {
        let out = compile_run(
            "(defn classify ((n : i32)) : i32
               (cond
                 ((< n 0) (- 1))
                 ((= n 0) 0)
                 (else 1)))
             (defn main () : Unit
               (println (classify (- 5)))
               (println (classify 0))
               (println (classify 10)))",
        );
        assert_eq!(out, "-1\n0\n1\n");
    }

    // ── Let and mutation ────────────────────────────────────────

    #[test]
    fn test_let_binding() {
        let out = compile_run(
            "(defn main () : Unit
               (let (((x : i32) 10)
                     ((y : i32) 20))
                 (println (+ x y))))",
        );
        assert_eq!(out, "30\n");
    }

    #[test]
    fn test_mutable_binding() {
        let out = compile_run(
            "(defn main () : Unit
               (let (((mut x : i32) 0))
                 (set! x 42)
                 (println x)))",
        );
        assert_eq!(out, "42\n");
    }

    #[test]
    fn test_do_block() {
        let out = compile_run(
            "(defn main () : Unit
               (let (((result : i32) (do (println 1) 42)))
                 (println result)))",
        );
        assert_eq!(out, "1\n42\n");
    }

    // ── Function calls ──────────────────────────────────────────

    #[test]
    fn test_function_call() {
        let out = compile_run(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () : Unit (println (add 3 4)))",
        );
        assert_eq!(out, "7\n");
    }

    #[test]
    fn test_recursion() {
        let out = compile_run(
            "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))
             (defn main () : Unit
               (println (factorial 10)))",
        );
        assert_eq!(out, "3628800\n");
    }

    #[test]
    fn test_fibonacci() {
        let out = compile_run(
            "(defn fib ((n : i32)) : i32
               (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
             (defn main () : Unit
               (println (fib 0))
               (println (fib 1))
               (println (fib 10)))",
        );
        assert_eq!(out, "0\n1\n55\n");
    }

    #[test]
    fn test_mutual_recursion() {
        let out = compile_run(
            "(defn is-even ((n : i32)) : Bool
               (if (= n 0) true (is-odd (- n 1))))
             (defn is-odd ((n : i32)) : Bool
               (if (= n 0) false (is-even (- n 1))))
             (defn main () : Unit
               (println (is-even 10))
               (println (is-odd 10)))",
        );
        assert_eq!(out, "true\nfalse\n");
    }

    // ── Oracle tests (compiler vs interpreter agree) ────────────

    #[test]
    fn oracle_arithmetic() {
        oracle_test(
            "(defn main () : Unit
               (println (+ 1 2))
               (println (- 10 3))
               (println (* 4 5))
               (println (/ 15 4))
               (println (mod 17 5))
               (println (+ 1 2 3 4 5)))",
        );
    }

    #[test]
    fn oracle_comparisons() {
        oracle_test(
            "(defn main () : Unit
               (println (< 1 2))
               (println (> 1 2))
               (println (= 3 3))
               (println (!= 3 4))
               (println (<= 5 5))
               (println (>= 5 6)))",
        );
    }

    #[test]
    fn oracle_booleans() {
        oracle_test(
            "(defn main () : Unit
               (println (not true))
               (println (and true true))
               (println (and true false))
               (println (or false true)))",
        );
    }

    #[test]
    fn oracle_if_else() {
        oracle_test(
            "(defn main () : Unit
               (println (if (> 5 3) 1 0))
               (println (if (< 5 3) 1 0)))",
        );
    }

    #[test]
    fn oracle_function_calls() {
        oracle_test(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () : Unit (println (add 3 4)))",
        );
    }

    #[test]
    fn oracle_factorial() {
        oracle_test(
            "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))
             (defn main () : Unit
               (println (factorial 1))
               (println (factorial 5))
               (println (factorial 10)))",
        );
    }

    #[test]
    fn oracle_fibonacci() {
        oracle_test(
            "(defn fib ((n : i32)) : i32
               (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
             (defn main () : Unit
               (println (fib 0))
               (println (fib 1))
               (println (fib 10)))",
        );
    }

    // ── Fixture-based tests ─────────────────────────────────────

    fn run_fixture_compiled(name: &str) -> String {
        let path = format!(
            "{}/tests/fixtures/{}.weir",
            env!("CARGO_MANIFEST_DIR").replace("/crates/weir-codegen", ""),
            name
        );
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
        compile_run(&source)
    }

    fn run_fixture_interpreted(name: &str) -> String {
        let path = format!(
            "{}/tests/fixtures/{}.weir",
            env!("CARGO_MANIFEST_DIR").replace("/crates/weir-codegen", ""),
            name
        );
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
        interp_run(&source)
    }

    #[test]
    fn fixture_codegen_arithmetic() {
        let compiled = run_fixture_compiled("codegen-arithmetic");
        let interpreted = run_fixture_interpreted("codegen-arithmetic");
        assert_eq!(compiled, interpreted, "codegen-arithmetic: compiler and interpreter disagree");
    }

    #[test]
    fn fixture_codegen_control_flow() {
        let compiled = run_fixture_compiled("codegen-control-flow");
        let interpreted = run_fixture_interpreted("codegen-control-flow");
        assert_eq!(compiled, interpreted, "codegen-control-flow: compiler and interpreter disagree");
    }

    #[test]
    fn fixture_codegen_functions() {
        let compiled = run_fixture_compiled("codegen-functions");
        let interpreted = run_fixture_interpreted("codegen-functions");
        assert_eq!(compiled, interpreted, "codegen-functions: compiler and interpreter disagree");
    }

    #[test]
    fn fixture_codegen_let_mutation() {
        let compiled = run_fixture_compiled("codegen-let-mutation");
        let interpreted = run_fixture_interpreted("codegen-let-mutation");
        assert_eq!(compiled, interpreted, "codegen-let-mutation: compiler and interpreter disagree");
    }
}
