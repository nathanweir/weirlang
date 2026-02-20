use smol_str::SmolStr;
use wasm_encoder::{BlockType, Function, Instruction, ValType};
use weir_ast::{ExprId, ExprKind, Literal};
use weir_typeck::Ty;

use crate::runtime::memarg;
use crate::WasmCompiler;

impl WasmCompiler<'_> {
    /// Compile a single expression, emitting instructions into `func`.
    ///
    /// Returns the WASM value type of the result (or None for Unit).
    pub(crate) fn compile_expr(
        &mut self,
        expr_id: ExprId,
        func: &mut Function,
    ) -> Option<ValType> {
        let expr = &self.ast_module.exprs[expr_id];
        let ty = self.expr_ty(expr_id);

        match &expr.kind {
            ExprKind::Lit(lit) => self.compile_lit(lit, &ty, func),

            ExprKind::Var(name) => self.compile_var(name, &ty, func),

            ExprKind::Call { func: callee, args } => {
                self.compile_call(*callee, args, &ty, func)
            }

            ExprKind::Let { bindings, body } => {
                for binding in bindings {
                    let val_ty = self.expr_ty(binding.value);
                    self.compile_expr(binding.value, func);
                    if let Some(local_idx) = self.locals.get(&binding.name).copied() {
                        let wt = self.wasm_ty(&val_ty);
                        match wt {
                            Some(ValType::I64) => func.instruction(&Instruction::LocalSet(local_idx)),
                            Some(ValType::I32) => func.instruction(&Instruction::LocalSet(local_idx)),
                            Some(ValType::F64) => func.instruction(&Instruction::LocalSet(local_idx)),
                            Some(ValType::F32) => func.instruction(&Instruction::LocalSet(local_idx)),
                            _ => func.instruction(&Instruction::Drop),
                        };
                    } else {
                        // Allocate a new local
                        let wt = self.wasm_ty(&val_ty).unwrap_or(ValType::I64);
                        let idx = self.next_local;
                        self.next_local += 1;
                        self.local_types.push(wt);
                        self.locals.insert(binding.name.clone(), idx);
                        func.instruction(&Instruction::LocalSet(idx));
                    }
                }
                self.compile_body(body, func)
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(*condition, func);
                // Narrow to i32 for boolean (condition might be i64)
                let cond_ty = self.expr_ty(*condition);
                if self.wasm_ty(&cond_ty) == Some(ValType::I64) {
                    func.instruction(&Instruction::I32WrapI64);
                }

                let result_wt = self.wasm_ty(&ty);
                let block_ty = match result_wt {
                    Some(vt) => BlockType::Result(vt),
                    None => BlockType::Empty,
                };

                if let Some(else_id) = else_branch {
                    func.instruction(&Instruction::If(block_ty));
                    self.compile_expr(*then_branch, func);
                    func.instruction(&Instruction::Else);
                    self.compile_expr(*else_id, func);
                    func.instruction(&Instruction::End);
                } else {
                    func.instruction(&Instruction::If(block_ty));
                    self.compile_expr(*then_branch, func);
                    func.instruction(&Instruction::End);
                }
                result_wt
            }

            ExprKind::Cond { clauses, else_clause } => {
                let result_wt = self.wasm_ty(&ty);
                let block_ty = match result_wt {
                    Some(vt) => BlockType::Result(vt),
                    None => BlockType::Empty,
                };

                // Compile as nested if/else
                for (test, body) in clauses {
                    self.compile_expr(*test, func);
                    let cond_ty = self.expr_ty(*test);
                    if self.wasm_ty(&cond_ty) == Some(ValType::I64) {
                        func.instruction(&Instruction::I32WrapI64);
                    }
                    func.instruction(&Instruction::If(block_ty));
                    self.compile_expr(*body, func);
                    func.instruction(&Instruction::Else);
                }

                // else clause or default
                if let Some(else_expr) = else_clause {
                    self.compile_expr(*else_expr, func);
                } else if let Some(vt) = result_wt {
                    // Default value
                    match vt {
                        ValType::I32 => { func.instruction(&Instruction::I32Const(0)); }
                        ValType::I64 => { func.instruction(&Instruction::I64Const(0)); }
                        ValType::F32 => { func.instruction(&Instruction::F32Const(0.0)); }
                        ValType::F64 => { func.instruction(&Instruction::F64Const(0.0)); }
                        _ => {}
                    }
                }

                // Close all if/else blocks
                for _ in clauses {
                    func.instruction(&Instruction::End);
                }

                result_wt
            }

            ExprKind::Do { body } => self.compile_body(body, func),

            ExprKind::When { condition, body } => {
                self.compile_expr(*condition, func);
                let cond_ty = self.expr_ty(*condition);
                if self.wasm_ty(&cond_ty) == Some(ValType::I64) {
                    func.instruction(&Instruction::I32WrapI64);
                }
                func.instruction(&Instruction::If(BlockType::Empty));
                self.compile_body(body, func);
                // Drop result since when returns Unit
                func.instruction(&Instruction::End);
                None
            }

            ExprKind::Unless { condition, body } => {
                self.compile_expr(*condition, func);
                let cond_ty = self.expr_ty(*condition);
                if self.wasm_ty(&cond_ty) == Some(ValType::I64) {
                    func.instruction(&Instruction::I32WrapI64);
                }
                func.instruction(&Instruction::I32Eqz);
                func.instruction(&Instruction::If(BlockType::Empty));
                self.compile_body(body, func);
                func.instruction(&Instruction::End);
                None
            }

            ExprKind::Match { scrutinee, arms } => {
                let result_wt = self.wasm_ty(&ty);
                let scrutinee_ty = self.expr_ty(*scrutinee);

                // Compile scrutinee into a local
                self.compile_expr(*scrutinee, func);
                let scrut_wt = self.wasm_ty(&scrutinee_ty).unwrap_or(ValType::I64);
                let scrut_local = self.next_local;
                self.next_local += 1;
                self.local_types.push(scrut_wt);
                func.instruction(&Instruction::LocalSet(scrut_local));

                // For tagged ADTs: extract tag from high bits
                let block_ty = match result_wt {
                    Some(vt) => BlockType::Result(vt),
                    None => BlockType::Empty,
                };

                // Compile as nested if/else checking tags
                // For now, handle integer literal patterns and wildcards
                let arm_count = arms.len();
                for (i, arm) in arms.iter().enumerate() {
                    let is_last = i == arm_count - 1;
                    let pattern = &self.ast_module.patterns[arm.pattern];

                    match &pattern.kind {
                        weir_ast::PatternKind::Wildcard | weir_ast::PatternKind::Var(_) => {
                            if let weir_ast::PatternKind::Var(name) = &pattern.kind {
                                // Bind the scrutinee to the variable
                                let idx = self.next_local;
                                self.next_local += 1;
                                self.local_types.push(scrut_wt);
                                self.locals.insert(name.clone(), idx);
                                func.instruction(&Instruction::LocalGet(scrut_local));
                                func.instruction(&Instruction::LocalSet(idx));
                            }
                            // Wildcard/var — just emit the body
                            self.compile_body(&arm.body, func);
                        }
                        weir_ast::PatternKind::Literal(lit) => {
                            // Compare scrutinee with literal
                            func.instruction(&Instruction::LocalGet(scrut_local));
                            match lit {
                                Literal::Int(n) => {
                                    if scrut_wt == ValType::I64 {
                                        func.instruction(&Instruction::I64Const(*n));
                                        func.instruction(&Instruction::I64Eq);
                                    } else {
                                        func.instruction(&Instruction::I32Const(*n as i32));
                                        func.instruction(&Instruction::I32Eq);
                                    }
                                }
                                Literal::Bool(b) => {
                                    func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                                    func.instruction(&Instruction::I32Eq);
                                }
                                _ => {
                                    // String/float patterns — not yet implemented
                                    func.instruction(&Instruction::Drop);
                                    func.instruction(&Instruction::I32Const(0));
                                }
                            }
                            func.instruction(&Instruction::If(block_ty));
                            self.compile_body(&arm.body, func);
                            if !is_last {
                                func.instruction(&Instruction::Else);
                            }
                        }
                        weir_ast::PatternKind::Constructor { name, args } => {
                            // Check tag: high 32 bits of the i64 value
                            if let Some(tag) = self.constructor_tags.get(name).copied() {
                                func.instruction(&Instruction::LocalGet(scrut_local));
                                func.instruction(&Instruction::I64Const(32));
                                func.instruction(&Instruction::I64ShrU);
                                func.instruction(&Instruction::I32WrapI64);
                                func.instruction(&Instruction::I32Const(tag as i32));
                                func.instruction(&Instruction::I32Eq);
                                func.instruction(&Instruction::If(block_ty));

                                // Bind args if any — extract payload from low 32 bits
                                if args.len() == 1 {
                                    let arg_pat = &self.ast_module.patterns[args[0]];
                                    if let weir_ast::PatternKind::Var(arg_name) = &arg_pat.kind {
                                        let idx = self.next_local;
                                        self.next_local += 1;
                                        self.local_types.push(ValType::I64);
                                        self.locals.insert(arg_name.clone(), idx);
                                        func.instruction(&Instruction::LocalGet(scrut_local));
                                        func.instruction(&Instruction::I64Const(0xFFFFFFFF));
                                        func.instruction(&Instruction::I64And);
                                        func.instruction(&Instruction::LocalSet(idx));
                                    }
                                }

                                self.compile_body(&arm.body, func);
                                if !is_last {
                                    func.instruction(&Instruction::Else);
                                }
                            } else {
                                // Unknown constructor — skip
                                self.compile_body(&arm.body, func);
                            }
                        }
                        weir_ast::PatternKind::StructDestructure { .. } => {
                            // TODO: implement struct destructuring
                            self.compile_body(&arm.body, func);
                        }
                    }
                }

                // Close all if blocks (for non-wildcard patterns)
                for arm in arms.iter().rev() {
                    let pattern = &self.ast_module.patterns[arm.pattern];
                    match &pattern.kind {
                        weir_ast::PatternKind::Wildcard | weir_ast::PatternKind::Var(_) => {}
                        _ => {
                            func.instruction(&Instruction::End);
                        }
                    }
                }

                result_wt
            }

            ExprKind::SetBang { place, value } => {
                let val_ty = self.expr_ty(*value);
                self.compile_expr(*value, func);
                if let ExprKind::Var(name) = &self.ast_module.exprs[*place].kind {
                    if let Some(&local_idx) = self.locals.get(name) {
                        func.instruction(&Instruction::LocalSet(local_idx));
                    }
                }
                let _ = val_ty;
                None
            }

            ExprKind::VectorLit(elems) => {
                let elem_ty = if let Ty::Vector(et) = &ty {
                    et.as_ref().clone()
                } else {
                    Ty::I64
                };

                // Call vec_alloc(len)
                let len = elems.len() as i64;
                func.instruction(&Instruction::I64Const(len));
                func.instruction(&Instruction::I64Const(0)); // shape
                if let Some(&idx) = self.runtime_fn_indices.get("weir_gc_vec_alloc") {
                    func.instruction(&Instruction::Call(idx));
                }

                // Store in temp local
                let vec_local = self.next_local;
                self.next_local += 1;
                self.local_types.push(ValType::I32);
                func.instruction(&Instruction::LocalSet(vec_local));

                // Store each element
                for (i, &elem_id) in elems.iter().enumerate() {
                    func.instruction(&Instruction::LocalGet(vec_local));
                    let offset = ((1 + i) * 8) as i64;
                    self.compile_expr(elem_id, func);
                    // Widen to i64 if needed
                    let ewt = self.wasm_ty(&elem_ty);
                    if ewt == Some(ValType::I32) {
                        func.instruction(&Instruction::I64ExtendI32U);
                    }
                    func.instruction(&Instruction::I64Store(memarg(offset as u64)));
                }

                func.instruction(&Instruction::LocalGet(vec_local));
                Some(ValType::I32)
            }

            ExprKind::FieldAccess(field_name) => {
                // This shouldn't appear standalone — it's used in calls
                // like (thing .field)
                let _ = field_name;
                None
            }

            ExprKind::Lambda { .. } => {
                // TODO: implement closure compilation
                func.instruction(&Instruction::I32Const(0));
                Some(ValType::I32)
            }

            ExprKind::Ann { expr, .. } => self.compile_expr(*expr, func),

            ExprKind::Unsafe { body } => self.compile_body(body, func),

            ExprKind::WithArena { body, .. } => self.compile_body(body, func),

            ExprKind::Try(inner) => self.compile_expr(*inner, func),

            ExprKind::MapLit(_) => {
                // TODO: implement map literals
                func.instruction(&Instruction::I32Const(0));
                Some(ValType::I32)
            }

            ExprKind::SwapBang { .. }
            | ExprKind::WithTasks { .. }
            | ExprKind::Spawn(_) => {
                // Not supported in WASM
                None
            }

            ExprKind::Target { .. } => {
                // Should have been resolved during expansion
                None
            }
        }
    }

    fn compile_lit(
        &mut self,
        lit: &Literal,
        ty: &Ty,
        func: &mut Function,
    ) -> Option<ValType> {
        match lit {
            Literal::Int(n) => {
                let wt = self.wasm_ty(ty).unwrap_or(ValType::I64);
                match wt {
                    ValType::I32 => {
                        func.instruction(&Instruction::I32Const(*n as i32));
                    }
                    ValType::I64 => {
                        func.instruction(&Instruction::I64Const(*n));
                    }
                    _ => {
                        func.instruction(&Instruction::I64Const(*n));
                    }
                }
                Some(wt)
            }
            Literal::Float(f) => {
                let wt = self.wasm_ty(ty).unwrap_or(ValType::F64);
                match wt {
                    ValType::F32 => {
                        func.instruction(&Instruction::F32Const(*f as f32));
                    }
                    _ => {
                        func.instruction(&Instruction::F64Const(*f));
                    }
                }
                Some(wt)
            }
            Literal::String(s) => {
                // Store string in data section and return pointer
                let offset = self.intern_string(s);
                func.instruction(&Instruction::I32Const(offset as i32));
                Some(ValType::I32)
            }
            Literal::Bool(b) => {
                let val = if *b { 1 } else { 0 };
                // Booleans are i64 in Weir's type system for compatibility
                match self.wasm_ty(ty) {
                    Some(ValType::I64) => {
                        func.instruction(&Instruction::I64Const(val));
                        Some(ValType::I64)
                    }
                    _ => {
                        func.instruction(&Instruction::I32Const(val as i32));
                        Some(ValType::I32)
                    }
                }
            }
        }
    }

    fn compile_var(
        &self,
        name: &SmolStr,
        ty: &Ty,
        func: &mut Function,
    ) -> Option<ValType> {
        // Check locals
        if let Some(&idx) = self.locals.get(name) {
            func.instruction(&Instruction::LocalGet(idx));
            return self.wasm_ty(ty);
        }

        // Check nullary constructors (e.g., LT, EQ, GT used as bare variables)
        if let Some(&tag) = self.constructor_tags.get(name) {
            let tag_val = (tag as i64) << 32;
            func.instruction(&Instruction::I64Const(tag_val));
            return Some(ValType::I64);
        }

        // Check function references (for passing functions as values)
        // For now, just emit 0
        if self.func_indices.contains_key(name) {
            func.instruction(&Instruction::I32Const(0));
            return Some(ValType::I32);
        }

        // Unknown — emit default
        let wt = self.wasm_ty(ty).unwrap_or(ValType::I64);
        match wt {
            ValType::I32 => func.instruction(&Instruction::I32Const(0)),
            ValType::I64 => func.instruction(&Instruction::I64Const(0)),
            ValType::F32 => func.instruction(&Instruction::F32Const(0.0)),
            ValType::F64 => func.instruction(&Instruction::F64Const(0.0)),
            _ => func.instruction(&Instruction::I32Const(0)),
        };
        Some(wt)
    }

    fn compile_call(
        &mut self,
        callee: ExprId,
        args: &[weir_ast::Arg],
        result_ty: &Ty,
        func: &mut Function,
    ) -> Option<ValType> {
        let callee_expr = &self.ast_module.exprs[callee];

        if let ExprKind::Var(name) = &callee_expr.kind {
            // Check for built-in operators
            if let Some(result) = self.compile_builtin_call(name, args, result_ty, func) {
                return result;
            }

            // Compile arguments
            for arg in args {
                self.compile_expr(arg.value, func);
            }

            // Look up function index
            if let Some(&idx) = self.func_indices.get(name) {
                func.instruction(&Instruction::Call(idx));
                return self.wasm_ty(result_ty);
            }

            // Check runtime functions
            if let Some(&idx) = self.runtime_fn_indices.get(name.as_str()) {
                func.instruction(&Instruction::Call(idx));
                return self.wasm_ty(result_ty);
            }

            // Constructor call — pack tag + args into i64
            if let Some(&tag) = self.constructor_tags.get(name) {
                // If args were already compiled above, we need the payload
                // For nullary constructors:
                if args.is_empty() {
                    // Drop any args already pushed (shouldn't happen for 0 args)
                    let tag_val = (tag as i64) << 32;
                    func.instruction(&Instruction::I64Const(tag_val));
                    return Some(ValType::I64);
                } else {
                    // Tag in high 32 bits, single arg in low 32 bits
                    // Args were already compiled — the last one is on the stack
                    // We need to pack: (tag << 32) | payload
                    let payload_local = self.next_local;
                    self.next_local += 1;
                    self.local_types.push(ValType::I64);

                    // If only one arg and it's i32, extend
                    let arg_wt = self.wasm_ty(&self.expr_ty(args[0].value));
                    if arg_wt == Some(ValType::I32) {
                        func.instruction(&Instruction::I64ExtendI32U);
                    }
                    func.instruction(&Instruction::LocalSet(payload_local));

                    func.instruction(&Instruction::I64Const((tag as i64) << 32));
                    func.instruction(&Instruction::LocalGet(payload_local));
                    func.instruction(&Instruction::I64Or);
                    return Some(ValType::I64);
                }
            }

            // Unknown function — try as extern
            self.wasm_ty(result_ty)
        } else if let ExprKind::FieldAccess(field) = &callee_expr.kind {
            // Field access: compile the argument (struct), then load field
            if args.len() == 1 {
                self.compile_expr(args[0].value, func);
                // Load field at offset based on struct layout
                if let Some(offset) = self.get_field_offset(&self.expr_ty(args[0].value), field) {
                    let field_wt = self.wasm_ty(result_ty).unwrap_or(ValType::I32);
                    match field_wt {
                        ValType::I64 => func.instruction(&Instruction::I64Load(memarg(offset as u64))),
                        ValType::F64 => func.instruction(&Instruction::F64Load(memarg(offset as u64))),
                        ValType::F32 => func.instruction(&Instruction::F32Load(memarg(offset as u64))),
                        _ => func.instruction(&Instruction::I32Load(memarg(offset as u64))),
                    };
                    return Some(field_wt);
                }
            }
            self.wasm_ty(result_ty)
        } else {
            // Indirect call (closure)
            // TODO: implement call_indirect
            for arg in args {
                self.compile_expr(arg.value, func);
            }
            self.wasm_ty(result_ty)
        }
    }

    fn compile_builtin_call(
        &mut self,
        name: &str,
        args: &[weir_ast::Arg],
        result_ty: &Ty,
        func: &mut Function,
    ) -> Option<Option<ValType>> {
        match name {
            "+" | "-" | "*" | "/" | "%" => {
                if args.len() != 2 {
                    return None;
                }
                let lhs_ty = self.expr_ty(args[0].value);
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);

                let wt = self.wasm_ty(&lhs_ty).unwrap_or(ValType::I64);
                match (name, wt) {
                    ("+", ValType::I64) => { func.instruction(&Instruction::I64Add); }
                    ("-", ValType::I64) => { func.instruction(&Instruction::I64Sub); }
                    ("*", ValType::I64) => { func.instruction(&Instruction::I64Mul); }
                    ("/", ValType::I64) => { func.instruction(&Instruction::I64DivS); }
                    ("%", ValType::I64) => { func.instruction(&Instruction::I64RemS); }
                    ("+", ValType::I32) => { func.instruction(&Instruction::I32Add); }
                    ("-", ValType::I32) => { func.instruction(&Instruction::I32Sub); }
                    ("*", ValType::I32) => { func.instruction(&Instruction::I32Mul); }
                    ("/", ValType::I32) => { func.instruction(&Instruction::I32DivS); }
                    ("%", ValType::I32) => { func.instruction(&Instruction::I32RemS); }
                    ("+", ValType::F64) => { func.instruction(&Instruction::F64Add); }
                    ("-", ValType::F64) => { func.instruction(&Instruction::F64Sub); }
                    ("*", ValType::F64) => { func.instruction(&Instruction::F64Mul); }
                    ("/", ValType::F64) => { func.instruction(&Instruction::F64Div); }
                    ("+", ValType::F32) => { func.instruction(&Instruction::F32Add); }
                    ("-", ValType::F32) => { func.instruction(&Instruction::F32Sub); }
                    ("*", ValType::F32) => { func.instruction(&Instruction::F32Mul); }
                    ("/", ValType::F32) => { func.instruction(&Instruction::F32Div); }
                    _ => {}
                }
                Some(Some(wt))
            }

            "=" | "!=" | "<" | ">" | "<=" | ">=" => {
                if args.len() != 2 {
                    return None;
                }
                let lhs_ty = self.expr_ty(args[0].value);
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);

                let wt = self.wasm_ty(&lhs_ty).unwrap_or(ValType::I64);

                // String equality
                if matches!(lhs_ty, Ty::Str) && name == "=" {
                    if let Some(&idx) = self.runtime_fn_indices.get("weir_str_eq") {
                        func.instruction(&Instruction::Call(idx));
                    }
                    return Some(Some(ValType::I64));
                }

                match (name, wt) {
                    ("=", ValType::I64) => { func.instruction(&Instruction::I64Eq); }
                    ("!=", ValType::I64) => { func.instruction(&Instruction::I64Ne); }
                    ("<", ValType::I64) => { func.instruction(&Instruction::I64LtS); }
                    (">", ValType::I64) => { func.instruction(&Instruction::I64GtS); }
                    ("<=", ValType::I64) => { func.instruction(&Instruction::I64LeS); }
                    (">=", ValType::I64) => { func.instruction(&Instruction::I64GeS); }
                    ("=", ValType::I32) => { func.instruction(&Instruction::I32Eq); }
                    ("!=", ValType::I32) => { func.instruction(&Instruction::I32Ne); }
                    ("<", ValType::I32) => { func.instruction(&Instruction::I32LtS); }
                    (">", ValType::I32) => { func.instruction(&Instruction::I32GtS); }
                    ("<=", ValType::I32) => { func.instruction(&Instruction::I32LeS); }
                    (">=", ValType::I32) => { func.instruction(&Instruction::I32GeS); }
                    ("=", ValType::F64) => { func.instruction(&Instruction::F64Eq); }
                    ("!=", ValType::F64) => { func.instruction(&Instruction::F64Ne); }
                    ("<", ValType::F64) => { func.instruction(&Instruction::F64Lt); }
                    (">", ValType::F64) => { func.instruction(&Instruction::F64Gt); }
                    ("<=", ValType::F64) => { func.instruction(&Instruction::F64Le); }
                    (">=", ValType::F64) => { func.instruction(&Instruction::F64Ge); }
                    ("=", ValType::F32) => { func.instruction(&Instruction::F32Eq); }
                    ("!=", ValType::F32) => { func.instruction(&Instruction::F32Ne); }
                    ("<", ValType::F32) => { func.instruction(&Instruction::F32Lt); }
                    (">", ValType::F32) => { func.instruction(&Instruction::F32Gt); }
                    ("<=", ValType::F32) => { func.instruction(&Instruction::F32Le); }
                    (">=", ValType::F32) => { func.instruction(&Instruction::F32Ge); }
                    _ => {}
                }

                // Comparisons produce i32, extend to i64 for Weir's Bool type
                func.instruction(&Instruction::I64ExtendI32U);
                Some(Some(ValType::I64))
            }

            "and" => {
                if args.len() != 2 { return None; }
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                func.instruction(&Instruction::I64And);
                Some(Some(ValType::I64))
            }

            "or" => {
                if args.len() != 2 { return None; }
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                func.instruction(&Instruction::I64Or);
                Some(Some(ValType::I64))
            }

            "not" => {
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                func.instruction(&Instruction::I64Eqz);
                func.instruction(&Instruction::I64ExtendI32U);
                Some(Some(ValType::I64))
            }

            "println" | "print" => {
                if args.is_empty() { return None; }
                let arg_ty = self.expr_ty(args[0].value);
                self.compile_expr(args[0].value, func);

                let print_fn = match &arg_ty {
                    Ty::Str => "weir_print_str",
                    Ty::F32 | Ty::F64 => "weir_print_f64",
                    Ty::Bool => {
                        // Convert i64 bool to i32
                        func.instruction(&Instruction::I32WrapI64);
                        "weir_print_bool"
                    }
                    Ty::Unit => "weir_print_unit",
                    _ => "weir_print_i64",
                };

                if let Some(&idx) = self.runtime_fn_indices.get(print_fn) {
                    func.instruction(&Instruction::Call(idx));
                }

                if name == "println" {
                    if let Some(&idx) = self.runtime_fn_indices.get("weir_print_newline") {
                        func.instruction(&Instruction::Call(idx));
                    }
                }

                Some(None) // Unit
            }

            "str" => {
                if args.len() != 1 { return None; }
                let arg_ty = self.expr_ty(args[0].value);
                self.compile_expr(args[0].value, func);

                match &arg_ty {
                    Ty::Str => { /* already a string */ }
                    _ => {
                        if let Some(&idx) = self.runtime_fn_indices.get("weir_i64_to_str") {
                            func.instruction(&Instruction::Call(idx));
                        }
                    }
                }
                Some(Some(ValType::I32))
            }

            "vec-get" => {
                if args.len() != 2 { return None; }
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                if let Some(&idx) = self.runtime_fn_indices.get("weir_vec_get") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(self.wasm_ty(result_ty))
            }

            "vec-len" | "length" => {
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                if let Some(&idx) = self.runtime_fn_indices.get("weir_vec_len") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(Some(ValType::I64))
            }

            "vec-set" => {
                if args.len() != 3 { return None; }
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                self.compile_expr(args[2].value, func);
                if let Some(&idx) = self.runtime_fn_indices.get("weir_vec_set") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(None)
            }

            "string-concat" | "str-concat" => {
                if args.len() != 2 { return None; }
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                if let Some(&idx) = self.runtime_fn_indices.get("weir_str_concat") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(Some(ValType::I32))
            }

            "string-length" => {
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                if let Some(&idx) = self.runtime_fn_indices.get("weir_string_length") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(Some(ValType::I64))
            }

            "random-seed" => {
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                func.instruction(&Instruction::Drop);
                Some(None)
            }

            _ => None, // Not a builtin
        }
    }

    pub(crate) fn compile_body(
        &mut self,
        body: &[ExprId],
        func: &mut Function,
    ) -> Option<ValType> {
        let mut last_ty = None;
        for (i, &expr_id) in body.iter().enumerate() {
            if i > 0 && last_ty.is_some() {
                // Drop previous expression result if not the last
                func.instruction(&Instruction::Drop);
            }
            last_ty = self.compile_expr(expr_id, func);
        }
        last_ty
    }
}
