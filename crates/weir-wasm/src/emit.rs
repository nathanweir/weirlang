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
                self.compile_call(expr_id, *callee, args, &ty, func)
            }

            ExprKind::Let { bindings, body } => {
                for binding in bindings {
                    let val_ty = self.expr_ty(binding.value);
                    let compiled_wt = self.compile_expr(binding.value, func);
                    // Use the actual compiled type if the type checker type maps to None
                    let wt = self.wasm_ty(&val_ty).or(compiled_wt);
                    if let Some(local_idx) = self.locals.get(&binding.name).copied() {
                        match wt {
                            Some(_) => func.instruction(&Instruction::LocalSet(local_idx)),
                            None => func.instruction(&Instruction::Drop),
                        };
                    } else {
                        // Allocate a new local
                        let local_wt = wt.unwrap_or(ValType::I64);
                        let idx = self.next_local;
                        self.next_local += 1;
                        self.local_types.push(local_wt);
                        self.locals.insert(binding.name.clone(), idx);
                        if wt.is_some() {
                            func.instruction(&Instruction::LocalSet(idx));
                        }
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
                    self.tco_block_depth += 1;
                    self.compile_expr(*then_branch, func);
                    func.instruction(&Instruction::Else);
                    self.compile_expr(*else_id, func);
                    self.tco_block_depth -= 1;
                    func.instruction(&Instruction::End);
                } else {
                    func.instruction(&Instruction::If(block_ty));
                    self.tco_block_depth += 1;
                    self.compile_expr(*then_branch, func);
                    self.tco_block_depth -= 1;
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

                // Compile as nested if/else — each opens a block
                for (test, body) in clauses {
                    self.compile_expr(*test, func);
                    let cond_ty = self.expr_ty(*test);
                    if self.wasm_ty(&cond_ty) == Some(ValType::I64) {
                        func.instruction(&Instruction::I32WrapI64);
                    }
                    func.instruction(&Instruction::If(block_ty));
                    self.tco_block_depth += 1;
                    self.compile_expr(*body, func);
                    func.instruction(&Instruction::Else);
                    // Note: else is at the same depth as the if body
                }

                // else clause or default (innermost else)
                if let Some(else_expr) = else_clause {
                    self.compile_expr(*else_expr, func);
                } else if let Some(vt) = result_wt {
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
                    self.tco_block_depth -= 1;
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
                self.tco_block_depth += 1;
                self.compile_body(body, func);
                // Drop result since when returns Unit
                self.tco_block_depth -= 1;
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
                self.tco_block_depth += 1;
                self.compile_body(body, func);
                self.tco_block_depth -= 1;
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
                            self.tco_block_depth += 1;
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
                                self.tco_block_depth += 1;

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
                        weir_ast::PatternKind::Constructor { name, .. } => {
                            // Only opened a block if the constructor had a tag
                            if self.constructor_tags.contains_key(name) {
                                self.tco_block_depth -= 1;
                                func.instruction(&Instruction::End);
                            }
                        }
                        _ => {
                            self.tco_block_depth -= 1;
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
        call_expr_id: ExprId,
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

            // TCO: self-tail-call → update params, branch to loop header
            if self.tail_positions.contains(&call_expr_id) {
                if let Some(ref fn_name) = self.current_fn_name.clone() {
                    if name == fn_name {
                        return self.compile_tail_call(args, func);
                    }
                }
            }

            // Compile arguments
            for arg in args {
                self.compile_expr(arg.value, func);
            }

            // Look up function index
            if let Some(&idx) = self.func_indices.get(name) {
                func.instruction(&Instruction::Call(idx));
                // Use the function's declared return type (more reliable than call-site type)
                if let Some(ft) = self.type_info.fn_types.get(name) {
                    return self.wasm_ty(&ft.return_type);
                }
                return self.wasm_ty(result_ty);
            }

            // Check runtime functions
            if let Some(&idx) = self.runtime_fn_indices.get(name.as_str()) {
                func.instruction(&Instruction::Call(idx));
                return self.wasm_ty(result_ty);
            }

            // Struct constructor call — allocate + store fields
            if let Some(sinfo) = self.type_info.struct_defs.get(name).cloned() {
                // Args were already compiled and pushed onto the stack.
                // We need to store them into allocated memory.
                let num_fields = sinfo.fields.len();

                // Save compiled args into temp locals (they're on the stack from above)
                let mut field_locals = Vec::new();
                for (i, (_fname, fty)) in sinfo.fields.iter().enumerate().rev() {
                    // Args are on stack in forward order, so we pop in reverse
                    let _ = i;
                    let fwt = self.wasm_ty(fty).unwrap_or(ValType::I64);
                    let local_idx = self.next_local;
                    self.next_local += 1;
                    self.local_types.push(fwt);
                    field_locals.push((local_idx, fwt));
                    func.instruction(&Instruction::LocalSet(local_idx));
                }
                field_locals.reverse();

                // Allocate: weir_gc_alloc(size, shape)
                let alloc_size = (num_fields * 8) as i32;
                func.instruction(&Instruction::I32Const(alloc_size));
                func.instruction(&Instruction::I64Const(0)); // shape descriptor
                if let Some(&alloc_idx) = self.runtime_fn_indices.get("weir_gc_alloc") {
                    func.instruction(&Instruction::Call(alloc_idx));
                }

                // Store in a temp local for the pointer
                let ptr_local = self.next_local;
                self.next_local += 1;
                self.local_types.push(ValType::I32);
                func.instruction(&Instruction::LocalSet(ptr_local));

                // Store each field at its offset
                for (i, (local_idx, fwt)) in field_locals.iter().enumerate() {
                    let offset = (i * 8) as u64;
                    func.instruction(&Instruction::LocalGet(ptr_local));
                    func.instruction(&Instruction::LocalGet(*local_idx));
                    // Widen to i64 for storage if the field is i32/f32
                    match fwt {
                        ValType::I32 => {
                            func.instruction(&Instruction::I64ExtendI32U);
                            func.instruction(&Instruction::I64Store(memarg(offset)));
                        }
                        ValType::F32 => {
                            // Store f32 as f32 (4 bytes) within the 8-byte slot
                            func.instruction(&Instruction::F32Store(memarg(offset)));
                        }
                        ValType::F64 => {
                            func.instruction(&Instruction::F64Store(memarg(offset)));
                        }
                        _ => {
                            // I64
                            func.instruction(&Instruction::I64Store(memarg(offset)));
                        }
                    }
                }

                // Return the pointer
                func.instruction(&Instruction::LocalGet(ptr_local));
                return Some(ValType::I32);
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
                    _ => {
                        // Widen i32 integer types to i64 for weir_print_i64
                        if self.wasm_ty(&arg_ty) == Some(ValType::I32) {
                            func.instruction(&Instruction::I64ExtendI32U);
                        }
                        "weir_print_i64"
                    }
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
                if args.is_empty() { return None; }

                // Helper closure: convert a value to string based on its type
                // (inlined below since closures can't capture &mut self)

                // Convert first arg to string
                let first_ty = self.expr_ty(args[0].value);
                let first_wt = self.compile_expr(args[0].value, func);
                match &first_ty {
                    Ty::Str => { /* already a string */ }
                    Ty::F32 | Ty::F64 => {
                        // Convert to f64 if f32
                        if first_wt == Some(ValType::F32) {
                            func.instruction(&Instruction::F64PromoteF32);
                        }
                        if let Some(&idx) = self.runtime_fn_indices.get("weir_f64_to_str") {
                            func.instruction(&Instruction::Call(idx));
                        }
                    }
                    Ty::Bool => {
                        // Bool is i64, narrow to i32 for weir_bool_to_str
                        func.instruction(&Instruction::I32WrapI64);
                        if let Some(&idx) = self.runtime_fn_indices.get("weir_bool_to_str") {
                            func.instruction(&Instruction::Call(idx));
                        }
                    }
                    _ => {
                        // Widen to I64 if needed (weir_i64_to_str expects I64)
                        if first_wt == Some(ValType::I32) {
                            func.instruction(&Instruction::I64ExtendI32U);
                        }
                        if let Some(&idx) = self.runtime_fn_indices.get("weir_i64_to_str") {
                            func.instruction(&Instruction::Call(idx));
                        }
                    }
                }

                // For multi-arg str: convert each subsequent arg and concatenate
                for arg in &args[1..] {
                    let arg_ty = self.expr_ty(arg.value);
                    let arg_wt = self.compile_expr(arg.value, func);
                    match &arg_ty {
                        Ty::Str => { /* already a string */ }
                        Ty::F32 | Ty::F64 => {
                            if arg_wt == Some(ValType::F32) {
                                func.instruction(&Instruction::F64PromoteF32);
                            }
                            if let Some(&idx) = self.runtime_fn_indices.get("weir_f64_to_str") {
                                func.instruction(&Instruction::Call(idx));
                            }
                        }
                        Ty::Bool => {
                            func.instruction(&Instruction::I32WrapI64);
                            if let Some(&idx) = self.runtime_fn_indices.get("weir_bool_to_str") {
                                func.instruction(&Instruction::Call(idx));
                            }
                        }
                        _ => {
                            // Widen to I64 if needed
                            if arg_wt == Some(ValType::I32) {
                                func.instruction(&Instruction::I64ExtendI32U);
                            }
                            if let Some(&idx) = self.runtime_fn_indices.get("weir_i64_to_str") {
                                func.instruction(&Instruction::Call(idx));
                            }
                        }
                    }
                    if let Some(&idx) = self.runtime_fn_indices.get("weir_str_concat") {
                        func.instruction(&Instruction::Call(idx));
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

            "nth" => {
                // nth(vec, idx) — vector index, same as vec-get
                if args.len() != 2 { return None; }
                self.compile_expr(args[0].value, func);
                let idx_wt = self.compile_expr(args[1].value, func);
                // Widen index to I64 if needed (weir_vec_get expects I64)
                if idx_wt == Some(ValType::I32) {
                    func.instruction(&Instruction::I64ExtendI32U);
                }
                if let Some(&idx) = self.runtime_fn_indices.get("weir_vec_get") {
                    func.instruction(&Instruction::Call(idx));
                }
                // vec_get returns i64 — narrow if result type needs it
                let result_wt = self.wasm_ty(result_ty).unwrap_or(ValType::I64);
                if result_wt == ValType::I32 {
                    func.instruction(&Instruction::I32WrapI64);
                }
                Some(Some(result_wt))
            }

            "set-nth" => {
                // set-nth(vec, idx, val) — immutable vector update
                // Allocate a new vector, copy all elements, replace one
                if args.len() != 3 { return None; }
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                self.compile_expr(args[2].value, func);
                // Widen val to i64 if needed
                let val_ty = self.expr_ty(args[2].value);
                let val_wt = self.wasm_ty(&val_ty);
                if val_wt == Some(ValType::I32) {
                    func.instruction(&Instruction::I64ExtendI32U);
                }
                if let Some(&idx) = self.runtime_fn_indices.get("weir_vec_set_nth") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(Some(ValType::I32))
            }

            "append" => {
                // append(vec, val) — return new vector with val appended
                if args.len() != 2 { return None; }
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                // Widen val to i64 if needed
                let val_ty = self.expr_ty(args[1].value);
                let val_wt = self.wasm_ty(&val_ty);
                if val_wt == Some(ValType::I32) {
                    func.instruction(&Instruction::I64ExtendI32U);
                }
                if let Some(&idx) = self.runtime_fn_indices.get("weir_vec_append") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(Some(ValType::I32))
            }

            "mod" => {
                if args.len() != 2 { return None; }
                let lhs_ty = self.expr_ty(args[0].value);
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                let wt = self.wasm_ty(&lhs_ty).unwrap_or(ValType::I64);
                match wt {
                    ValType::I64 => { func.instruction(&Instruction::I64RemS); }
                    ValType::I32 => { func.instruction(&Instruction::I32RemS); }
                    _ => {}
                }
                Some(Some(wt))
            }

            "min" => {
                if args.len() != 2 { return None; }
                let lhs_ty = self.expr_ty(args[0].value);
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                let wt = self.wasm_ty(&lhs_ty).unwrap_or(ValType::I64);
                // min(a, b) = if a < b then a else b
                // Need to save both values
                let a_local = self.next_local;
                self.next_local += 1;
                self.local_types.push(wt);
                let b_local = self.next_local;
                self.next_local += 1;
                self.local_types.push(wt);
                func.instruction(&Instruction::LocalSet(b_local));
                func.instruction(&Instruction::LocalSet(a_local));
                func.instruction(&Instruction::LocalGet(a_local));
                func.instruction(&Instruction::LocalGet(b_local));
                match wt {
                    ValType::I64 => { func.instruction(&Instruction::I64LtS); }
                    ValType::I32 => { func.instruction(&Instruction::I32LtS); }
                    ValType::F64 => { func.instruction(&Instruction::F64Lt); }
                    ValType::F32 => { func.instruction(&Instruction::F32Lt); }
                    _ => { func.instruction(&Instruction::I64LtS); }
                }
                func.instruction(&Instruction::If(BlockType::Result(wt)));
                func.instruction(&Instruction::LocalGet(a_local));
                func.instruction(&Instruction::Else);
                func.instruction(&Instruction::LocalGet(b_local));
                func.instruction(&Instruction::End);
                Some(Some(wt))
            }

            "max" => {
                if args.len() != 2 { return None; }
                let lhs_ty = self.expr_ty(args[0].value);
                self.compile_expr(args[0].value, func);
                self.compile_expr(args[1].value, func);
                let wt = self.wasm_ty(&lhs_ty).unwrap_or(ValType::I64);
                let a_local = self.next_local;
                self.next_local += 1;
                self.local_types.push(wt);
                let b_local = self.next_local;
                self.next_local += 1;
                self.local_types.push(wt);
                func.instruction(&Instruction::LocalSet(b_local));
                func.instruction(&Instruction::LocalSet(a_local));
                func.instruction(&Instruction::LocalGet(a_local));
                func.instruction(&Instruction::LocalGet(b_local));
                match wt {
                    ValType::I64 => { func.instruction(&Instruction::I64GtS); }
                    ValType::I32 => { func.instruction(&Instruction::I32GtS); }
                    ValType::F64 => { func.instruction(&Instruction::F64Gt); }
                    ValType::F32 => { func.instruction(&Instruction::F32Gt); }
                    _ => { func.instruction(&Instruction::I64GtS); }
                }
                func.instruction(&Instruction::If(BlockType::Result(wt)));
                func.instruction(&Instruction::LocalGet(a_local));
                func.instruction(&Instruction::Else);
                func.instruction(&Instruction::LocalGet(b_local));
                func.instruction(&Instruction::End);
                Some(Some(wt))
            }

            "random-int" => {
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                if let Some(&idx) = self.runtime_fn_indices.get("weir_random_int") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(Some(ValType::I64))
            }

            "random-seed" => {
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                if let Some(&idx) = self.runtime_fn_indices.get("weir_random_seed") {
                    func.instruction(&Instruction::Call(idx));
                }
                Some(None)
            }

            "wasm-set-state" => {
                // Store an I32 pointer in the wasm_state global (global 2)
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                func.instruction(&Instruction::GlobalSet(2));
                Some(None)
            }

            "wasm-get-state" => {
                // Load the I32 pointer from the wasm_state global (global 2)
                if !args.is_empty() { return None; }
                func.instruction(&Instruction::GlobalGet(2));
                Some(Some(ValType::I32))
            }

            "not" => {
                // Logical not: (not x) → if x is truthy return 0, else 1
                if args.len() != 1 { return None; }
                self.compile_expr(args[0].value, func);
                let arg_ty = self.expr_ty(args[0].value);
                let wt = self.wasm_ty(&arg_ty).unwrap_or(ValType::I64);
                if wt == ValType::I32 {
                    func.instruction(&Instruction::I32Eqz);
                } else {
                    func.instruction(&Instruction::I64Eqz);
                }
                func.instruction(&Instruction::I64ExtendI32U);
                Some(Some(ValType::I64))
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

    /// Compile a self-tail-call: update params with new arg values, then branch
    /// back to the loop header.
    fn compile_tail_call(
        &mut self,
        args: &[weir_ast::Arg],
        func: &mut Function,
    ) -> Option<ValType> {
        // Compile all new argument values first (before overwriting params)
        let mut temp_locals = Vec::new();
        for arg in args {
            let arg_wt = self.compile_expr(arg.value, func);
            let wt = arg_wt.unwrap_or(ValType::I64);
            let temp = self.next_local;
            self.next_local += 1;
            self.local_types.push(wt);
            func.instruction(&Instruction::LocalSet(temp));
            temp_locals.push((temp, wt));
        }

        // Copy from temp locals to parameter locals (indices 0..N)
        for (i, &(temp, _wt)) in temp_locals.iter().enumerate() {
            func.instruction(&Instruction::LocalGet(temp));
            func.instruction(&Instruction::LocalSet(i as u32));
        }

        // Branch back to loop header
        // tco_block_depth is the number of nested blocks between here and the loop
        func.instruction(&Instruction::Br(self.tco_block_depth));

        // Tail calls don't produce a result (the br is unconditional)
        None
    }
}
