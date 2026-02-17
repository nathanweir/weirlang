use smol_str::SmolStr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use weir_ast::*;

// ── Error ────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct InterpError {
    pub message: String,
    pub span: Option<Span>,
}

impl InterpError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
        }
    }

    fn with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
        }
    }
}

impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(span) = self.span {
            write!(f, "[{}:{}] {}", span.start, span.end, self.message)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl std::error::Error for InterpError {}

// ── Value ────────────────────────────────────────────────────────

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,
    Vector(Vec<Value>),
    Map(Vec<(Value, Value)>),
    Closure {
        name: Option<SmolStr>,
        params: Vec<SmolStr>,
        body: Vec<ExprId>,
        env: Env,
    },
    /// ADT constructor. When args.len() == arity, it's fully applied.
    Constructor {
        type_name: SmolStr,
        variant_name: SmolStr,
        arity: usize,
        args: Vec<Value>,
    },
    StructConstructor {
        type_name: SmolStr,
        field_names: Vec<SmolStr>,
    },
    StructInstance {
        type_name: SmolStr,
        fields: Vec<(SmolStr, Value)>,
    },
    Builtin(SmolStr),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{:.1}", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Unit => write!(f, "()"),
            Value::Vector(elems) => {
                write!(f, "[")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, "]")
            }
            Value::Map(pairs) => {
                write!(f, "{{")?;
                for (i, (k, v)) in pairs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{} {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Closure { name, .. } => match name {
                Some(n) => write!(f, "<fn {}>", n),
                None => write!(f, "<fn>"),
            },
            Value::Constructor {
                variant_name,
                args,
                arity,
                ..
            } => {
                if args.len() < *arity {
                    write!(f, "<constructor {}>", variant_name)
                } else if args.is_empty() {
                    write!(f, "{}", variant_name)
                } else {
                    write!(f, "({}", variant_name)?;
                    for a in args {
                        write!(f, " {}", a)?;
                    }
                    write!(f, ")")
                }
            }
            Value::StructConstructor { type_name, .. } => {
                write!(f, "<struct-constructor {}>", type_name)
            }
            Value::StructInstance { type_name, fields } => {
                write!(f, "({}", type_name)?;
                for (name, val) in fields {
                    write!(f, " :{} {}", name, val)?;
                }
                write!(f, ")")
            }
            Value::Builtin(name) => write!(f, "<builtin {}>", name),
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Unit)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Float(b)) => (*a as f64) == *b,
            (Value::Float(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (
                Value::Constructor {
                    variant_name: a_name,
                    args: a_args,
                    ..
                },
                Value::Constructor {
                    variant_name: b_name,
                    args: b_args,
                    ..
                },
            ) => a_name == b_name && a_args == b_args,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            _ => false,
        }
    }
}

// ── Environment ──────────────────────────────────────────────────

#[derive(Clone, Debug)]
pub struct Env(Rc<RefCell<EnvInner>>);

#[derive(Debug)]
struct EnvInner {
    bindings: Vec<(SmolStr, Value)>,
    mutable: Vec<SmolStr>,
    parent: Option<Env>,
}

impl Env {
    fn new() -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: Vec::new(),
            mutable: Vec::new(),
            parent: None,
        })))
    }

    fn child(&self) -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: Vec::new(),
            mutable: Vec::new(),
            parent: Some(self.clone()),
        })))
    }

    fn define(&self, name: SmolStr, value: Value, is_mut: bool) {
        let mut inner = self.0.borrow_mut();
        inner.bindings.push((name.clone(), value));
        if is_mut {
            inner.mutable.push(name);
        }
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        let inner = self.0.borrow();
        for (k, v) in inner.bindings.iter().rev() {
            if k == name {
                return Some(v.clone());
            }
        }
        if let Some(ref parent) = inner.parent {
            parent.lookup(name)
        } else {
            None
        }
    }

    fn set(&self, name: &str, value: Value) -> Result<(), InterpError> {
        let mut inner = self.0.borrow_mut();
        // Check if binding exists in this scope
        let found = inner.bindings.iter().any(|(k, _)| k == name);
        if found {
            let is_mut = inner.mutable.iter().any(|m| m == name);
            if !is_mut {
                return Err(InterpError::new(format!(
                    "cannot mutate immutable binding '{}'",
                    name
                )));
            }
            for (k, v) in inner.bindings.iter_mut().rev() {
                if k == name {
                    *v = value;
                    return Ok(());
                }
            }
            unreachable!()
        } else {
            let parent = inner.parent.clone();
            drop(inner);
            match parent {
                Some(p) => p.set(name, value),
                None => Err(InterpError::new(format!("undefined variable '{}'", name))),
            }
        }
    }
}

// ── Interpreter ──────────────────────────────────────────────────

pub fn interpret(module: &Module) -> Result<String, InterpError> {
    let mut interp = Interpreter {
        module,
        global_env: Env::new(),
        output: String::new(),
        method_to_class: HashMap::new(),
        class_instances: HashMap::new(),
    };
    interp.run()?;
    Ok(interp.output)
}

struct RuntimeInstance {
    type_tag: SmolStr,
    methods: HashMap<SmolStr, Value>,
}

struct Interpreter<'a> {
    module: &'a Module,
    global_env: Env,
    output: String,
    /// method_name -> class_name (reverse lookup for dispatch)
    method_to_class: HashMap<SmolStr, SmolStr>,
    /// class_name -> Vec<RuntimeInstance>
    class_instances: HashMap<SmolStr, Vec<RuntimeInstance>>,
}

impl<'a> Interpreter<'a> {
    fn run(&mut self) -> Result<(), InterpError> {
        // Register builtins
        self.register_builtins();

        // Process all top-level items
        for (item, _span) in &self.module.items {
            self.process_item(item)?;
        }

        // Call main if it exists
        if let Some(main_fn) = self.global_env.lookup("main") {
            self.call_value(&main_fn, &[], Span::new(0, 0))?;
        }

        Ok(())
    }

    fn register_builtins(&self) {
        let builtins = [
            "+", "-", "*", "/", "mod", "<", ">", "<=", ">=", "=", "!=", "not", "and", "or",
            "println", "print", "str", "len", "nth", "append", "type-of",
        ];
        for name in builtins {
            self.global_env.define(
                SmolStr::new(name),
                Value::Builtin(SmolStr::new(name)),
                false,
            );
        }
    }

    // ── Top-level items ──────────────────────────────────────────

    fn process_item(&mut self, item: &Item) -> Result<(), InterpError> {
        match item {
            Item::Defn(d) => self.process_defn(d),
            Item::Deftype(d) => self.process_deftype(d),
            Item::Defstruct(d) => self.process_defstruct(d),
            Item::Defclass(d) => self.process_defclass(d),
            Item::Instance(d) => self.process_instance(d),
            Item::Import(_) | Item::Declare(_) | Item::ExternC(_) => {
                // Skip for now
                Ok(())
            }
        }
    }

    fn process_defn(&mut self, d: &Defn) -> Result<(), InterpError> {
        let params: Vec<SmolStr> = d.params.iter().map(|p| p.name.clone()).collect();
        let closure = Value::Closure {
            name: Some(d.name.clone()),
            params,
            body: d.body.clone(),
            env: self.global_env.clone(),
        };
        self.global_env.define(d.name.clone(), closure, false);
        Ok(())
    }

    fn process_deftype(&mut self, d: &Deftype) -> Result<(), InterpError> {
        for v in &d.variants {
            let arity = v.fields.len();
            let constructor = Value::Constructor {
                type_name: d.name.clone(),
                variant_name: v.name.clone(),
                arity,
                args: Vec::new(),
            };
            // Zero-arity constructors are already "fully applied"
            self.global_env.define(v.name.clone(), constructor, false);
        }
        Ok(())
    }

    fn process_defstruct(&mut self, d: &Defstruct) -> Result<(), InterpError> {
        let field_names: Vec<SmolStr> = d.fields.iter().map(|f| f.name.clone()).collect();
        let constructor = Value::StructConstructor {
            type_name: d.name.clone(),
            field_names,
        };
        self.global_env.define(d.name.clone(), constructor, false);
        Ok(())
    }

    fn process_defclass(&mut self, d: &Defclass) -> Result<(), InterpError> {
        // Register method names in the method_to_class reverse lookup
        for m in &d.methods {
            self.method_to_class.insert(m.name.clone(), d.name.clone());
        }
        Ok(())
    }

    fn process_instance(&mut self, d: &InstanceDef) -> Result<(), InterpError> {
        // Determine the head type tag from the instance's type args
        let type_tag = if d.type_args.is_empty() {
            SmolStr::new("()")
        } else {
            // Resolve the first type arg to get the type tag
            let texpr = &self.module.type_exprs[d.type_args[0]];
            self.type_expr_to_tag(texpr)
        };

        // Evaluate each method definition into a closure
        let mut methods = HashMap::new();
        for method_defn in &d.methods {
            let params: Vec<SmolStr> = method_defn.params.iter().map(|p| p.name.clone()).collect();
            let closure = Value::Closure {
                name: Some(method_defn.name.clone()),
                params,
                body: method_defn.body.clone(),
                env: self.global_env.clone(),
            };
            methods.insert(method_defn.name.clone(), closure);
        }

        let entry = self
            .class_instances
            .entry(d.class_name.clone())
            .or_default();
        entry.push(RuntimeInstance { type_tag, methods });

        Ok(())
    }

    /// Extract a type tag string from a type expression (for instance dispatch).
    fn type_expr_to_tag(&self, texpr: &TypeExpr) -> SmolStr {
        match &texpr.kind {
            TypeExprKind::Named(name) => name.clone(),
            TypeExprKind::Applied { constructor, .. } => {
                let con_expr = &self.module.type_exprs[*constructor];
                if let TypeExprKind::Named(name) = &con_expr.kind {
                    name.clone()
                } else {
                    SmolStr::new("<unknown>")
                }
            }
            _ => SmolStr::new("<unknown>"),
        }
    }

    /// Get the type tag of a runtime value for dispatch.
    fn value_type_tag(value: &Value) -> SmolStr {
        match value {
            Value::Int(_) => SmolStr::new("i32"),
            Value::Float(_) => SmolStr::new("f64"),
            Value::Bool(_) => SmolStr::new("Bool"),
            Value::String(_) => SmolStr::new("String"),
            Value::Unit => SmolStr::new("Unit"),
            Value::Vector(_) => SmolStr::new("Vector"),
            Value::Map(_) => SmolStr::new("Map"),
            Value::Constructor { type_name, .. } => type_name.clone(),
            Value::StructInstance { type_name, .. } => type_name.clone(),
            _ => SmolStr::new("<unknown>"),
        }
    }

    /// Try to dispatch a typeclass method call.
    fn dispatch_method(
        &mut self,
        method_name: &str,
        args: &[Value],
        span: Span,
    ) -> Result<Option<Value>, InterpError> {
        let class_name = match self.method_to_class.get(method_name) {
            Some(cn) => cn.clone(),
            None => return Ok(None),
        };

        if args.is_empty() {
            return Err(InterpError::with_span(
                format!(
                    "typeclass method '{}' requires at least one argument for dispatch",
                    method_name
                ),
                span,
            ));
        }

        // Try each argument for dispatch (not just the first).
        // This handles HKT cases like Functor where the dispatching argument
        // (e.g., `(Some 5)`) may not be the first argument.
        for arg in args {
            let type_tag = Self::value_type_tag(arg);

            // Clone the method value to release the borrow on self
            let method_val = self.class_instances.get(&class_name).and_then(|instances| {
                instances.iter().find_map(|inst| {
                    if inst.type_tag == type_tag {
                        inst.methods.get(method_name).cloned()
                    } else {
                        None
                    }
                })
            });

            if let Some(method_val) = method_val {
                let result = self.call_value(&method_val, args, span)?;
                return Ok(Some(result));
            }
        }

        let type_tag = Self::value_type_tag(&args[0]);
        Err(InterpError::with_span(
            format!("no instance of {} for type '{}'", class_name, type_tag),
            span,
        ))
    }

    // ── Expression evaluation ────────────────────────────────────

    fn eval_expr(&mut self, env: &Env, expr_id: ExprId) -> Result<Value, InterpError> {
        let expr = &self.module.exprs[expr_id];
        let span = expr.span;

        match &expr.kind {
            ExprKind::Lit(lit) => Ok(self.lit_to_value(lit)),

            ExprKind::Var(name) => env.lookup(name).ok_or_else(|| {
                InterpError::with_span(format!("undefined variable '{}'", name), span)
            }),

            ExprKind::Call { func, args } => {
                // Handle field access specially: (.field obj)
                let func_expr = &self.module.exprs[*func];
                if let ExprKind::FieldAccess(field) = &func_expr.kind {
                    if args.is_empty() {
                        return Err(InterpError::with_span(
                            format!("field access .{} requires an argument", field),
                            span,
                        ));
                    }
                    let obj = self.eval_expr(env, args[0].value)?;
                    return self.field_access(&obj, field, span);
                }

                // Check for typeclass method dispatch
                if let ExprKind::Var(name) = &func_expr.kind {
                    if self.method_to_class.contains_key(name.as_str()) {
                        let name = name.clone();
                        let arg_vals: Vec<Value> = args
                            .iter()
                            .map(|a| self.eval_expr(env, a.value))
                            .collect::<Result<_, _>>()?;
                        if let Some(result) = self.dispatch_method(&name, &arg_vals, span)? {
                            return Ok(result);
                        }
                        // Fall through if dispatch failed (shouldn't normally happen)
                    }
                }

                let func_val = self.eval_expr(env, *func)?;

                // Struct constructor with named args
                if let Value::StructConstructor {
                    ref type_name,
                    ref field_names,
                } = func_val
                {
                    return self.construct_struct(
                        env,
                        type_name.clone(),
                        field_names.clone(),
                        args,
                        span,
                    );
                }

                let arg_vals: Vec<Value> = args
                    .iter()
                    .map(|a| self.eval_expr(env, a.value))
                    .collect::<Result<_, _>>()?;
                self.call_value(&func_val, &arg_vals, span)
            }

            ExprKind::Let { bindings, body } => {
                let let_env = env.child();
                for b in bindings {
                    let val = self.eval_expr(&let_env, b.value)?;
                    let_env.define(b.name.clone(), val, b.is_mut);
                }
                self.eval_body(&let_env, body)
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.eval_expr(env, *condition)?;
                if cond.is_truthy() {
                    self.eval_expr(env, *then_branch)
                } else if let Some(e) = else_branch {
                    self.eval_expr(env, *e)
                } else {
                    Ok(Value::Unit)
                }
            }

            ExprKind::Cond {
                clauses,
                else_clause,
            } => {
                for (test, body) in clauses {
                    let test_val = self.eval_expr(env, *test)?;
                    if test_val.is_truthy() {
                        return self.eval_expr(env, *body);
                    }
                }
                if let Some(e) = else_clause {
                    self.eval_expr(env, *e)
                } else {
                    Ok(Value::Unit)
                }
            }

            ExprKind::When { condition, body } => {
                let cond = self.eval_expr(env, *condition)?;
                if cond.is_truthy() {
                    self.eval_body(env, body)
                } else {
                    Ok(Value::Unit)
                }
            }

            ExprKind::Unless { condition, body } => {
                let cond = self.eval_expr(env, *condition)?;
                if !cond.is_truthy() {
                    self.eval_body(env, body)
                } else {
                    Ok(Value::Unit)
                }
            }

            ExprKind::Match { scrutinee, arms } => {
                let val = self.eval_expr(env, *scrutinee)?;
                for arm in arms {
                    if let Some(bindings) = self.try_match(arm.pattern, &val) {
                        let match_env = env.child();
                        for (name, bound_val) in bindings {
                            match_env.define(name, bound_val, false);
                        }
                        return self.eval_body(&match_env, &arm.body);
                    }
                }
                Err(InterpError::with_span(
                    format!("no matching pattern for value: {}", val),
                    span,
                ))
            }

            ExprKind::Lambda { params, body, .. } => {
                let param_names: Vec<SmolStr> = params.iter().map(|p| p.name.clone()).collect();
                Ok(Value::Closure {
                    name: None,
                    params: param_names,
                    body: body.clone(),
                    env: env.clone(),
                })
            }

            ExprKind::Do { body } => self.eval_body(env, body),

            ExprKind::SetBang { place, value } => {
                let place_expr = &self.module.exprs[*place];
                if let ExprKind::Var(name) = &place_expr.kind {
                    let val = self.eval_expr(env, *value)?;
                    env.set(name, val)?;
                    Ok(Value::Unit)
                } else {
                    Err(InterpError::with_span(
                        "set! target must be a variable",
                        span,
                    ))
                }
            }

            ExprKind::Ann { expr, .. } => {
                // Ignore type annotation, just evaluate the expression
                self.eval_expr(env, *expr)
            }

            ExprKind::FieldAccess(field) => {
                // Standalone field access becomes a closure that extracts the field
                let field = field.clone();
                // Return a closure that takes one arg and accesses the field
                // We fake this by using a builtin-like approach
                Ok(Value::Builtin(SmolStr::new(format!(".{}", field))))
            }

            ExprKind::VectorLit(elems) => {
                let vals: Vec<Value> = elems
                    .iter()
                    .map(|&e| self.eval_expr(env, e))
                    .collect::<Result<_, _>>()?;
                Ok(Value::Vector(vals))
            }

            ExprKind::MapLit(pairs) => {
                let vals: Vec<(Value, Value)> = pairs
                    .iter()
                    .map(|&(k, v)| Ok((self.eval_expr(env, k)?, self.eval_expr(env, v)?)))
                    .collect::<Result<_, InterpError>>()?;
                Ok(Value::Map(vals))
            }

            ExprKind::Unsafe { body } => self.eval_body(env, body),

            ExprKind::Try(inner) => {
                // No error handling yet, just evaluate
                self.eval_expr(env, *inner)
            }
        }
    }

    fn eval_body(&mut self, env: &Env, body: &[ExprId]) -> Result<Value, InterpError> {
        let mut result = Value::Unit;
        for &expr in body {
            result = self.eval_expr(env, expr)?;
        }
        Ok(result)
    }

    fn lit_to_value(&self, lit: &Literal) -> Value {
        match lit {
            Literal::Int(n) => Value::Int(*n),
            Literal::Float(n) => Value::Float(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
        }
    }

    // ── Function calling ─────────────────────────────────────────

    fn call_value(
        &mut self,
        func: &Value,
        args: &[Value],
        span: Span,
    ) -> Result<Value, InterpError> {
        match func {
            Value::Closure {
                params, body, env, ..
            } => {
                if args.len() != params.len() {
                    return Err(InterpError::with_span(
                        format!("expected {} arguments, got {}", params.len(), args.len()),
                        span,
                    ));
                }
                let call_env = env.child();
                for (param, arg) in params.iter().zip(args.iter()) {
                    call_env.define(param.clone(), arg.clone(), false);
                }
                self.eval_body(&call_env, body)
            }

            Value::Constructor {
                type_name,
                variant_name,
                arity,
                args: existing_args,
            } => {
                let mut new_args = existing_args.clone();
                new_args.extend_from_slice(args);
                if new_args.len() > *arity {
                    return Err(InterpError::with_span(
                        format!(
                            "constructor {} expects {} arguments, got {}",
                            variant_name,
                            arity,
                            new_args.len()
                        ),
                        span,
                    ));
                }
                Ok(Value::Constructor {
                    type_name: type_name.clone(),
                    variant_name: variant_name.clone(),
                    arity: *arity,
                    args: new_args,
                })
            }

            Value::Builtin(name) => self.call_builtin(name, args, span),

            _ => Err(InterpError::with_span(
                format!("cannot call value: {}", func),
                span,
            )),
        }
    }

    // ── Struct construction ──────────────────────────────────────

    fn construct_struct(
        &mut self,
        env: &Env,
        type_name: SmolStr,
        field_names: Vec<SmolStr>,
        args: &[Arg],
        span: Span,
    ) -> Result<Value, InterpError> {
        let has_named = args.iter().any(|a| a.name.is_some());

        let fields = if has_named {
            // Named construction
            let mut fields: Vec<(SmolStr, Value)> = Vec::new();
            for arg in args {
                let name = arg.name.as_ref().ok_or_else(|| {
                    InterpError::with_span(
                        "cannot mix positional and named arguments in struct construction",
                        span,
                    )
                })?;
                let val = self.eval_expr(env, arg.value)?;
                fields.push((name.clone(), val));
            }
            // Verify all fields are present
            for expected in &field_names {
                if !fields.iter().any(|(n, _)| n == expected) {
                    return Err(InterpError::with_span(
                        format!("missing field '{}' in struct {}", expected, type_name),
                        span,
                    ));
                }
            }
            fields
        } else {
            // Positional construction
            if args.len() != field_names.len() {
                return Err(InterpError::with_span(
                    format!(
                        "struct {} expects {} fields, got {}",
                        type_name,
                        field_names.len(),
                        args.len()
                    ),
                    span,
                ));
            }
            let mut fields = Vec::new();
            for (name, arg) in field_names.iter().zip(args.iter()) {
                let val = self.eval_expr(env, arg.value)?;
                fields.push((name.clone(), val));
            }
            fields
        };

        Ok(Value::StructInstance { type_name, fields })
    }

    // ── Field access ─────────────────────────────────────────────

    fn field_access(&self, obj: &Value, field: &str, span: Span) -> Result<Value, InterpError> {
        match obj {
            Value::StructInstance { type_name, fields } => {
                for (name, val) in fields {
                    if name == field {
                        return Ok(val.clone());
                    }
                }
                Err(InterpError::with_span(
                    format!("struct {} has no field '{}'", type_name, field),
                    span,
                ))
            }
            Value::Map(pairs) => {
                let key = Value::String(field.to_string());
                for (k, v) in pairs {
                    if *k == key {
                        return Ok(v.clone());
                    }
                }
                Err(InterpError::with_span(
                    format!("key '{}' not found in map", field),
                    span,
                ))
            }
            _ => Err(InterpError::with_span(
                format!("cannot access field .{} on {}", field, obj),
                span,
            )),
        }
    }

    // ── Pattern matching ─────────────────────────────────────────

    fn try_match(&self, pat_id: PatternId, value: &Value) -> Option<Vec<(SmolStr, Value)>> {
        let pat = &self.module.patterns[pat_id];
        match &pat.kind {
            PatternKind::Wildcard => Some(vec![]),

            PatternKind::Var(name) => Some(vec![(name.clone(), value.clone())]),

            PatternKind::Literal(lit) => {
                let lit_val = self.lit_to_value(lit);
                if lit_val == *value {
                    Some(vec![])
                } else {
                    None
                }
            }

            PatternKind::Constructor { name, args } => {
                if let Value::Constructor {
                    variant_name,
                    args: val_args,
                    arity,
                    ..
                } = value
                {
                    if name != variant_name || args.len() != *arity || args.len() != val_args.len()
                    {
                        return None;
                    }
                    let mut bindings = Vec::new();
                    for (sub_pat, sub_val) in args.iter().zip(val_args.iter()) {
                        match self.try_match(*sub_pat, sub_val) {
                            Some(sub_bindings) => bindings.extend(sub_bindings),
                            None => return None,
                        }
                    }
                    Some(bindings)
                } else {
                    None
                }
            }

            PatternKind::StructDestructure { fields } => {
                if let Value::StructInstance {
                    fields: val_fields, ..
                } = value
                {
                    let mut bindings = Vec::new();
                    for field_pat in fields {
                        let val = val_fields
                            .iter()
                            .find(|(name, _)| *name == field_pat.field_name)
                            .map(|(_, v)| v);
                        match val {
                            Some(v) => {
                                let bind_name = field_pat
                                    .binding
                                    .clone()
                                    .unwrap_or_else(|| field_pat.field_name.clone());
                                bindings.push((bind_name, v.clone()));
                            }
                            None => return None,
                        }
                    }
                    Some(bindings)
                } else {
                    None
                }
            }
        }
    }

    // ── Built-in functions ───────────────────────────────────────

    fn call_builtin(
        &mut self,
        name: &str,
        args: &[Value],
        span: Span,
    ) -> Result<Value, InterpError> {
        // Handle field access builtins (.field)
        if let Some(field) = name.strip_prefix('.') {
            if args.len() != 1 {
                return Err(InterpError::with_span(
                    format!(".{} expects 1 argument, got {}", field, args.len()),
                    span,
                ));
            }
            return self.field_access(&args[0], field, span);
        }

        match name {
            "+" => self.numeric_fold(args, 0, 0.0, |a, b| a + b, |a, b| a + b, span),
            "*" => self.numeric_fold(args, 1, 1.0, |a, b| a * b, |a, b| a * b, span),

            "-" => {
                if args.is_empty() {
                    return Err(InterpError::with_span(
                        "- requires at least 1 argument",
                        span,
                    ));
                }
                if args.len() == 1 {
                    return match &args[0] {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        Value::Float(n) => Ok(Value::Float(-n)),
                        _ => Err(InterpError::with_span("- expects numeric arguments", span)),
                    };
                }
                self.numeric_reduce(args, |a, b| a - b, |a, b| a - b, span)
            }

            "/" => {
                if args.len() < 2 {
                    return Err(InterpError::with_span(
                        "/ requires at least 2 arguments",
                        span,
                    ));
                }
                // Check for division by zero
                for arg in &args[1..] {
                    match arg {
                        Value::Int(0) => {
                            return Err(InterpError::with_span("division by zero", span))
                        }
                        Value::Float(f) if *f == 0.0 => {
                            return Err(InterpError::with_span("division by zero", span))
                        }
                        _ => {}
                    }
                }
                self.numeric_reduce(args, |a, b| a / b, |a, b| a / b, span)
            }

            "mod" => {
                if args.len() != 2 {
                    return Err(InterpError::with_span(
                        "mod requires exactly 2 arguments",
                        span,
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => {
                        if *b == 0 {
                            return Err(InterpError::with_span("division by zero", span));
                        }
                        Ok(Value::Int(a % b))
                    }
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64) % b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a % (*b as f64))),
                    _ => Err(InterpError::with_span(
                        "mod expects numeric arguments",
                        span,
                    )),
                }
            }

            "<" => self.compare_op(args, |ord| ord.is_lt(), span),
            ">" => self.compare_op(args, |ord| ord.is_gt(), span),
            "<=" => self.compare_op(args, |ord| ord.is_le(), span),
            ">=" => self.compare_op(args, |ord| ord.is_ge(), span),

            "=" => {
                if args.len() != 2 {
                    return Err(InterpError::with_span(
                        "= requires exactly 2 arguments",
                        span,
                    ));
                }
                Ok(Value::Bool(args[0] == args[1]))
            }

            "!=" => {
                if args.len() != 2 {
                    return Err(InterpError::with_span(
                        "!= requires exactly 2 arguments",
                        span,
                    ));
                }
                Ok(Value::Bool(args[0] != args[1]))
            }

            "not" => {
                if args.len() != 1 {
                    return Err(InterpError::with_span(
                        "not requires exactly 1 argument",
                        span,
                    ));
                }
                Ok(Value::Bool(!args[0].is_truthy()))
            }

            "and" => {
                for arg in args {
                    if !arg.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                }
                Ok(Value::Bool(true))
            }

            "or" => {
                for arg in args {
                    if arg.is_truthy() {
                        return Ok(Value::Bool(true));
                    }
                }
                Ok(Value::Bool(false))
            }

            "println" => {
                if args.is_empty() {
                    self.output.push('\n');
                } else if args.len() == 1 {
                    self.output.push_str(&format!("{}\n", args[0]));
                } else {
                    return Err(InterpError::with_span(
                        format!("println expects 0 or 1 arguments, got {}", args.len()),
                        span,
                    ));
                }
                Ok(Value::Unit)
            }

            "print" => {
                if args.len() != 1 {
                    return Err(InterpError::with_span(
                        format!("print expects 1 argument, got {}", args.len()),
                        span,
                    ));
                }
                self.output.push_str(&format!("{}", args[0]));
                Ok(Value::Unit)
            }

            "str" => {
                let mut result = String::new();
                for arg in args {
                    result.push_str(&format!("{}", arg));
                }
                Ok(Value::String(result))
            }

            "len" => {
                if args.len() != 1 {
                    return Err(InterpError::with_span("len requires 1 argument", span));
                }
                match &args[0] {
                    Value::Vector(v) => Ok(Value::Int(v.len() as i64)),
                    Value::Map(m) => Ok(Value::Int(m.len() as i64)),
                    Value::String(s) => Ok(Value::Int(s.len() as i64)),
                    _ => Err(InterpError::with_span(
                        format!("len not supported for {}", args[0]),
                        span,
                    )),
                }
            }

            "nth" => {
                if args.len() != 2 {
                    return Err(InterpError::with_span("nth requires 2 arguments", span));
                }
                match (&args[0], &args[1]) {
                    (Value::Vector(v), Value::Int(i)) => {
                        let idx = *i as usize;
                        if idx < v.len() {
                            Ok(v[idx].clone())
                        } else {
                            Err(InterpError::with_span(
                                format!("index {} out of bounds (len {})", i, v.len()),
                                span,
                            ))
                        }
                    }
                    _ => Err(InterpError::with_span("nth expects (vector, int)", span)),
                }
            }

            "append" => {
                if args.len() != 2 {
                    return Err(InterpError::with_span("append requires 2 arguments", span));
                }
                match (&args[0], &args[1]) {
                    (Value::Vector(v), val) => {
                        let mut new_vec = v.clone();
                        new_vec.push(val.clone());
                        Ok(Value::Vector(new_vec))
                    }
                    _ => Err(InterpError::with_span(
                        "append expects (vector, value)",
                        span,
                    )),
                }
            }

            "type-of" => {
                if args.len() != 1 {
                    return Err(InterpError::with_span("type-of requires 1 argument", span));
                }
                let type_name = match &args[0] {
                    Value::Int(_) => "Int",
                    Value::Float(_) => "Float",
                    Value::Bool(_) => "Bool",
                    Value::String(_) => "String",
                    Value::Unit => "Unit",
                    Value::Vector(_) => "Vector",
                    Value::Map(_) => "Map",
                    Value::Closure { .. } => "Fn",
                    Value::Constructor { type_name, .. } => type_name.as_str(),
                    Value::StructConstructor { type_name, .. } => type_name.as_str(),
                    Value::StructInstance { type_name, .. } => type_name.as_str(),
                    Value::Builtin(_) => "Builtin",
                };
                Ok(Value::String(type_name.to_string()))
            }

            _ => Err(InterpError::with_span(
                format!("unknown builtin '{}'", name),
                span,
            )),
        }
    }

    // ── Numeric helpers ──────────────────────────────────────────

    fn numeric_fold(
        &self,
        args: &[Value],
        int_init: i64,
        float_init: f64,
        int_op: fn(i64, i64) -> i64,
        float_op: fn(f64, f64) -> f64,
        span: Span,
    ) -> Result<Value, InterpError> {
        if args.is_empty() {
            return Ok(Value::Int(int_init));
        }

        let has_float = args.iter().any(|a| matches!(a, Value::Float(_)));

        if has_float {
            let mut acc = float_init;
            for arg in args {
                match arg {
                    Value::Float(f) => acc = float_op(acc, *f),
                    Value::Int(i) => acc = float_op(acc, *i as f64),
                    _ => {
                        return Err(InterpError::with_span(
                            "arithmetic expects numeric arguments",
                            span,
                        ))
                    }
                }
            }
            Ok(Value::Float(acc))
        } else {
            let mut acc = int_init;
            for arg in args {
                match arg {
                    Value::Int(i) => acc = int_op(acc, *i),
                    _ => {
                        return Err(InterpError::with_span(
                            "arithmetic expects numeric arguments",
                            span,
                        ))
                    }
                }
            }
            Ok(Value::Int(acc))
        }
    }

    fn numeric_reduce(
        &self,
        args: &[Value],
        int_op: fn(i64, i64) -> i64,
        float_op: fn(f64, f64) -> f64,
        span: Span,
    ) -> Result<Value, InterpError> {
        let has_float = args.iter().any(|a| matches!(a, Value::Float(_)));

        if has_float {
            let mut iter = args.iter();
            let mut acc = match iter.next().unwrap() {
                Value::Float(f) => *f,
                Value::Int(i) => *i as f64,
                _ => {
                    return Err(InterpError::with_span(
                        "arithmetic expects numeric arguments",
                        span,
                    ))
                }
            };
            for arg in iter {
                match arg {
                    Value::Float(f) => acc = float_op(acc, *f),
                    Value::Int(i) => acc = float_op(acc, *i as f64),
                    _ => {
                        return Err(InterpError::with_span(
                            "arithmetic expects numeric arguments",
                            span,
                        ))
                    }
                }
            }
            Ok(Value::Float(acc))
        } else {
            let mut iter = args.iter();
            let mut acc = match iter.next().unwrap() {
                Value::Int(i) => *i,
                _ => {
                    return Err(InterpError::with_span(
                        "arithmetic expects numeric arguments",
                        span,
                    ))
                }
            };
            for arg in iter {
                match arg {
                    Value::Int(i) => acc = int_op(acc, *i),
                    _ => {
                        return Err(InterpError::with_span(
                            "arithmetic expects numeric arguments",
                            span,
                        ))
                    }
                }
            }
            Ok(Value::Int(acc))
        }
    }

    fn compare_op(
        &self,
        args: &[Value],
        pred: fn(std::cmp::Ordering) -> bool,
        span: Span,
    ) -> Result<Value, InterpError> {
        if args.len() != 2 {
            return Err(InterpError::with_span(
                "comparison requires exactly 2 arguments",
                span,
            ));
        }
        let ord = match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => {
                a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::Int(a), Value::Float(b)) => (*a as f64)
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Float(a), Value::Int(b)) => a
                .partial_cmp(&(*b as f64))
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::String(a), Value::String(b)) => a.cmp(b),
            _ => {
                return Err(InterpError::with_span(
                    "comparison expects comparable arguments",
                    span,
                ))
            }
        };
        Ok(Value::Bool(pred(ord)))
    }
}

// ── Tests ────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn run(source: &str) -> Result<String, InterpError> {
        let expanded = weir_macros::expand(source);
        assert!(
            expanded.errors.is_empty(),
            "macro errors: {:?}",
            expanded.errors
        );
        let (module, errors) = weir_parser::parse(&expanded.source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        interpret(&module)
    }

    fn run_ok(source: &str) -> String {
        run(source).expect("interpreter error")
    }

    #[test]
    fn test_arithmetic() {
        let output = run_ok("(defn main () (println (+ 1 2)))");
        assert_eq!(output, "3\n");
    }

    #[test]
    fn test_arithmetic_ops() {
        let output = run_ok(
            "(defn main ()
               (println (+ 1 2))
               (println (- 10 3))
               (println (* 4 5))
               (println (/ 15 4))
               (println (mod 17 5)))",
        );
        assert_eq!(output, "3\n7\n20\n3\n2\n");
    }

    #[test]
    fn test_float_arithmetic() {
        let output = run_ok(
            "(defn main ()
               (println (+ 1.5 2.5))
               (println (* 3.0 2.0)))",
        );
        assert_eq!(output, "4.0\n6.0\n");
    }

    #[test]
    fn test_nested_arithmetic() {
        let output = run_ok("(defn main () (println (+ (* 2 3) (- 10 4))))");
        assert_eq!(output, "12\n");
    }

    #[test]
    fn test_variadic_arithmetic() {
        let output = run_ok(
            "(defn main ()
               (println (+ 1 2 3 4 5))
               (println (* 1 2 3 4)))",
        );
        assert_eq!(output, "15\n24\n");
    }

    #[test]
    fn test_negation() {
        let output = run_ok("(defn main () (println (- 5)))");
        assert_eq!(output, "-5\n");
    }

    #[test]
    fn test_comparison() {
        let output = run_ok(
            "(defn main ()
               (println (< 1 2))
               (println (> 1 2))
               (println (= 3 3))
               (println (!= 3 4))
               (println (<= 5 5))
               (println (>= 5 6)))",
        );
        assert_eq!(output, "true\nfalse\ntrue\ntrue\ntrue\nfalse\n");
    }

    #[test]
    fn test_boolean_ops() {
        let output = run_ok(
            "(defn main ()
               (println (not true))
               (println (not false))
               (println (and true true))
               (println (and true false))
               (println (or false false))
               (println (or false true)))",
        );
        assert_eq!(output, "false\ntrue\ntrue\nfalse\nfalse\ntrue\n");
    }

    #[test]
    fn test_string_ops() {
        let output = run_ok(
            r#"(defn main ()
               (println (str "hello" " " "world"))
               (println (str "num: " 42)))"#,
        );
        assert_eq!(output, "hello world\nnum: 42\n");
    }

    #[test]
    fn test_let_binding() {
        let output = run_ok(
            "(defn main ()
               (let ((x 10)
                     (y 20))
                 (println (+ x y))))",
        );
        assert_eq!(output, "30\n");
    }

    #[test]
    fn test_let_nested() {
        let output = run_ok(
            "(defn main ()
               (let ((x 5))
                 (let ((y (+ x 3)))
                   (println y))))",
        );
        assert_eq!(output, "8\n");
    }

    #[test]
    fn test_mutable_binding() {
        let output = run_ok(
            "(defn main ()
               (let ((mut x 0))
                 (set! x 42)
                 (println x)))",
        );
        assert_eq!(output, "42\n");
    }

    #[test]
    fn test_immutable_set_error() {
        let result = run("(defn main ()
               (let ((x 0))
                 (set! x 42)))");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .message
            .contains("cannot mutate immutable"));
    }

    #[test]
    fn test_if_expression() {
        let output = run_ok(
            "(defn main ()
               (println (if (> 5 3) \"yes\" \"no\"))
               (println (if (< 5 3) \"yes\" \"no\")))",
        );
        assert_eq!(output, "yes\nno\n");
    }

    #[test]
    fn test_if_no_else() {
        let output = run_ok(
            "(defn main ()
               (if true (println \"hello\"))
               (if false (println \"hidden\"))
               (println \"done\"))",
        );
        assert_eq!(output, "hello\ndone\n");
    }

    #[test]
    fn test_cond() {
        let output = run_ok(
            "(defn main ()
               (let ((x 0))
                 (println (cond
                   ((< x 0) \"negative\")
                   ((= x 0) \"zero\")
                   (else \"positive\")))))",
        );
        assert_eq!(output, "zero\n");
    }

    #[test]
    fn test_when_unless() {
        let output = run_ok(
            "(defn main ()
               (when true (println \"when-true\"))
               (when false (println \"when-false\"))
               (unless false (println \"unless-false\"))
               (unless true (println \"unless-true\")))",
        );
        assert_eq!(output, "when-true\nunless-false\n");
    }

    #[test]
    fn test_function_calls() {
        let output = run_ok(
            "(defn add (x y) (+ x y))
             (defn main () (println (add 3 4)))",
        );
        assert_eq!(output, "7\n");
    }

    #[test]
    fn test_recursion() {
        let output = run_ok(
            "(defn factorial (n)
               (if (<= n 1)
                 1
                 (* n (factorial (- n 1)))))
             (defn main () (println (factorial 10)))",
        );
        assert_eq!(output, "3628800\n");
    }

    #[test]
    fn test_fibonacci() {
        let output = run_ok(
            "(defn fib (n)
               (if (<= n 1)
                 n
                 (+ (fib (- n 1)) (fib (- n 2)))))
             (defn main ()
               (println (fib 0))
               (println (fib 1))
               (println (fib 10)))",
        );
        assert_eq!(output, "0\n1\n55\n");
    }

    #[test]
    fn test_mutual_recursion() {
        let output = run_ok(
            "(defn is-even (n)
               (if (= n 0) true (is-odd (- n 1))))
             (defn is-odd (n)
               (if (= n 0) false (is-even (- n 1))))
             (defn main ()
               (println (is-even 10))
               (println (is-odd 10)))",
        );
        assert_eq!(output, "true\nfalse\n");
    }

    #[test]
    fn test_lambda() {
        let output = run_ok(
            "(defn main ()
               (let ((double (fn (x) (* x 2))))
                 (println (double 21))))",
        );
        assert_eq!(output, "42\n");
    }

    #[test]
    fn test_closure() {
        let output = run_ok(
            "(defn make-adder (n)
               (fn (x) (+ x n)))
             (defn main ()
               (let ((add5 (make-adder 5)))
                 (println (add5 10))
                 (println (add5 20))))",
        );
        assert_eq!(output, "15\n25\n");
    }

    #[test]
    fn test_higher_order() {
        let output = run_ok(
            "(defn apply (f x) (f x))
             (defn double (x) (* x 2))
             (defn main () (println (apply double 21)))",
        );
        assert_eq!(output, "42\n");
    }

    #[test]
    fn test_deftype_and_match() {
        let output = run_ok(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn describe (opt)
               (match opt
                 ((Some val) (str \"got: \" val))
                 (None \"nothing\")))
             (defn main ()
               (println (describe (Some 42)))
               (println (describe None)))",
        );
        assert_eq!(output, "got: 42\nnothing\n");
    }

    #[test]
    fn test_nested_match() {
        let output = run_ok(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn main ()
               (let ((x (Some (Some 5))))
                 (match x
                   ((Some (Some n)) (println n))
                   ((Some None) (println \"inner none\"))
                   (None (println \"none\")))))",
        );
        assert_eq!(output, "5\n");
    }

    #[test]
    fn test_match_literal() {
        let output = run_ok(
            "(defn describe (n)
               (match n
                 (0 \"zero\")
                 (1 \"one\")
                 (_ \"other\")))
             (defn main ()
               (println (describe 0))
               (println (describe 1))
               (println (describe 99)))",
        );
        assert_eq!(output, "zero\none\nother\n");
    }

    #[test]
    fn test_defstruct() {
        let output = run_ok(
            "(defstruct Point
               (x : f32)
               (y : f32))
             (defn main ()
               (let ((p (Point 3.0 4.0)))
                 (println (.x p))
                 (println (.y p))))",
        );
        assert_eq!(output, "3.0\n4.0\n");
    }

    #[test]
    fn test_vector() {
        let output = run_ok(
            "(defn main ()
               (let ((v [1 2 3]))
                 (println (len v))
                 (println (nth v 0))
                 (println (nth v 2))))",
        );
        assert_eq!(output, "3\n1\n3\n");
    }

    #[test]
    fn test_do_block() {
        let output = run_ok(
            "(defn main ()
               (let ((result (do
                 (println \"step 1\")
                 (println \"step 2\")
                 42)))
                 (println result)))",
        );
        assert_eq!(output, "step 1\nstep 2\n42\n");
    }

    #[test]
    fn test_print_no_newline() {
        let output = run_ok(
            "(defn main ()
               (print \"hello \")
               (print \"world\")
               (println))",
        );
        assert_eq!(output, "hello world\n");
    }

    #[test]
    fn test_threading_first_expanded() {
        // After macro expansion, (-> 5 add1 double) becomes (double (add1 5))
        let output = run_ok(
            "(defn add1 (x) (+ x 1))
             (defn double (x) (* x 2))
             (defn main ()
               (println (double (add1 5))))",
        );
        assert_eq!(output, "12\n");
    }

    #[test]
    fn test_threading_with_calls_expanded() {
        // After macro expansion, (-> 10 (+ 5) (* 2)) becomes (* (+ 10 5) 2)
        let output = run_ok(
            "(defn main ()
               (println (* (+ 10 5) 2)))",
        );
        assert_eq!(output, "30\n");
    }

    #[test]
    fn test_counter() {
        let output = run_ok(
            "(defn main ()
               (let ((mut count 0))
                 (set! count (+ count 1))
                 (set! count (+ count 1))
                 (set! count (+ count 1))
                 (println count)))",
        );
        assert_eq!(output, "3\n");
    }

    #[test]
    fn test_no_main_is_ok() {
        let output = run_ok("(defn helper (x) (+ x 1))");
        assert_eq!(output, "");
    }

    #[test]
    fn test_type_of() {
        let output = run_ok(
            "(defn main ()
               (println (type-of 42))
               (println (type-of 3.5))
               (println (type-of true))
               (println (type-of \"hello\"))
               (println (type-of [1 2 3])))",
        );
        assert_eq!(output, "Int\nFloat\nBool\nString\nVector\n");
    }

    // ── Generic functions (Phase 8a) ────────────────────────────

    #[test]
    fn test_generic_identity() {
        let output = run_ok(
            "(defn id (x) x)
             (defn main ()
               (println (id 42))
               (println (id \"hello\")))",
        );
        assert_eq!(output, "42\nhello\n");
    }

    #[test]
    fn test_generic_wrap() {
        let output = run_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defn wrap (x) (Some x))
             (defn main ()
               (println (wrap 42))
               (println (wrap \"hello\")))",
        );
        assert_eq!(output, "(Some 42)\n(Some hello)\n");
    }

    #[test]
    fn test_generic_const() {
        let output = run_ok(
            "(defn const-fn (x y) x)
             (defn main ()
               (println (const-fn 1 \"hello\"))
               (println (const-fn \"world\" 99)))",
        );
        assert_eq!(output, "1\nworld\n");
    }

    // ── Typeclasses (Phase 8c) ────────────────────────────────

    #[test]
    fn test_typeclass_basic() {
        let output = run_ok(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (defn main ()
               (println (== 1 1))
               (println (== 1 2)))",
        );
        assert_eq!(output, "true\nfalse\n");
    }

    #[test]
    fn test_typeclass_show() {
        let output = run_ok(
            "(defclass (Show 'a)
               (show : (Fn ['a] String)))
             (instance (Show i32)
               (defn show ((x : i32)) : String (str x)))
             (defn main ()
               (println (show 42)))",
        );
        assert_eq!(output, "42\n");
    }

    #[test]
    fn test_typeclass_multiple_instances() {
        let output = run_ok(
            "(defclass (Show 'a)
               (show : (Fn ['a] String)))
             (instance (Show i32)
               (defn show ((x : i32)) : String (str \"Int:\" x)))
             (instance (Show Bool)
               (defn show ((x : Bool)) : String (str \"Bool:\" x)))
             (defn main ()
               (println (show 42))
               (println (show true)))",
        );
        assert_eq!(output, "Int:42\nBool:true\n");
    }

    // ── Fixture-based end-to-end tests ───────────────────────────

    fn run_fixture(name: &str) -> String {
        let path = format!(
            "{}/tests/fixtures/{}.weir",
            env!("CARGO_MANIFEST_DIR").replace("/crates/weir-interp", ""),
            name
        );
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
        let expanded = weir_macros::expand(&source);
        assert!(
            expanded.errors.is_empty(),
            "macro errors in {}: {:?}",
            name,
            expanded.errors
        );
        let (module, errors) = weir_parser::parse(&expanded.source);
        assert!(errors.is_empty(), "parse errors in {}: {:?}", name, errors);
        interpret(&module).unwrap_or_else(|e| panic!("runtime error in {}: {}", name, e))
    }

    #[test]
    fn fixture_fibonacci() {
        insta::assert_snapshot!(run_fixture("fibonacci"), @r"
        0
        1
        1
        5
        55
        ");
    }

    #[test]
    fn fixture_factorial() {
        insta::assert_snapshot!(run_fixture("factorial"), @r"
        1
        120
        3628800
        ");
    }

    #[test]
    fn fixture_closures() {
        insta::assert_snapshot!(run_fixture("closures"), @r"
        6
        11
        10
        12
        ");
    }

    #[test]
    fn fixture_data_structures() {
        insta::assert_snapshot!(run_fixture("data-structures"), @r"
        Some(42)
        None
        Some(hello)
        3.0
        4.0
        25.0
        3
        20
        4
        40
        ");
    }

    #[test]
    fn fixture_milestone0_demo() {
        insta::assert_snapshot!(run_fixture("milestone0-demo"), @r"
        === Milestone 0 Demo ===
        1 + 2 = 3
        10 - 3 = 7
        abs(-42) = 42
        max(7, 12) = 12
        5! = 120
        10! = 3628800
        fib(10) = 55
        1+2+3 = 6
        result: 3
        error: division by zero
        Hello World
        Goodbye World
        === Done ===
        ");
    }

    #[test]
    fn fixture_macros() {
        insta::assert_snapshot!(run_fixture("macros"), @r"
        12
        30
        33
        100
        200
        300
        ");
    }

    #[test]
    fn fixture_typeclasses() {
        insta::assert_snapshot!(run_fixture("typeclasses"), @r"
        42
        true
        true
        false
        ");
    }

    #[test]
    fn test_functor_fmap() {
        insta::assert_snapshot!(run_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Functor 'f)
               (fmap : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
             (instance (Functor Option)
               (defn fmap ((f : (Fn ['a] 'b)) (opt : (Option 'a))) : (Option 'b)
                 (match opt
                   ((Some x) (Some (f x)))
                   (None None))))
             (defn main ()
               (println (fmap (fn (x) (+ x 1)) (Some 5)))
               (println (fmap (fn (x) (+ x 10)) None)))"
        ), @r"
        (Some 6)
        None
        ");
    }
}
