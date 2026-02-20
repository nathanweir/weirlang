pub mod tco;

use la_arena::{Arena, Idx};
use smol_str::SmolStr;
pub use weir_lexer::Span;

// ── Compile target ───────────────────────────────────────────────

/// Which platform the compiler is targeting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompileTarget {
    Native,
    Wasm,
}

impl CompileTarget {
    /// The keyword string used in `(target (:native ...) (:wasm ...))` forms.
    pub fn keyword(self) -> &'static str {
        match self {
            CompileTarget::Native => "native",
            CompileTarget::Wasm => "wasm",
        }
    }
}

// ── ID types ──────────────────────────────────────────────────────

pub type ExprId = Idx<Expr>;
pub type TypeExprId = Idx<TypeExpr>;
pub type PatternId = Idx<Pattern>;

// ── Module ────────────────────────────────────────────────────────

/// A parsed source file.
#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<(Item, Span)>,
    pub exprs: Arena<Expr>,
    pub type_exprs: Arena<TypeExpr>,
    pub patterns: Arena<Pattern>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            exprs: Arena::new(),
            type_exprs: Arena::new(),
            patterns: Arena::new(),
        }
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

// ── Top-level items ───────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Item {
    Defn(Defn),
    Deftype(Deftype),
    Defstruct(Defstruct),
    Defclass(Defclass),
    Instance(InstanceDef),
    Import(Import),
    Declare(Declare),
    ExternC(ExternC),
}

/// Function definition: `(defn name (params) : return-type body...)`
#[derive(Debug, Clone)]
pub struct Defn {
    pub name: SmolStr,
    pub name_span: Span,
    pub params: Vec<Param>,
    /// Byte offset of the end of the params list `)`.
    pub params_end: u32,
    pub return_type: Option<TypeExprId>,
    pub body: Vec<ExprId>,
    pub is_pub: bool,
    pub docstring: Option<String>,
    pub span: Span,
}

/// Function parameter.
#[derive(Debug, Clone)]
pub struct Param {
    pub name: SmolStr,
    pub name_span: Span,
    pub type_ann: Option<TypeExprId>,
    pub is_mut: bool,
    pub is_ref: bool,
    pub span: Span,
}

/// Sum type definition: `(deftype (Name 'a) variants...)`
#[derive(Debug, Clone)]
pub struct Deftype {
    pub name: SmolStr,
    pub name_span: Span,
    pub type_params: Vec<SmolStr>,
    pub variants: Vec<Variant>,
    pub is_pub: bool,
    pub span: Span,
}

/// Variant of a sum type.
#[derive(Debug, Clone)]
pub struct Variant {
    pub name: SmolStr,
    pub name_span: Span,
    pub fields: Vec<TypeExprId>,
    pub span: Span,
}

/// Struct definition: `(defstruct Name (field : type)...)`
#[derive(Debug, Clone)]
pub struct Defstruct {
    pub name: SmolStr,
    pub name_span: Span,
    pub type_params: Vec<SmolStr>,
    pub fields: Vec<StructField>,
    pub is_pub: bool,
    pub span: Span,
}

/// Field of a struct.
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: SmolStr,
    pub name_span: Span,
    pub type_ann: TypeExprId,
    pub span: Span,
}

/// Typeclass definition: `(defclass (Name 'a) methods...)`
#[derive(Debug, Clone)]
pub struct Defclass {
    pub name: SmolStr,
    pub name_span: Span,
    pub type_params: Vec<SmolStr>,
    pub superclasses: Vec<Constraint>,
    pub methods: Vec<MethodSig>,
    pub is_pub: bool,
    pub span: Span,
}

/// Method signature in a typeclass.
#[derive(Debug, Clone)]
pub struct MethodSig {
    pub name: SmolStr,
    pub name_span: Span,
    pub type_ann: TypeExprId,
    pub span: Span,
}

/// Typeclass constraint: `(ClassName 'a)`.
#[derive(Debug, Clone)]
pub struct Constraint {
    pub class_name: SmolStr,
    pub type_args: Vec<TypeExprId>,
    pub span: Span,
}

/// Typeclass instance: `(instance (Class Type) methods...)`
#[derive(Debug, Clone)]
pub struct InstanceDef {
    pub class_name: SmolStr,
    pub type_args: Vec<TypeExprId>,
    pub constraints: Vec<Constraint>,
    pub methods: Vec<Defn>,
    pub span: Span,
}

/// Import declaration: `(import module.path ...)`
#[derive(Debug, Clone)]
pub struct Import {
    pub module_path: SmolStr,
    pub kind: ImportKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
    /// `(import m (a b c))`
    Names(Vec<SmolStr>),
    /// `(import m :as alias)`
    Alias(SmolStr),
    /// `(import m :all)`
    All,
}

/// Type declaration: `(declare name type)`
#[derive(Debug, Clone)]
pub struct Declare {
    pub name: SmolStr,
    pub type_ann: TypeExprId,
    pub span: Span,
}

/// Extern C block: `(extern "C" decls...)`
#[derive(Debug, Clone)]
pub struct ExternC {
    pub declarations: Vec<Defn>,
    pub span: Span,
}

// ── Expressions ───────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Literal value.
    Lit(Literal),
    /// Variable reference.
    Var(SmolStr),
    /// Function call: `(f args...)`
    Call { func: ExprId, args: Vec<Arg> },
    /// Let binding: `(let (bindings...) body...)`
    Let {
        bindings: Vec<LetBinding>,
        body: Vec<ExprId>,
    },
    /// If expression: `(if cond then else?)`
    If {
        condition: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    },
    /// Cond expression: `(cond (test expr)... (else expr))`
    Cond {
        clauses: Vec<(ExprId, ExprId)>,
        else_clause: Option<ExprId>,
    },
    /// When: `(when cond body...)`
    When {
        condition: ExprId,
        body: Vec<ExprId>,
    },
    /// Unless: `(unless cond body...)`
    Unless {
        condition: ExprId,
        body: Vec<ExprId>,
    },
    /// Match: `(match expr (pattern body...)...)`
    Match {
        scrutinee: ExprId,
        arms: Vec<MatchArm>,
    },
    /// Lambda: `(fn (params) body...)`
    Lambda {
        params: Vec<Param>,
        return_type: Option<TypeExprId>,
        body: Vec<ExprId>,
    },
    /// Do block: `(do exprs...)`
    Do { body: Vec<ExprId> },
    /// Mutation: `(set! place value)`
    SetBang { place: ExprId, value: ExprId },
    /// Type annotation: `(ann type expr)`
    Ann { type_ann: TypeExprId, expr: ExprId },
    /// Field access: `.field`
    FieldAccess(SmolStr),
    /// Vector literal: `[exprs...]`
    VectorLit(Vec<ExprId>),
    /// Map literal: `{key val ...}`
    MapLit(Vec<(ExprId, ExprId)>),

    /// Unsafe block: `(unsafe exprs...)`
    Unsafe { body: Vec<ExprId> },
    /// Arena block: `(with-arena name body...)`
    WithArena { name: SmolStr, body: Vec<ExprId> },
    /// Try/question operator: `expr?`
    Try(ExprId),

    /// Atomic swap: `(swap! atom func)` — CAS loop
    SwapBang { atom: ExprId, func: ExprId },
    /// Structured concurrency block: `(with-tasks body...)`
    WithTasks { body: Vec<ExprId> },
    /// Spawn a task: `(spawn expr)` — must be inside with-tasks
    Spawn(ExprId),
    /// Platform conditional: `(target (:native expr) (:wasm expr))`
    Target {
        branches: Vec<(SmolStr, ExprId)>,
    },
}

/// A function call argument (positional or named).
#[derive(Debug, Clone)]
pub struct Arg {
    pub name: Option<SmolStr>,
    pub value: ExprId,
    pub span: Span,
}

/// Literal values.
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

/// A binding in a let expression.
#[derive(Debug, Clone)]
pub struct LetBinding {
    pub name: SmolStr,
    pub name_span: Span,
    pub type_ann: Option<TypeExprId>,
    pub value: ExprId,
    pub is_mut: bool,
    pub span: Span,
}

/// A match arm: `(pattern body...)`
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: PatternId,
    pub body: Vec<ExprId>,
    pub span: Span,
}

// ── Patterns ──────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    /// Wildcard: `_`
    Wildcard,
    /// Variable binding (lowercase).
    Var(SmolStr),
    /// Constructor: `Name` or `(Name args...)`
    Constructor { name: SmolStr, args: Vec<PatternId> },
    /// Literal pattern.
    Literal(Literal),
    /// Struct destructuring: `{field1 field2}` or `{field1 name1 field2 name2}`
    StructDestructure { fields: Vec<FieldPattern> },
}

/// Field in a struct destructuring pattern.
#[derive(Debug, Clone)]
pub struct FieldPattern {
    pub field_name: SmolStr,
    pub binding: Option<SmolStr>,
    pub span: Span,
}

// ── Type expressions ──────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct TypeExpr {
    pub kind: TypeExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeExprKind {
    /// Named type: `i32`, `String`, `Enemy`
    Named(SmolStr),
    /// Function type: `(Fn [params] return)`
    Fn {
        params: Vec<TypeExprId>,
        return_type: TypeExprId,
    },
    /// Applied type: `(List 'a)`, `(Map 'k 'v)`
    Applied {
        constructor: TypeExprId,
        args: Vec<TypeExprId>,
    },
    /// Type variable: `'a`, `'b`
    TypeVar(SmolStr),
    /// Constrained type: `(=> (Eq 'a) type)`
    Constrained {
        constraints: Vec<Constraint>,
        inner: TypeExprId,
    },
}

// ── Pretty printer ────────────────────────────────────────────────

pub fn pretty_print(module: &Module) -> String {
    let mut printer = PrettyPrinter {
        module,
        buf: String::new(),
        indent: 0,
    };
    printer.print_module();
    printer.buf
}

struct PrettyPrinter<'a> {
    module: &'a Module,
    buf: String,
    indent: usize,
}

impl<'a> PrettyPrinter<'a> {
    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.buf.push_str("  ");
        }
    }

    fn writeln(&mut self, s: &str) {
        self.write_indent();
        self.buf.push_str(s);
        self.buf.push('\n');
    }

    fn print_module(&mut self) {
        for (item, _span) in &self.module.items {
            self.print_item(item);
        }
    }

    fn print_item(&mut self, item: &Item) {
        match item {
            Item::Defn(d) => self.print_defn(d),
            Item::Deftype(d) => self.print_deftype(d),
            Item::Defstruct(d) => self.print_defstruct(d),
            Item::Defclass(d) => self.print_defclass(d),
            Item::Instance(d) => self.print_instance(d),
            Item::Import(d) => self.print_import(d),
            Item::Declare(d) => self.print_declare(d),
            Item::ExternC(d) => self.print_extern_c(d),
        }
    }

    fn print_defn(&mut self, d: &Defn) {
        let pub_str = if d.is_pub { " pub" } else { "" };
        self.writeln(&format!("(defn{} {}", pub_str, d.name));
        self.indent += 1;
        if let Some(ref doc) = d.docstring {
            self.writeln(&format!("doc: {:?}", doc));
        }
        self.writeln("params:");
        self.indent += 1;
        for p in &d.params {
            self.print_param(p);
        }
        self.indent -= 1;
        if let Some(ret) = d.return_type {
            self.write_indent();
            self.buf.push_str("returns: ");
            self.print_type_expr(ret);
            self.buf.push('\n');
        }
        self.writeln("body:");
        self.indent += 1;
        for &e in &d.body {
            self.write_indent();
            self.print_expr(e);
            self.buf.push('\n');
        }
        self.indent -= 1;
        self.indent -= 1;
        self.writeln(")");
    }

    fn print_param(&mut self, p: &Param) {
        self.write_indent();
        let mut mods = String::new();
        if p.is_mut {
            mods.push_str("mut ");
        }
        if p.is_ref {
            mods.push_str("ref ");
        }
        self.buf.push_str(&format!("{}{}", mods, p.name));
        if let Some(t) = p.type_ann {
            self.buf.push_str(" : ");
            self.print_type_expr(t);
        }
        self.buf.push('\n');
    }

    fn print_deftype(&mut self, d: &Deftype) {
        let pub_str = if d.is_pub { " pub" } else { "" };
        let params = if d.type_params.is_empty() {
            String::new()
        } else {
            format!(
                " {}",
                d.type_params
                    .iter()
                    .map(|p| format!("'{}", p))
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        };
        self.writeln(&format!("(deftype{} {}{}", pub_str, d.name, params));
        self.indent += 1;
        for v in &d.variants {
            self.write_indent();
            self.buf.push_str(v.name.as_str());
            for &f in &v.fields {
                self.buf.push(' ');
                self.print_type_expr(f);
            }
            self.buf.push('\n');
        }
        self.indent -= 1;
        self.writeln(")");
    }

    fn print_defstruct(&mut self, d: &Defstruct) {
        let pub_str = if d.is_pub { " pub" } else { "" };
        self.writeln(&format!("(defstruct{} {}", pub_str, d.name));
        self.indent += 1;
        for f in &d.fields {
            self.write_indent();
            self.buf.push_str(&format!("{} : ", f.name));
            self.print_type_expr(f.type_ann);
            self.buf.push('\n');
        }
        self.indent -= 1;
        self.writeln(")");
    }

    fn print_defclass(&mut self, d: &Defclass) {
        let pub_str = if d.is_pub { " pub" } else { "" };
        self.writeln(&format!("(defclass{} {}", pub_str, d.name));
        self.indent += 1;
        if !d.superclasses.is_empty() {
            self.writeln("superclasses:");
            self.indent += 1;
            for c in &d.superclasses {
                self.print_constraint(c);
            }
            self.indent -= 1;
        }
        if !d.type_params.is_empty() {
            self.writeln(&format!(
                "type-params: {}",
                d.type_params
                    .iter()
                    .map(|p| format!("'{}", p))
                    .collect::<Vec<_>>()
                    .join(" ")
            ));
        }
        self.writeln("methods:");
        self.indent += 1;
        for m in &d.methods {
            self.write_indent();
            self.buf.push_str(&format!("{} : ", m.name));
            self.print_type_expr(m.type_ann);
            self.buf.push('\n');
        }
        self.indent -= 1;
        self.indent -= 1;
        self.writeln(")");
    }

    fn print_constraint(&mut self, c: &Constraint) {
        self.write_indent();
        self.buf.push_str(&format!("({}", c.class_name));
        for &t in &c.type_args {
            self.buf.push(' ');
            self.print_type_expr(t);
        }
        self.buf.push_str(")\n");
    }

    fn print_instance(&mut self, d: &InstanceDef) {
        self.write_indent();
        self.buf.push_str(&format!("(instance {}", d.class_name));
        for &t in &d.type_args {
            self.buf.push(' ');
            self.print_type_expr(t);
        }
        self.buf.push('\n');
        self.indent += 1;
        if !d.constraints.is_empty() {
            self.writeln("constraints:");
            self.indent += 1;
            for c in &d.constraints {
                self.print_constraint(c);
            }
            self.indent -= 1;
        }
        for m in &d.methods {
            self.print_defn(m);
        }
        self.indent -= 1;
        self.writeln(")");
    }

    fn print_import(&mut self, d: &Import) {
        self.write_indent();
        match &d.kind {
            ImportKind::Names(names) => {
                self.buf.push_str(&format!(
                    "(import {} ({}))\n",
                    d.module_path,
                    names
                        .iter()
                        .map(|n| n.as_str())
                        .collect::<Vec<_>>()
                        .join(" ")
                ));
            }
            ImportKind::Alias(alias) => {
                self.buf
                    .push_str(&format!("(import {} :as {})\n", d.module_path, alias));
            }
            ImportKind::All => {
                self.buf
                    .push_str(&format!("(import {} :all)\n", d.module_path));
            }
        }
    }

    fn print_declare(&mut self, d: &Declare) {
        self.write_indent();
        self.buf.push_str(&format!("(declare {} ", d.name));
        self.print_type_expr(d.type_ann);
        self.buf.push_str(")\n");
    }

    fn print_extern_c(&mut self, d: &ExternC) {
        self.writeln("(extern \"C\"");
        self.indent += 1;
        for decl in &d.declarations {
            self.print_defn(decl);
        }
        self.indent -= 1;
        self.writeln(")");
    }

    fn print_expr(&mut self, id: ExprId) {
        let expr = &self.module.exprs[id];
        match &expr.kind {
            ExprKind::Lit(lit) => self.print_literal(lit),
            ExprKind::Var(name) => self.buf.push_str(name),
            ExprKind::Call { func, args } => {
                self.buf.push('(');
                self.print_expr(*func);
                for arg in args {
                    self.buf.push(' ');
                    if let Some(ref name) = arg.name {
                        self.buf.push(':');
                        self.buf.push_str(name);
                        self.buf.push(' ');
                    }
                    self.print_expr(arg.value);
                }
                self.buf.push(')');
            }
            ExprKind::Let { bindings, body } => {
                self.buf.push_str("(let");
                self.indent += 1;
                self.buf.push('\n');
                self.writeln("bindings:");
                self.indent += 1;
                for b in bindings {
                    self.write_indent();
                    if b.is_mut {
                        self.buf.push_str("mut ");
                    }
                    self.buf.push_str(b.name.as_str());
                    if let Some(t) = b.type_ann {
                        self.buf.push_str(" : ");
                        self.print_type_expr(t);
                    }
                    self.buf.push_str(" = ");
                    self.print_expr(b.value);
                    self.buf.push('\n');
                }
                self.indent -= 1;
                self.writeln("body:");
                self.indent += 1;
                for &e in body {
                    self.write_indent();
                    self.print_expr(e);
                    self.buf.push('\n');
                }
                self.indent -= 1;
                self.indent -= 1;
                self.write_indent();
                self.buf.push(')');
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.buf.push_str("(if ");
                self.print_expr(*condition);
                self.buf.push(' ');
                self.print_expr(*then_branch);
                if let Some(e) = else_branch {
                    self.buf.push(' ');
                    self.print_expr(*e);
                }
                self.buf.push(')');
            }
            ExprKind::Cond {
                clauses,
                else_clause,
            } => {
                self.buf.push_str("(cond");
                for (test, body) in clauses {
                    self.buf.push_str(" (");
                    self.print_expr(*test);
                    self.buf.push(' ');
                    self.print_expr(*body);
                    self.buf.push(')');
                }
                if let Some(e) = else_clause {
                    self.buf.push_str(" (else ");
                    self.print_expr(*e);
                    self.buf.push(')');
                }
                self.buf.push(')');
            }
            ExprKind::When { condition, body } => {
                self.buf.push_str("(when ");
                self.print_expr(*condition);
                for &e in body {
                    self.buf.push(' ');
                    self.print_expr(e);
                }
                self.buf.push(')');
            }
            ExprKind::Unless { condition, body } => {
                self.buf.push_str("(unless ");
                self.print_expr(*condition);
                for &e in body {
                    self.buf.push(' ');
                    self.print_expr(e);
                }
                self.buf.push(')');
            }
            ExprKind::Match { scrutinee, arms } => {
                self.buf.push_str("(match ");
                self.print_expr(*scrutinee);
                self.indent += 1;
                for arm in arms {
                    self.buf.push('\n');
                    self.write_indent();
                    self.buf.push('(');
                    self.print_pattern(arm.pattern);
                    for &e in &arm.body {
                        self.buf.push(' ');
                        self.print_expr(e);
                    }
                    self.buf.push(')');
                }
                self.indent -= 1;
                self.buf.push(')');
            }
            ExprKind::Lambda {
                params,
                return_type,
                body,
            } => {
                self.buf.push_str("(fn (");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.buf.push(' ');
                    }
                    self.buf.push_str(p.name.as_str());
                    if let Some(t) = p.type_ann {
                        self.buf.push_str(" : ");
                        self.print_type_expr(t);
                    }
                }
                self.buf.push(')');
                if let Some(ret) = return_type {
                    self.buf.push_str(" : ");
                    self.print_type_expr(*ret);
                }
                for &e in body {
                    self.buf.push(' ');
                    self.print_expr(e);
                }
                self.buf.push(')');
            }
            ExprKind::Do { body } => {
                self.buf.push_str("(do");
                for &e in body {
                    self.buf.push(' ');
                    self.print_expr(e);
                }
                self.buf.push(')');
            }
            ExprKind::SetBang { place, value } => {
                self.buf.push_str("(set! ");
                self.print_expr(*place);
                self.buf.push(' ');
                self.print_expr(*value);
                self.buf.push(')');
            }
            ExprKind::Ann { type_ann, expr } => {
                self.buf.push_str("(ann ");
                self.print_type_expr(*type_ann);
                self.buf.push(' ');
                self.print_expr(*expr);
                self.buf.push(')');
            }
            ExprKind::FieldAccess(name) => {
                self.buf.push('.');
                self.buf.push_str(name);
            }
            ExprKind::VectorLit(elems) => {
                self.buf.push('[');
                for (i, &e) in elems.iter().enumerate() {
                    if i > 0 {
                        self.buf.push(' ');
                    }
                    self.print_expr(e);
                }
                self.buf.push(']');
            }
            ExprKind::MapLit(pairs) => {
                self.buf.push('{');
                for (i, (k, v)) in pairs.iter().enumerate() {
                    if i > 0 {
                        self.buf.push(' ');
                    }
                    self.print_expr(*k);
                    self.buf.push(' ');
                    self.print_expr(*v);
                }
                self.buf.push('}');
            }

            ExprKind::Unsafe { body } => {
                self.buf.push_str("(unsafe");
                for &e in body {
                    self.buf.push(' ');
                    self.print_expr(e);
                }
                self.buf.push(')');
            }
            ExprKind::WithArena { name, body } => {
                self.buf.push_str("(with-arena ");
                self.buf.push_str(name);
                for &e in body {
                    self.buf.push(' ');
                    self.print_expr(e);
                }
                self.buf.push(')');
            }
            ExprKind::Try(inner) => {
                self.print_expr(*inner);
                self.buf.push('?');
            }

            ExprKind::SwapBang { atom, func } => {
                self.buf.push_str("(swap! ");
                self.print_expr(*atom);
                self.buf.push(' ');
                self.print_expr(*func);
                self.buf.push(')');
            }
            ExprKind::WithTasks { body } => {
                self.buf.push_str("(with-tasks");
                for &e in body {
                    self.buf.push(' ');
                    self.print_expr(e);
                }
                self.buf.push(')');
            }
            ExprKind::Spawn(inner) => {
                self.buf.push_str("(spawn ");
                self.print_expr(*inner);
                self.buf.push(')');
            }
            ExprKind::Target { branches } => {
                self.buf.push_str("(target");
                for (kw, expr) in branches {
                    self.buf.push_str(" (:");
                    self.buf.push_str(kw);
                    self.buf.push(' ');
                    self.print_expr(*expr);
                    self.buf.push(')');
                }
                self.buf.push(')');
            }
        }
    }

    fn print_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Int(n) => self.buf.push_str(&n.to_string()),
            Literal::Float(f) => self.buf.push_str(&format!("{:?}", f)),
            Literal::String(s) => self.buf.push_str(&format!("{:?}", s)),
            Literal::Bool(b) => self.buf.push_str(if *b { "true" } else { "false" }),
        }
    }

    fn print_pattern(&mut self, id: PatternId) {
        let pat = &self.module.patterns[id];
        match &pat.kind {
            PatternKind::Wildcard => self.buf.push('_'),
            PatternKind::Var(name) => self.buf.push_str(name),
            PatternKind::Constructor { name, args } => {
                if args.is_empty() {
                    self.buf.push_str(name);
                } else {
                    self.buf.push('(');
                    self.buf.push_str(name);
                    for &a in args {
                        self.buf.push(' ');
                        self.print_pattern(a);
                    }
                    self.buf.push(')');
                }
            }
            PatternKind::Literal(lit) => self.print_literal(lit),
            PatternKind::StructDestructure { fields } => {
                self.buf.push('{');
                for (i, f) in fields.iter().enumerate() {
                    if i > 0 {
                        self.buf.push(' ');
                    }
                    self.buf.push_str(f.field_name.as_str());
                    if let Some(ref b) = f.binding {
                        self.buf.push(' ');
                        self.buf.push_str(b);
                    }
                }
                self.buf.push('}');
            }
        }
    }

    fn print_type_expr(&mut self, id: TypeExprId) {
        let ty = &self.module.type_exprs[id];
        match &ty.kind {
            TypeExprKind::Named(name) => self.buf.push_str(name),
            TypeExprKind::Fn {
                params,
                return_type,
            } => {
                self.buf.push_str("(Fn [");
                for (i, &p) in params.iter().enumerate() {
                    if i > 0 {
                        self.buf.push(' ');
                    }
                    self.print_type_expr(p);
                }
                self.buf.push_str("] ");
                self.print_type_expr(*return_type);
                self.buf.push(')');
            }
            TypeExprKind::Applied { constructor, args } => {
                self.buf.push('(');
                self.print_type_expr(*constructor);
                for &a in args {
                    self.buf.push(' ');
                    self.print_type_expr(a);
                }
                self.buf.push(')');
            }
            TypeExprKind::TypeVar(name) => {
                self.buf.push('\'');
                self.buf.push_str(name);
            }
            TypeExprKind::Constrained { constraints, inner } => {
                self.buf.push_str("(=> ");
                for c in constraints {
                    self.buf.push('(');
                    self.buf.push_str(c.class_name.as_str());
                    for &t in &c.type_args {
                        self.buf.push(' ');
                        self.print_type_expr(t);
                    }
                    self.buf.push_str(") ");
                }
                self.print_type_expr(*inner);
                self.buf.push(')');
            }
        }
    }
}
