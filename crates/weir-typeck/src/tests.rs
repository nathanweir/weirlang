use super::*;

    fn check_src(source: &str) -> Vec<TypeError> {
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);
        let result = check(&module);
        result.errors
    }

    fn check_ok(source: &str) {
        let errors = check_src(source);
        assert!(errors.is_empty(), "unexpected type errors: {:?}", errors);
    }

    fn check_err(source: &str) -> String {
        let errors = check_src(source);
        assert!(!errors.is_empty(), "expected type errors, got none");
        errors
            .iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
            .join("\n")
    }

    // ── Passing programs ─────────────────────────────────────────

    #[test]
    fn basic_arithmetic() {
        check_ok("(defn main () (+ 1 2))");
    }

    #[test]
    fn annotated_function() {
        check_ok("(defn add ((x : i32) (y : i32)) : i32 (+ x y))");
    }

    #[test]
    fn annotated_return_unit() {
        check_ok("(defn greet () : Unit (println \"hello\"))");
    }

    #[test]
    fn let_binding_inferred() {
        check_ok(
            "(defn main ()
               (let ((x 5) (y 10))
                 (+ x y)))",
        );
    }

    #[test]
    fn let_binding_annotated() {
        check_ok(
            "(defn main ()
               (let (((x : i32) 5))
                 (+ x 1)))",
        );
    }

    #[test]
    fn if_expression_types_match() {
        check_ok(
            "(defn main ()
               (if (> 1 0) 42 0))",
        );
    }

    #[test]
    fn cond_expression() {
        check_ok(
            "(defn main ()
               (cond
                 ((> 1 0) 42)
                 (else 0)))",
        );
    }

    #[test]
    fn function_call_types() {
        check_ok(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () (add 1 2))",
        );
    }

    #[test]
    fn recursion() {
        check_ok(
            "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))",
        );
    }

    #[test]
    fn lambda() {
        check_ok(
            "(defn main ()
               (let ((f (fn (x) (+ x 1))))
                 (f 10)))",
        );
    }

    #[test]
    fn closures() {
        check_ok(
            "(defn make-adder ((n : i32)) : (Fn [i32] i32)
               (fn (x) (+ x n)))",
        );
    }

    #[test]
    fn deftype_and_match() {
        check_ok(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn unwrap ((x : (Option i32))) : i32
               (match x
                 ((Some v) v)
                 (None 0)))",
        );
    }

    #[test]
    fn defstruct_and_field_access() {
        check_ok(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn main ()
               (let ((p (Point 1.0 2.0)))
                 (.x p)))",
        );
    }

    #[test]
    fn defstruct_destructure() {
        check_ok(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn get-x ((p : Point)) : f64
               (match p
                 ({:x} x)))",
        );
    }

    #[test]
    fn defstruct_destructure_multi_field() {
        check_ok(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn sum ((p : Point)) : f64
               (match p
                 ({:x :y} (+ x y))))",
        );
    }

    #[test]
    fn defstruct_destructure_renamed() {
        check_ok(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn sum ((p : Point)) : f64
               (match p
                 ({:x px :y py} (+ px py))))",
        );
    }

    #[test]
    fn defstruct_operations() {
        check_ok(
            "(defstruct Vec2
               (x : f64)
               (y : f64))
             (defn v-add ((a : Vec2) (b : Vec2)) : Vec2
               (Vec2 (+ (.x a) (.x b)) (+ (.y a) (.y b))))
             (defn main ()
               (let ((c (v-add (Vec2 1.0 0.0) (Vec2 0.0 1.0))))
                 (.x c)))",
        );
    }

    #[test]
    fn vector_literal() {
        check_ok("(defn main () [1 2 3])");
    }

    #[test]
    fn numeric_defaults_i32() {
        // 42 defaults to i32; function expects i32 — should work
        check_ok(
            "(defn id ((x : i32)) : i32 x)
             (defn main () (id 42))",
        );
    }

    #[test]
    fn numeric_defaults_f64() {
        check_ok(
            "(defn id ((x : f64)) : f64 x)
             (defn main () (id 3.5))",
        );
    }

    #[test]
    fn numeric_promotion_i64() {
        // Integer literal should unify with i64 when used in that context
        check_ok(
            "(defn id ((x : i64)) : i64 x)
             (defn main () (id 42))",
        );
    }

    #[test]
    fn match_with_wildcard() {
        check_ok(
            "(defn classify ((n : i32)) : String
               (match n
                 (0 \"zero\")
                 (_ \"other\")))",
        );
    }

    #[test]
    fn when_unless() {
        check_ok(
            "(defn main ()
               (when (> 1 0) (println \"yes\"))
               (unless (> 1 0) (println \"no\")))",
        );
    }

    #[test]
    fn do_block() {
        check_ok(
            "(defn main () : i32
               (do (println \"hello\") 42))",
        );
    }

    #[test]
    fn set_bang() {
        check_ok(
            "(defn main ()
               (let ((mut x 0))
                 (set! x 5)))",
        );
    }

    #[test]
    fn mutual_recursion() {
        check_ok(
            "(defn is-even ((n : i32)) : Bool
               (if (= n 0) true (is-odd (- n 1))))
             (defn is-odd ((n : i32)) : Bool
               (if (= n 0) false (is-even (- n 1))))",
        );
    }

    #[test]
    fn polymorphic_function_multiple_calls() {
        // A polymorphic function can be called with different types
        check_ok(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn main ()
               (let ((a (Some 42))
                     (b (Some \"hello\")))
                 (println a)
                 (println b)))",
        );
    }

    // ── Generic functions (Phase 8a) ────────────────────────────

    #[test]
    fn generic_identity() {
        check_ok(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () (let ((a (id 42)) (b (id \"hello\"))) a))",
        );
    }

    #[test]
    fn generic_function_calling_generic_constructor() {
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defn wrap ((x : 'a)) : (Option 'a) (Some x))
             (defn main () (wrap 42))",
        );
    }

    #[test]
    fn generic_multiple_type_params() {
        check_ok(
            "(defn const ((x : 'a) (y : 'b)) : 'a x)
             (defn main () (const 1 \"hello\"))",
        );
    }

    #[test]
    fn error_type_mismatch_through_generic() {
        let msg = check_err(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () : i32 (id \"hello\"))",
        );
        assert!(
            msg.contains("type mismatch"),
            "expected type mismatch, got: {}",
            msg
        );
    }

    // ── Failing programs (type errors) ───────────────────────────

    #[test]
    fn error_return_type_mismatch() {
        let msg = check_err("(defn bad () : i32 (str \"hello\"))");
        insta::assert_snapshot!(msg, @"type mismatch: expected String, got i32");
    }

    #[test]
    fn error_arg_type_mismatch() {
        let msg = check_err(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () (add 1 \"two\"))",
        );
        insta::assert_snapshot!(msg, @"type mismatch: expected i32, got String");
    }

    #[test]
    fn error_wrong_arg_count() {
        let msg = check_err(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () (add 1))",
        );
        insta::assert_snapshot!(msg, @"function 'add' expects 2 arguments, got 1");
    }

    #[test]
    fn error_undefined_variable() {
        let msg = check_err("(defn main () (+ x 1))");
        insta::assert_snapshot!(msg, @"undefined variable 'x'");
    }

    #[test]
    fn error_if_branch_mismatch() {
        let msg = check_err(
            "(defn main ()
               (if true 42 \"hello\"))",
        );
        insta::assert_snapshot!(msg, @"type mismatch: expected integer type, got String");
    }

    #[test]
    fn error_non_exhaustive_match() {
        let msg = check_err(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn main ()
               (match (Some 1)
                 ((Some x) x)))",
        );
        insta::assert_snapshot!(msg, @"non-exhaustive match: missing variants: None");
    }

    #[test]
    fn error_wrong_constructor_arity() {
        let msg = check_err(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn main () (Some 1 2))",
        );
        insta::assert_snapshot!(msg, @"constructor Some expects 1 arguments, got 2");
    }

    #[test]
    fn error_struct_wrong_field_count() {
        let msg = check_err(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn main () (Point 1.0))",
        );
        insta::assert_snapshot!(msg, @"struct Point has 2 fields, got 1 arguments");
    }

    #[test]
    fn error_struct_unknown_field() {
        let msg = check_err(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn main ()
               (let ((p (Point 1.0 2.0)))
                 (.z p)))",
        );
        insta::assert_snapshot!(msg, @"struct Point has no field 'z'");
    }

    #[test]
    fn error_not_requires_bool() {
        let msg = check_err("(defn main () (not 42))");
        insta::assert_snapshot!(msg, @"type mismatch: expected integer type, got Bool");
    }

    #[test]
    fn error_if_condition_not_bool() {
        let msg = check_err("(defn main () (if 42 1 2))");
        insta::assert_snapshot!(msg, @"type mismatch: expected integer type, got Bool");
    }

    #[test]
    fn error_float_literal_as_integer() {
        let msg = check_err(
            "(defn id ((x : i32)) : i32 x)
             (defn main () (id 3.5))",
        );
        insta::assert_snapshot!(msg, @"type mismatch: expected float type, got i32");
    }

    // ── Typeclasses (Phase 8b) ──────────────────────────────────

    #[test]
    fn typeclass_basic_instance() {
        check_ok(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (defn main () (== 1 2))",
        );
    }

    #[test]
    fn typeclass_missing_instance() {
        let msg = check_err(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (defn main () (== 1 2))",
        );
        assert!(
            msg.contains("no instance of Eq"),
            "expected 'no instance', got: {}",
            msg
        );
    }

    #[test]
    fn typeclass_constrained_function() {
        check_ok(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (declare equal? (=> (Eq 'a) (Fn ['a 'a] Bool)))
             (defn equal? ((x : 'a) (y : 'a)) : Bool (== x y))
             (defn main () (equal? 1 2))",
        );
    }

    #[test]
    fn typeclass_multiple_methods() {
        check_ok(
            "(defclass (Show 'a)
               (show : (Fn ['a] String)))
             (instance (Show i32)
               (defn show ((x : i32)) : String (str x)))
             (defn main () (show 42))",
        );
    }

    #[test]
    fn error_duplicate_instance() {
        let msg = check_err(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (defn main () (== 1 2))",
        );
        assert!(
            msg.contains("duplicate instance"),
            "expected 'duplicate instance', got: {}",
            msg
        );
    }

    #[test]
    fn error_unknown_typeclass() {
        let msg = check_err(
            "(instance (Foo i32)
               (defn bar ((x : i32)) : i32 x))
             (defn main () 42)",
        );
        assert!(
            msg.contains("unknown typeclass"),
            "expected 'unknown typeclass', got: {}",
            msg
        );
    }

    // ── Ord typeclass ─────────────────────────────────────────────

    #[test]
    fn test_ord_typeclass_resolves() {
        check_ok(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (instance (Ord i32)
               (defn compare ((x : i32) (y : i32)) : Ordering
                 (if (< x y) LT (if (> x y) GT EQ))))
             (defn min-of ((x : i32) (y : i32)) : i32
               (match (compare x y)
                 (LT x) (EQ x) (GT y)))
             (defn max-of ((x : i32) (y : i32)) : i32
               (match (compare x y)
                 (GT x) (_ y)))
             (defn main ()
               (println (min-of 10 3))
               (println (max-of 10 3)))",
        );
    }

    // ── Kind system + HKTs (Phase 8d) ────────────────────────────

    #[test]
    fn kind_functor_class() {
        // Functor uses 'f as a type constructor (* -> *), applied as ('f 'a)
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Functor 'f)
               (fmap : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
             (instance (Functor Option)
               (defn fmap ((f : (Fn ['a] 'b)) (opt : (Option 'a))) : (Option 'b)
                 (match opt
                   ((Some x) (Some (f x)))
                   (None None))))
             (defn main () (fmap (fn (x) (+ x 1)) (Some 5)))",
        );
    }

    #[test]
    fn error_kind_mismatch_star_for_hkt() {
        // i32 has kind *, but Functor expects * -> *
        let msg = check_err(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Functor 'f)
               (fmap : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
             (instance (Functor i32)
               (defn fmap ((f : (Fn ['a] 'b)) (x : i32)) : i32 x))
             (defn main () 42)",
        );
        assert!(
            msg.contains("kind mismatch"),
            "expected 'kind mismatch', got: {}",
            msg
        );
    }

    #[test]
    fn kind_type_constructor_registration() {
        // Option has kind * -> * (1 type param)
        // Using it bare in an instance that expects * -> * should work
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Wrapper 'f)
               (wrap : (Fn ['a] ('f 'a))))
             (instance (Wrapper Option)
               (defn wrap ((x : 'a)) : (Option 'a) (Some x)))
             (defn main () (wrap 42))",
        );
    }

    #[test]
    fn hkt_type_application_unification() {
        // HKT type application should unify correctly:
        // ('f 'a) with (Option i32) should bind 'f=Option, 'a=i32
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Functor 'f)
               (fmap : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
             (instance (Functor Option)
               (defn fmap ((f : (Fn ['a] 'b)) (opt : (Option 'a))) : (Option 'b)
                 (match opt
                   ((Some x) (Some (f x)))
                   (None None))))
             (defn main () : (Option i32)
               (fmap (fn (x) (+ x 1)) (Some 5)))",
        );
    }

    // ── External symbol awareness ────────────────────────────────

    fn check_with_ext(source: &str, externals: &[&str]) -> Vec<TypeError> {
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);
        let ext: HashSet<SmolStr> = externals.iter().map(|s| SmolStr::from(*s)).collect();
        let result = check_with_externals(&module, &ext);
        result.errors
    }

    #[test]
    fn external_variable_no_error() {
        // Calling an external function should not produce "undefined variable"
        let errors = check_with_ext("(defn main () (square 5))", &["square"]);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn unknown_variable_still_errors_without_external() {
        // Without declaring it external, the error should remain
        let errors = check_with_ext("(defn main () (square 5))", &[]);
        assert!(!errors.is_empty());
        assert!(errors[0].message.contains("undefined variable 'square'"));
    }

    #[test]
    fn external_constructor_in_pattern_no_error() {
        // Matching on an external constructor should not error
        let errors = check_with_ext(
            "(defn main ((x : i32))
               (match x
                 ((ExternalCon y) y)
                 (_ 0)))",
            &["ExternalCon"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn external_constructor_in_expr_no_error() {
        // Using an external constructor as a value should not error
        let errors = check_with_ext("(defn main () (Some 42))", &["Some"]);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn mixed_local_and_external() {
        // Local definitions coexist with external names
        let errors = check_with_ext(
            "(defn helper () 42)
             (defn main () (+ (helper) (remote-fn 10)))",
            &["remote-fn"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    // ── Orphan instance coherence ─────────────────────────────────

    #[test]
    fn orphan_instance_class_local_ok() {
        // Class defined locally, type is external → allowed (class owner)
        let errors = check_with_ext(
            "(defclass (MyClass 'a)
               (my-method : (Fn ['a] i32)))
             (instance (MyClass ExternalType)
               (defn my-method ((x : ExternalType)) : i32 42))
             (defn main () 0)",
            &["ExternalType"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn orphan_instance_type_local_ok() {
        // Class is external, type defined locally → allowed (type owner)
        let errors = check_with_ext(
            "(deftype Color Red Green Blue)
             (defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show Color)
               (defn show ((c : Color)) : i32 0))
             (defn main () 0)",
            &["Show"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn orphan_instance_primitive_ok() {
        // Class is external, type is i32 (primitive) → allowed
        let errors = check_with_ext(
            "(defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show i32)
               (defn show ((x : i32)) : i32 x))
             (defn main () 0)",
            &["Show"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn orphan_instance_rejected() {
        // Class is external, type is external → orphan
        let errors = check_with_ext(
            "(defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show ExternalType)
               (defn show ((x : ExternalType)) : i32 42))
             (defn main () 0)",
            &["Show", "ExternalType"],
        );
        assert!(!errors.is_empty());
        assert!(
            errors[0].message.contains("orphan instance"),
            "expected 'orphan instance', got: {}",
            errors[0].message
        );
    }

    #[test]
    fn orphan_rule_single_file_no_check() {
        // No externals → single-file mode → always passes
        let errors = check_with_ext(
            "(defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show i32)
               (defn show ((x : i32)) : i32 x))
             (defn main () 0)",
            &[],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    // ── From typeclass ────────────────────────────────────────────

    #[test]
    fn from_typeclass_resolves() {
        check_ok(
            "(deftype Color Red Green Blue)
             (deftype Primary Yes No)
             (defclass (From 'a 'b)
               (from : (Fn ['a] 'b)))
             (instance (From Color Primary)
               (defn from ((c : Color)) : Primary
                 (match c
                   (Red Yes)
                   (Blue Yes)
                   (_ No))))
             (defn main ()
               (let (((r : Primary) (from Red)))
                 (println r)))",
        );
    }

    #[test]
    fn from_typeclass_missing_instance_error() {
        let msg = check_err(
            "(deftype Color Red Green Blue)
             (deftype Primary Yes No)
             (defclass (From 'a 'b)
               (from : (Fn ['a] 'b)))
             (defn main ()
               (let (((r : Primary) (from Red)))
                 (println r)))",
        );
        assert!(
            msg.contains("no instance"),
            "expected 'no instance' error, got: {}",
            msg
        );
    }

    // ── Result type + ? operator tests ──────────────────────────

    #[test]
    fn result_type_basic() {
        check_ok(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn wrap ((x : i32)) : (Result i32 i32) (Ok x))
             (defn unwrap ((r : (Result i32 i32))) : i32
               (match r
                 ((Ok val) val)
                 ((Err _) 0)))
             (defn main () (println (unwrap (wrap 42))))",
        );
    }

    #[test]
    fn try_operator_unwraps_ok() {
        check_ok(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn safe-div ((x : i32) (y : i32)) : (Result i32 i32)
               (if (= y 0) (Err y) (Ok (/ x y))))
             (defn use-try ((x : i32) (y : i32)) : (Result i32 i32)
               (let ((result (safe-div x y)?))
                 (Ok (+ result 1))))
             (defn main () (println 0))",
        );
    }

    #[test]
    fn try_operator_error_on_non_result() {
        let msg = check_err(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn id ((x : i32)) : i32 x)
             (defn bad ((x : i32)) : (Result i32 i32) (let ((y (id x)?)) (Ok y)))
             (defn main () (println 0))",
        );
        assert!(
            msg.contains("? operator requires a Result type"),
            "expected '? operator requires a Result type' error, got: {}",
            msg
        );
    }

    #[test]
    fn try_operator_error_type_must_match() {
        let msg = check_err(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn f () : (Result i32 Bool) (Ok 42))
             (defn g () : (Result i32 i32) (let ((x (f)?)) (Ok x)))
             (defn main () (println 0))",
        );
        assert!(
            msg.contains("type mismatch"),
            "expected type mismatch error, got: {}",
            msg
        );
    }

    // ── Type-error fixture tests ────────────────────────────────

    fn fixture_check_err(name: &str) -> String {
        let path = format!(
            "{}/tests/fixtures/type-errors/{}.weir",
            env!("CARGO_MANIFEST_DIR").replace("/crates/weir-typeck", ""),
            name
        );
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
        check_err(&source)
    }

    #[test]
    fn fixture_type_error_mismatched_return() {
        let msg = fixture_check_err("mismatched-return");
        assert!(
            msg.contains("type mismatch"),
            "expected type mismatch error, got: {}",
            msg
        );
    }

    #[test]
    fn fixture_type_error_non_exhaustive_match() {
        let msg = fixture_check_err("non-exhaustive-match");
        assert!(
            msg.contains("non-exhaustive"),
            "expected non-exhaustive match error, got: {}",
            msg
        );
    }

    #[test]
    fn fixture_type_error_unknown_variable() {
        let msg = fixture_check_err("unknown-variable");
        assert!(
            msg.contains("undefined variable"),
            "expected undefined variable error, got: {}",
            msg
        );
    }

    #[test]
    fn test_comparison_without_ord_still_works() {
        // Without an Ord class in scope, < still works on primitives (no constraint deferred)
        check_ok(
            "(defn main () : Bool (< 1 2))",
        );
    }

    #[test]
    fn test_comparison_with_ord_defers_constraint() {
        // With Ord in scope, < on i32 defers and resolves the constraint
        check_ok(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (instance (Ord i32)
               (defn compare ((x : i32) (y : i32)) : Ordering
                 (if (< x y) LT (if (> x y) GT EQ))))
             (defn main () : Bool (< 1 2))",
        );
    }

    #[test]
    fn test_comparison_custom_type_without_ord_instance_errors() {
        let msg = check_err(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (deftype Color Red Green Blue)
             (defn main () : Bool (< Red Green))",
        );
        assert!(
            msg.contains("no instance of Ord for Color"),
            "expected 'no instance of Ord' error, got: {}",
            msg
        );
    }

    #[test]
    fn test_comparison_custom_type_with_ord_instance_passes() {
        check_ok(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (deftype Priority Low High)
             (instance (Ord Priority)
               (defn compare ((a : Priority) (b : Priority)) : Ordering
                 LT))
             (defn main () : Bool (< Low High))",
        );
    }

    // ── Arena escape analysis tests ─────────────────────────────

    #[test]
    fn arena_escape_vector() {
        let msg = check_err(
            "(defn main () (with-arena a [1 2 3]))",
        );
        assert!(msg.contains("arena-allocated value cannot escape"), "got: {}", msg);
    }

    #[test]
    fn arena_escape_lambda() {
        let msg = check_err(
            "(defn main () (with-arena a (fn (x) x)))",
        );
        assert!(msg.contains("arena-allocated value cannot escape"), "got: {}", msg);
    }

    #[test]
    fn arena_ok_returns_non_heap() {
        check_ok(
            "(defn main ()
               (with-arena a
                 (let ((v [1 2 3]))
                   (len v))))",
        );
    }

    #[test]
    fn arena_ok_returns_pre_existing_value() {
        check_ok(
            "(defn main ()
               (let ((x 42))
                 (with-arena a x)))",
        );
    }

    #[test]
    fn arena_escape_via_set() {
        let msg = check_err(
            "(defn main ()
               (let ((mut x [1]))
                 (with-arena a
                   (set! x [2 3]))))",
        );
        assert!(msg.contains("cannot assign arena-allocated value to outer variable"), "got: {}", msg);
    }

    #[test]
    fn arena_nested_inner_escape() {
        let msg = check_err(
            "(defn main ()
               (with-arena a
                 (let ((v [1 2 3]))
                   (with-arena b [4 5]))))",
        );
        assert!(msg.contains("arena-allocated value cannot escape"), "got: {}", msg);
    }

    #[test]
    fn arena_ok_returns_pre_existing_gc_vector() {
        check_ok(
            "(defn main ()
               (let ((v [1 2]))
                 (with-arena a v)))",
        );
    }

    #[test]
    fn arena_provenance_does_not_leak_across_scopes() {
        // After a let scope containing arena-provenance `x` pops,
        // a new `x` in the outer scope should not inherit stale provenance.
        check_ok(
            "(defn main () : i64
               (with-arena a
                 (let ((x [1 2 3]))
                   (len x))
                 (let ((x 42))
                   x)))",
        );
    }

    #[test]
    fn arena_shadowed_variable_independent_provenance() {
        // Inner `v` has arena provenance, but outer `v` (GC) should be
        // independently tracked and safe to return from the arena block.
        check_ok(
            "(defn main ()
               (let ((v [1 2]))
                 (with-arena a
                   (let ((v [3 4]))
                     (len v))
                   v)))",
        );
    }

    // ── Property-based tests ────────────────────────────────────

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn typeck_never_panics_on_parsed_input(s in "\\PC{0,200}") {
                let (module, _errors) = weir_parser::parse(&s);
                let _ = check(&module);
            }

            #[test]
            fn typeck_never_panics_on_lispy_input(
                s in proptest::string::string_regex(
                    r"[\(\)\[\] a-z0-9\+\-\*/:;'\n ]{0,150}"
                ).unwrap()
            ) {
                let (module, _errors) = weir_parser::parse(&s);
                let _ = check(&module);
            }

            #[test]
            fn typeck_is_deterministic(
                s in proptest::string::string_regex(
                    r"\(defn main \(\) \([\+\-\*] [0-9]{1,3} [0-9]{1,3}\)\)"
                ).unwrap()
            ) {
                let (module, errors) = weir_parser::parse(&s);
                if errors.is_empty() {
                    let r1 = check(&module);
                    let r2 = check(&module);
                    prop_assert_eq!(r1.errors.len(), r2.errors.len());
                }
            }
        }
    }

    // ── Dependency graph tests ────────────────────────────────────

    #[test]
    fn dep_graph_call_chain() {
        // A calls B calls C → forward and reverse deps
        let source = "(defn c () : i32 42)
                      (defn b () : i32 (c))
                      (defn a () : i32 (b))";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        // Forward: a→{b}, b→{c}
        assert!(deps.call_deps.get("a").unwrap().contains("b"));
        assert!(deps.call_deps.get("b").unwrap().contains("c"));
        // Reverse: c is called by {b}, b is called by {a}
        assert!(deps.callers.get("c").unwrap().contains("b"));
        assert!(deps.callers.get("b").unwrap().contains("a"));
    }

    #[test]
    fn dep_graph_type_usage() {
        // Function uses a deftype constructor
        let source = "(deftype Color (Red) (Green) (Blue))
                      (defn pick () : Color Red)";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        // pick uses type Color
        assert!(deps.type_deps.get("pick").unwrap().contains("Color"));
        // Reverse: Color is used by pick
        assert!(deps.type_users.get("Color").unwrap().contains("pick"));
    }

    #[test]
    fn dep_graph_diamond() {
        // A→B, A→C, B→D, C→D (diamond)
        let source = "(defn d () : i32 1)
                      (defn b () : i32 (d))
                      (defn c () : i32 (d))
                      (defn a () : i32 (+ (b) (c)))";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        let a_deps = deps.call_deps.get("a").unwrap();
        assert!(a_deps.contains("b"));
        assert!(a_deps.contains("c"));
        assert!(deps.call_deps.get("b").unwrap().contains("d"));
        assert!(deps.call_deps.get("c").unwrap().contains("d"));
        // Reverse: d is called by {b, c}
        let d_callers = deps.callers.get("d").unwrap();
        assert!(d_callers.contains("b"));
        assert!(d_callers.contains("c"));
    }

    #[test]
    fn dep_graph_pattern_match_type() {
        let source = "(deftype Option (Some i32) (None))
                      (defn unwrap ((x : Option)) : i32
                        (match x
                          ((Some v) v)
                          (None 0)))";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        assert!(deps.type_deps.get("unwrap").unwrap().contains("Option"));
        assert!(deps.type_users.get("Option").unwrap().contains("unwrap"));
    }

    // ── Atom type checking ──────────────────────────────────────────

    #[test]
    fn atom_create_and_deref() {
        check_ok(
            "(defn main () : Unit
               (let ((a (atom 42)))
                 (println (deref a))))",
        );
    }

    #[test]
    fn atom_typed_annotation() {
        // Test that atom type propagates correctly through a typed helper
        check_ok(
            "(defn make-atom ((x : i64)) : (Atom i64) (atom x))
             (defn main () : Unit
               (let ((a (make-atom 42)))
                 (println (deref a))))",
        );
    }

    #[test]
    fn atom_swap_bang_basic() {
        check_ok(
            "(defn inc ((x : i64)) : i64 (+ x 1))
             (defn main () : Unit
               (let ((a (atom 0)))
                 (swap! a inc)
                 (println (deref a))))",
        );
    }

    #[test]
    fn atom_swap_bang_with_lambda() {
        check_ok(
            "(defn main () : Unit
               (let ((a (atom 10)))
                 (swap! a (fn ((x : i64)) : i64 (+ x 5)))
                 (println (deref a))))",
        );
    }

    #[test]
    fn error_deref_non_atom() {
        let msg = check_err(
            "(defn main () : Unit (println (deref 42)))",
        );
        assert!(msg.contains("Atom"), "expected Atom error, got: {}", msg);
    }

    #[test]
    fn error_swap_bang_wrong_func_type() {
        let msg = check_err(
            "(defn to-str ((x : i64)) : String (str x))
             (defn main () : Unit
               (let ((a (atom 0)))
                 (swap! a to-str)))",
        );
        assert!(msg.contains("mismatch") || msg.contains("expected"), "expected type error, got: {}", msg);
    }

    #[test]
    fn atom_nested_in_let() {
        check_ok(
            "(defn main () : Unit
               (let ((a (atom 100))
                     (b (deref a)))
                 (println b)))",
        );
    }

    // ── Shareable typeclass ─────────────────────────────────────────

    #[test]
    fn shareable_primitives_ok() {
        // Primitives are Shareable — swap! on atom of i64 should work
        check_ok(
            "(defn main () : Unit
               (let ((a (atom 42)))
                 (println (swap! a (fn ((x : i64)) : i64 (+ x 1))))))",
        );
    }

    #[test]
    fn shareable_bool_ok() {
        check_ok(
            "(defn main () : Unit
               (let ((a (atom true)))
                 (println (swap! a (fn ((x : Bool)) : Bool (not x))))))",
        );
    }

    #[test]
    fn shareable_string_ok() {
        check_ok(
            "(defn main () : Unit
               (let ((a (atom (str 0))))
                 (println (swap! a (fn ((x : String)) : String x)))))",
        );
    }

    // ── spawn / with-tasks type checking ──────────────────────────

    #[test]
    fn spawn_outside_with_tasks_error() {
        let msg = check_err(
            "(defn main () : Unit (spawn (println 1)))",
        );
        assert!(msg.contains("with-tasks"), "expected with-tasks error, got: {}", msg);
    }

    #[test]
    fn with_tasks_basic() {
        check_ok(
            "(defn main () : Unit
               (with-tasks
                 (spawn (println 1))
                 (spawn (println 2))))",
        );
    }

    #[test]
    fn with_tasks_returns_last_body() {
        check_ok(
            "(defn main () : Unit
               (with-tasks
                 (spawn (println 1))
                 (println 3)))",
        );
    }

    // ── Channel type checking ────────────────────────────────────

    #[test]
    fn channel_create_send_recv() {
        check_ok(
            "(defn main () : Unit
               (let ((ch (channel)))
                 (send ch 42)
                 (println (recv ch))))",
        );
    }

    #[test]
    fn channel_type_inference() {
        // Channel type should be inferred from usage
        check_ok(
            "(defn main () : Unit
               (let ((ch (channel)))
                 (send ch true)
                 (println (recv ch))))",
        );
    }

    // ── par-map / par-for-each type checking ──────────────────────

    #[test]
    fn par_map_basic() {
        check_ok(
            "(defn main () : Unit
               (let ((v [1 2 3])
                     (doubled (par-map (fn ((x : i64)) : i64 (* x 2)) v)))
                 (println (len doubled))))",
        );
    }

    #[test]
    fn par_for_each_basic() {
        check_ok(
            "(defn main () : Unit
               (par-for-each (fn ((x : i64)) : Unit (println x)) [1 2 3]))",
        );
    }

    #[test]
    fn error_par_map_wrong_type() {
        let msg = check_err(
            "(defn main () : Unit
               (let ((v [1 2 3]))
                 (par-map (fn ((x : String)) : String x) v)))",
        );
        assert!(msg.contains("mismatch"), "expected type mismatch, got: {}", msg);
    }

    #[test]
    fn error_send_wrong_type() {
        let msg = check_err(
            "(defn main () : Unit
               (let ((ch (channel)))
                 (send ch 42)
                 (send ch true)))",
        );
        assert!(msg.contains("mismatch"), "expected type mismatch, got: {}", msg);
    }

    #[test]
    fn shareable_closure_rejected() {
        // Closures are NOT Shareable — swap! on atom of closure should fail
        let msg = check_err(
            "(defn main () : Unit
               (let ((f (fn ((x : i64)) : i64 x))
                     (a (atom f)))
                 (swap! a (fn ((g : (Fn [i64] i64))) : (Fn [i64] i64) g))))",
        );
        assert!(msg.contains("Shareable"), "expected Shareable error, got: {}", msg);
    }

    // ── Extern C / CFFI ──────────────────────────────────────────

    #[test]
    fn extern_c_with_unsafe() {
        check_ok(
            "(extern \"C\"
               (defn abs ((n : i32)) : i32))
             (defn main () : i32
               (unsafe (abs -42)))",
        );
    }

    #[test]
    fn extern_c_without_unsafe_errors() {
        let msg = check_err(
            "(extern \"C\"
               (defn abs ((n : i32)) : i32))
             (defn main () : i32
               (abs -42))",
        );
        assert!(
            msg.contains("unsafe"),
            "expected unsafe error, got: {}",
            msg
        );
    }

    #[test]
    fn extern_c_ptr_type() {
        check_ok(
            "(extern \"C\"
               (defn atoi ((s : Ptr)) : i32))
             (defn main ((p : Ptr)) : i32
               (unsafe (atoi p)))",
        );
    }

    // ── Mutability enforcement ──────────────────────────────────

    #[test]
    fn set_bang_immutable_binding_error() {
        let msg = check_err(
            "(defn main ()
               (let ((x 0))
                 (set! x 5)))",
        );
        assert!(msg.contains("cannot mutate immutable binding"), "got: {}", msg);
    }

    #[test]
    fn set_bang_mutable_binding_ok() {
        check_ok(
            "(defn main ()
               (let ((mut x 0))
                 (set! x 5)))",
        );
    }

    #[test]
    fn set_bang_immutable_param_error() {
        let msg = check_err(
            "(defn foo ((x : i32)) : Unit
               (set! x 5))",
        );
        assert!(msg.contains("cannot mutate immutable binding"), "got: {}", msg);
    }

    #[test]
    fn set_bang_mutable_param_ok() {
        check_ok(
            "(defn foo ((mut x : i32)) : Unit
               (set! x 5))",
        );
    }

    // ── Struct field assignment ─────────────────────────────────

    #[test]
    fn struct_field_set_ok() {
        check_ok(
            "(defstruct Point (x : i32) (y : i32))
             (defn main ()
               (let ((mut p (Point :x 1 :y 2)))
                 (set! (.x p) 10)))",
        );
    }

    #[test]
    fn struct_field_set_immutable_error() {
        let msg = check_err(
            "(defstruct Point (x : i32) (y : i32))
             (defn main ()
               (let ((p (Point :x 1 :y 2)))
                 (set! (.x p) 10)))",
        );
        assert!(msg.contains("cannot mutate field of immutable binding"), "got: {}", msg);
    }

    #[test]
    fn struct_field_set_type_mismatch() {
        let msg = check_err(
            "(defstruct Point (x : i32) (y : i32))
             (defn main ()
               (let ((mut p (Point :x 1 :y 2)))
                 (set! (.x p) \"hello\")))",
        );
        assert!(msg.contains("type mismatch") || msg.contains("cannot unify"), "got: {}", msg);
    }

    #[test]
    fn struct_field_set_nonexistent_field() {
        let msg = check_err(
            "(defstruct Point (x : i32) (y : i32))
             (defn main ()
               (let ((mut p (Point :x 1 :y 2)))
                 (set! (.z p) 10)))",
        );
        assert!(msg.contains("no field 'z'"), "got: {}", msg);
    }

    // ── For / ForEach ──────────────────────────────────────────────

    #[test]
    fn for_loop_basic() {
        check_ok(
            "(defn main () : Unit
               (for (i 0 (< i 10))
                 (println i)))",
        );
    }

    #[test]
    fn for_loop_condition_must_be_bool() {
        let msg = check_err(
            "(defn main ()
               (for (i 0 42)
                 (println i)))",
        );
        assert!(msg.contains("Bool") || msg.contains("unify"), "got: {}", msg);
    }

    #[test]
    fn for_each_basic() {
        check_ok(
            "(defn main () : Unit
               (let ((xs [1 2 3]))
                 (for-each (x xs)
                   (println x))))",
        );
    }

    #[test]
    fn for_each_requires_vector() {
        let msg = check_err(
            "(defn main ()
               (for-each (x 42)
                 (println x)))",
        );
        assert!(msg.contains("Vector") || msg.contains("for-each"), "got: {}", msg);
    }

    // ── Type-name-as-cast ──────────────────────────────────────────

    #[test]
    fn type_cast_f64() {
        check_ok(
            "(defn main () : f64
               (f64 42))",
        );
    }

    #[test]
    fn type_cast_i32() {
        check_ok(
            "(defn main () : i32
               (i32 3.14))",
        );
    }

    #[test]
    fn type_cast_wrong_arity() {
        let msg = check_err(
            "(defn main ()
               (f64 1 2))",
        );
        assert!(msg.contains("1 argument") || msg.contains("arity") || msg.contains("argument"), "got: {}", msg);
    }

    // ── Defglobal ──────────────────────────────────────────────────

    #[test]
    fn defglobal_immutable() {
        check_ok(
            "(defglobal *x* : i64 42)
             (defn main () : Unit
               (println *x*))",
        );
    }

    #[test]
    fn defglobal_mutable() {
        check_ok(
            "(defglobal mut *x* : i64 0)
             (defn main () : Unit
               (set! *x* 10)
               (println *x*))",
        );
    }

    #[test]
    fn defglobal_immutable_set_rejected() {
        let msg = check_err(
            "(defglobal *x* : i64 42)
             (defn main ()
               (set! *x* 10))",
        );
        assert!(msg.contains("immutable") || msg.contains("mutable") || msg.contains("cannot"), "got: {}", msg);
    }

    // ── MutVec / MutMap ────────────────────────────────────────────

    #[test]
    fn mutvec_basic() {
        check_ok(
            "(defn main () : Unit
               (let ((mut v (mut-vec)))
                 (push! v 1)
                 (push! v 2)
                 (println (len v))))",
        );
    }

    #[test]
    fn mutmap_basic() {
        check_ok(
            "(defn main () : Unit
               (let ((mut m (mut-map)))
                 (map-set! m 1 2)
                 (println (map-get m 1))))",
        );
    }
