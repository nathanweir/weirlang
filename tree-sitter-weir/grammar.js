/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "weir",

  extras: ($) => [/\s/, $.comment],

  word: ($) => $.symbol,

  rules: {
    source_file: ($) => repeat($._form),

    _form: ($) =>
      choice(
        $.list_expression,
        $.vector_literal,
        $.map_literal,
        $.number,
        $.float,
        $.string,
        $.boolean,
        $.keyword,
        $.type_var,
        $.field_access,
        $.operator,
        $.symbol,
      ),

    // --- List expressions dispatch to special forms or fallback to call ---

    list_expression: ($) =>
      choice(
        $.defn_form,
        $.deftype_form,
        $.defstruct_form,
        $.defclass_form,
        $.instance_form,
        $.import_form,
        $.pub_form,
        $.let_expr,
        $.if_expr,
        $.cond_expr,
        $.when_expr,
        $.unless_expr,
        $.match_expr,
        $.fn_expr,
        $.do_expr,
        $.set_bang_expr,
        $.declare_expr,
        $.threading_expr,
        $.call_expr,
      ),

    // --- Top-level definitions ---

    defn_form: ($) => seq("(", $.defn_inner, ")"),

    defn_inner: ($) =>
      seq(
        field("keyword", alias("defn", $.defn_keyword)),
        field("name", $.symbol),
        $._defn_rest,
      ),

    // The rest of a defn after the name: either arrow-style or params-style
    _defn_rest: ($) =>
      choice(
        // Arrow-style: (defn name (-> RetType) body...)
        seq($.arrow_type, repeat1($._form)),
        // Params with optional return type
        seq(
          field("params", $.param_list),
          optional($.return_type),
          repeat1($._form),
        ),
        // No params, no parens - just body (rare but valid)
        seq($.return_type, repeat1($._form)),
      ),

    arrow_type: ($) => seq("(", "->", repeat($._type_expr), ")"),

    param_list: ($) => seq("(", repeat($._param), ")"),

    _param: ($) =>
      choice(
        $.typed_param,
        $.symbol,
      ),

    typed_param: ($) =>
      seq("(", optional(alias("mut", $.mut_keyword)), field("name", $.symbol), ":", field("type", $._type_expr), ")"),

    return_type: ($) => seq(":", $._type_expr),

    deftype_form: ($) => seq("(", $.deftype_inner, ")"),

    deftype_inner: ($) =>
      seq(
        field("keyword", alias("deftype", $.deftype_keyword)),
        field("name", $._type_name),
        repeat($.variant),
      ),

    _type_name: ($) =>
      choice(
        $.symbol,
        $.applied_type_name,
      ),

    applied_type_name: ($) =>
      seq("(", $.symbol, repeat1($.type_var), ")"),

    variant: ($) =>
      choice(
        $.symbol, // Nullary variant like None
        seq("(", field("name", $.symbol), repeat($._type_expr), ")"), // Variant with fields
      ),

    defstruct_form: ($) => seq("(", $.defstruct_inner, ")"),

    defstruct_inner: ($) =>
      seq(
        field("keyword", alias("defstruct", $.defstruct_keyword)),
        field("name", $.symbol),
        repeat($.struct_field),
      ),

    struct_field: ($) =>
      seq("(", field("name", $.symbol), ":", field("type", $._type_expr), ")"),

    defclass_form: ($) => seq("(", $.defclass_inner, ")"),

    defclass_inner: ($) =>
      seq(
        field("keyword", alias("defclass", $.defclass_keyword)),
        field("name", $.type_constructor),
        repeat($.method_signature),
      ),

    type_constructor: ($) => seq("(", $.symbol, repeat($.type_var), ")"),

    method_signature: ($) =>
      seq("(", field("name", $.symbol), ":", field("type", $._type_expr), ")"),

    instance_form: ($) =>
      seq(
        "(",
        field("keyword", alias("instance", $.instance_keyword)),
        field("name", $.instance_head),
        repeat($._instance_member),
        ")",
      ),

    instance_head: ($) => seq("(", $.symbol, repeat($._type_expr), ")"),

    _instance_member: ($) =>
      choice(
        seq("(", $.defn_inner, ")"),
      ),

    import_form: ($) =>
      seq(
        "(",
        field("keyword", alias("import", $.import_keyword)),
        field("module", $.symbol),
        repeat($._form),
        ")",
      ),

    pub_form: ($) =>
      seq(
        "(",
        field("keyword", alias("pub", $.pub_keyword)),
        choice($.defn_inner, $.deftype_inner, $.defstruct_inner),
        ")",
      ),

    // --- Expressions ---

    let_expr: ($) =>
      seq(
        "(",
        alias("let", $.let_keyword),
        $.bindings,
        repeat1($._form),
        ")",
      ),

    bindings: ($) => seq("(", repeat1($.binding), ")"),

    binding: ($) =>
      seq(
        "(",
        optional(alias("mut", $.mut_keyword)),
        $._binding_target,
        $._form,
        ")",
      ),

    _binding_target: ($) =>
      choice(
        $.typed_binding_name,
        $.symbol,
      ),

    typed_binding_name: ($) =>
      seq("(", optional(alias("mut", $.mut_keyword)), $.symbol, ":", $._type_expr, ")"),

    if_expr: ($) =>
      seq(
        "(",
        alias("if", $.if_keyword),
        field("condition", $._form),
        field("then", $._form),
        optional(field("else", $._form)),
        ")",
      ),

    cond_expr: ($) =>
      seq(
        "(",
        alias("cond", $.cond_keyword),
        repeat1($.cond_clause),
        ")",
      ),

    cond_clause: ($) =>
      seq(
        "(",
        choice(alias("else", $.else_keyword), $._form),
        repeat1($._form),
        ")",
      ),

    when_expr: ($) =>
      seq(
        "(",
        alias("when", $.when_keyword),
        field("condition", $._form),
        repeat1($._form),
        ")",
      ),

    unless_expr: ($) =>
      seq(
        "(",
        alias("unless", $.unless_keyword),
        field("condition", $._form),
        repeat1($._form),
        ")",
      ),

    match_expr: ($) =>
      seq(
        "(",
        alias("match", $.match_keyword),
        field("scrutinee", $._form),
        repeat1($.match_arm),
        ")",
      ),

    match_arm: ($) =>
      seq("(", field("pattern", $._pattern), repeat1($._form), ")"),

    _pattern: ($) =>
      choice(
        $.constructor_pattern,
        $.symbol,
        $.number,
        $.float,
        $.string,
        $.boolean,
        $.keyword,
      ),

    constructor_pattern: ($) =>
      seq("(", field("constructor", $.symbol), repeat($._pattern), ")"),

    fn_expr: ($) =>
      seq(
        "(",
        alias("fn", $.fn_keyword),
        field("params", $.param_list),
        optional($.return_type),
        repeat1($._form),
        ")",
      ),

    do_expr: ($) =>
      seq(
        "(",
        alias("do", $.do_keyword),
        repeat1($._form),
        ")",
      ),

    set_bang_expr: ($) =>
      seq(
        "(",
        alias("set!", $.set_bang_keyword),
        field("place", $._form),
        field("value", $._form),
        ")",
      ),

    declare_expr: ($) =>
      seq(
        "(",
        alias("declare", $.declare_keyword),
        repeat1($._form),
        ")",
      ),

    threading_expr: ($) =>
      seq(
        "(",
        field("operator", $.threading_operator),
        repeat1($._form),
        ")",
      ),

    threading_operator: (_) => choice("->", "->>"),

    call_expr: ($) =>
      prec(
        -1,
        seq(
          "(",
          field("function", $._form),
          repeat($._form),
          ")",
        ),
      ),

    // --- Type expressions ---

    _type_expr: ($) =>
      choice(
        $.fn_type,
        $.constrained_type,
        $.applied_type,
        $.symbol,
        $.type_var,
      ),

    fn_type: ($) =>
      seq(
        "(",
        alias("Fn", $.fn_type_keyword),
        "[",
        repeat($._type_expr),
        "]",
        $._type_expr,
        ")",
      ),

    constrained_type: ($) =>
      seq(
        "(",
        "=>",
        $.constraint,
        $._type_expr,
        ")",
      ),

    constraint: ($) => seq("(", $.symbol, repeat1($._type_expr), ")"),

    applied_type: ($) =>
      seq("(", $.symbol, repeat1($._type_expr), ")"),

    // --- Collection literals ---

    vector_literal: ($) => seq("[", repeat($._form), "]"),

    map_literal: ($) => seq("{", repeat(seq($.keyword, $._form)), "}"),

    // --- Terminals ---

    comment: (_) => token(seq(";", /.*/)),

    number: (_) => token(prec(0, /\-?[0-9]+/)),

    float: (_) => token(prec(1, /\-?[0-9]+\.[0-9]+/)),

    string: (_) =>
      token(
        seq(
          '"',
          repeat(
            choice(
              /[^"\\]/,
              seq("\\", /./),
            ),
          ),
          '"',
        ),
      ),

    boolean: (_) => choice("true", "false"),

    keyword: (_) => token(seq(":", /[a-zA-Z_][a-zA-Z0-9_\-]*/)),

    type_var: (_) => token(seq("'", /[a-zA-Z][a-zA-Z0-9]*/)),

    field_access: (_) => token(seq(".", /[a-zA-Z_][a-zA-Z0-9_\-]*/)),

    operator: (_) =>
      token(
        choice(
          "+", "*", "/",
          "<=", ">=", "!=",
          "<", ">", "=",
          "-",
        ),
      ),

    symbol: (_) =>
      token(
        /[a-zA-Z_!?][a-zA-Z0-9_!?\-]*/,
      ),
  },
});
