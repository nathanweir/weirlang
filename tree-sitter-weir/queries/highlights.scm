; Keywords
(defn_keyword) @keyword.function
(fn_keyword) @keyword.function

(deftype_keyword) @keyword
(defstruct_keyword) @keyword
(defclass_keyword) @keyword
(instance_keyword) @keyword
(let_keyword) @keyword
(do_keyword) @keyword
(declare_keyword) @keyword

(if_keyword) @keyword.conditional
(cond_keyword) @keyword.conditional
(when_keyword) @keyword.conditional
(unless_keyword) @keyword.conditional
(match_keyword) @keyword.conditional
(else_keyword) @keyword.conditional

(import_keyword) @keyword.import

(pub_keyword) @keyword.modifier
(mut_keyword) @keyword.modifier

(set_bang_keyword) @keyword.operator
(threading_operator) @keyword.operator

; Function names in definitions
(defn_inner name: (symbol) @function)

; Method names in defclass
(method_signature name: (symbol) @function.method)

; Builtin functions
(call_expr
  function: (symbol) @function.builtin
  (#any-of? @function.builtin
    "println" "print" "sleep" "str" "len" "nth" "append" "type-of"
    "map" "filter" "reduce" "range" "head" "tail" "cons"))

; Operators as function calls
(call_expr
  function: (symbol) @operator
  (#any-of? @operator
    "+" "-" "*" "/" "mod"
    "<" ">" "<=" ">=" "=" "!="
    "not" "and" "or"))

; Type names
(return_type (_) @type)
(typed_param type: (_) @type)
(typed_binding_name ":" (_) @type)
(struct_field type: (_) @type)
(method_signature type: (_) @type)
(arrow_type (_) @type)
(fn_type_keyword) @type.builtin

; Type names in deftype/defstruct/defclass
(deftype_inner name: (symbol) @type)
(deftype_inner name: (applied_type_name (symbol) @type))
(defstruct_inner name: (symbol) @type)
(defclass_inner name: (type_constructor (symbol) @type))
(instance_head (symbol) @type)

; Constructors (variant names in deftype)
(variant name: (symbol) @constructor)
; Nullary variants are symbols directly under deftype_inner
; Handled by variant rule field name

; Constructor patterns in match
(constructor_pattern constructor: (symbol) @constructor)

; Parameters
(param_list (symbol) @variable.parameter)
(typed_param name: (symbol) @variable.parameter)

; Properties (field access)
(field_access) @property
(struct_field name: (symbol) @property)

; Type parameters
(type_var) @type.parameter

; Literals
(keyword) @string.special.symbol
(number) @number
(float) @number.float
(string) @string
(boolean) @boolean

; Comments
(comment) @comment

; Punctuation
["(" ")"] @punctuation.bracket
["[" "]"] @punctuation.bracket
["{" "}"] @punctuation.bracket
":" @punctuation.delimiter

; Module in import
(import_form module: (symbol) @module)
