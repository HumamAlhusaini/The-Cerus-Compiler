From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Inductive option (A : Type) : Type :=
| Some : A -> option A
| None : option A.

Parameter loc : Type.

Parameter str : Type.

Parameter char_code : Type.

(*  Expressions   *)
Inductive expression :=
  | EXPRESSION_WITHOUT_BLOCK :  list outer_attribute -> type_expr_without_block -> expression
  | EXPRESSION_WITH_BLOCK : list outer_attribute -> type_expr_with_block -> expression

with type_expr_without_block :=
  | LITERAL_EXPRESSION : literal_expression -> type_expr_without_block
  | PATH_EXPRESSION : path_expression -> type_expr_without_block
  | OPERATOR_EXPRESSION : operator_expression -> type_expr_without_block
  | GROUPED_EXPRESSION : grouped_expression -> type_expr_without_block
  | ARRAY_EXPRESSION : array_expression -> type_expr_without_block
  | AWAIT_EXPRESSION : await_expression -> type_expr_without_block
  | INDEX_EXPRESSION : index_expression -> type_expr_without_block
  | TUPLE_EXPRESSION : tuple_expression -> type_expr_without_block
  | TUPLE_INDEXING_EXPRESSION : tuple_indexing_expression -> type_expr_without_block
  | STRUCT_EXPRESSION : struct_expression -> type_expr_without_block
  | CALL_EXPRESSION : call_expression -> type_expr_without_block
  | METHOD_CALL_EXPRESSION : method_call_expression -> type_expr_without_block
  | FIELD_EXPRESSION : field_expression -> type_expr_without_block
  | CLOSURE_EXPRESSION : closure_expression -> type_expr_without_block
  | ASYNC_BLOCK_EXPRESSION : async_block_expression -> type_expr_without_block
  | CONTINUE_EXPRESSION : continue_expression -> type_expr_without_block
  | BREAK_EXPRESSION : break_expression -> type_expr_without_block
  | RANGE_EXPRESSION : range_expression -> type_expr_without_block
  | RETURN_EXPRESSION : return_expression -> type_expr_without_block
  | UNDERSCORE_EXPRESSION
  | MACRO_INVOCATION : macro_invocation -> type_expr_without_block 

with type_expr_with_block :=
  | BLOCK_EXPRESSION : block_expression -> type_expr_with_block
  | CONST_BLOCK_EXPRESSION : const_block_expression -> type_expr_with_block
  | UNSAFE_BLOCK_EXPRESSION : unsafe_block_expression -> type_expr_with_block
  | LOOP_EXPRESSION : loop_expression -> type_expr_with_block
  | IF_EXPRESSION : if_expression -> type_expr_with_block
  | IF_LET_EXPRESSION : if_let_expression -> type_expr_with_block
  | MATCH_EXPRESSION : match_expression -> type_expr_with_block
      (*  Expressions  *)

      (* Grouped Expressions *)
with grouped_expression :=
  | GROUPED_EXPR : expression -> grouped_expression
      (* Grouped Expressions *)
      (* Array and Index Expression*)
with array_expression :=
  | ARRAY_EXPR : list array_elements -> array_expression

with array_elements :=
  | ARRAY_ElEMENT : list expression -> array_elements
  | SEMI_ARRAY_ELEMENT : expression -> expression -> array_elements

with index_expression :=
  | INDEX_EXPR : expression -> expression -> index_expression

      (* Array and Index Expression*)
      (*Tuple and Tuple Indexing Expressions*)
with tuple_expression :=
  | TUPLE_EXPR : option tuple_elements -> tuple_expression

with tuple_elements :=
  | TUPLE_ELEMENTS : list expression -> option expression -> tuple_elements

with tuple_indexing_expression :=
  | TUPLE_INDEXING_EXPR : expression -> str (*tuple index is equivalent to string, for now...*) -> tuple_indexing_expression
      (*Tuple and Tuple Indexing Expressions*)
      (*  Struct Expressions *)
with struct_expression :=
  | STRUCT_EXPRESSION_STRUCT : path_in_expression -> option struct_expr_field_or_struct_base -> struct_expression
  | STRUCT_EXPRESSION_TUPLE :  path_in_expression -> option expr_list -> struct_expression
  | STRUCT_EXPRESSION_UNIT :  path_in_expression -> struct_expression

with struct_expr_field_or_struct_base :=
  | STRUCT_EXPR_FIELD_OPT : struct_expr_fields -> struct_expr_field_or_struct_base
  | STRUCT_BASE_OPT : struct_base -> struct_expr_field_or_struct_base

with struct_expr_fields :=
  | STRUCT_EXPR_FIELDS : struct_expr_field -> list struct_expr_field -> option struct_base -> struct_expr_fields

with struct_expr_field :=
  | STRUCT_EXPR_FIELD : list outer_attribute -> struct_expr_field

with identifier_or_id_tup :=
  | IDENTIFIER_ID_OPT : identifier -> identifier_or_id_tup
  | ID_AND_TUP : id_or_tup -> expression -> identifier_or_id_tup

with id_or_tup :=
  | IDENTIFIER_OPT : identifier -> id_or_tup
  | TUPLE_OPT : str -> id_or_tup

with struct_base :=
  | STRUCT_BASE : expression -> struct_base

with expr_list :=
  | EXPR_LIST : expression -> list expression -> expr_list

      (*  Struct Expressions *)
      (*  Call Expressions *)
with call_expression :=
  | CALL_EXPR : expression -> call_params -> call_expression

with call_params :=
  | CALL_PARAMS : expression -> list expression -> call_params
      (*  Call Expressions *)
      (*  Method Expressions *)
with method_call_expression :=
  | METHOD_CALL_EXPR : expression -> path_expr_segment -> option call_params -> method_call_expression
      (*  Method Expressions *)
      (*  Field Expressions *)
with field_expression :=
  | FIELD_EXPR : expression -> identifier -> field_expression
      (*  Field Expressions *)
      (*  Closure Expressions *)
      with closure_expression :=
  | CLOSURE_EXPR : bool -> bool -> option closure_params -> expr_or_type_no_bounds -> closure_expression

with expr_or_type_no_bounds :=
  | EXPR_OPT : expression -> expr_or_type_no_bounds
  | TYPE_BLOCK_OPT : type_no_bounds -> block_expression -> expr_or_type_no_bounds

with closure_params :=
  | CLOSURE_PARAMS : closure_param -> list closure_param -> closure_params

with closure_param :=
  | CLOSURE_PARAM : list outer_attribute -> pattern_no_top_alt -> option type -> closure_param
      (*  Closure Expressions *)
      (*  Loop Expressions *)
with loop_expression :=
  | LOOP_EXPR : option loop_label -> loop_types -> loop_expression

with loop_label :=
  | LOOP_LABEL : str -> loop_label

with break_expression :=
  | BREAK_EXPR : option str -> option expression -> break_expression

with continue_expression :=
  | CONTINUE_EXPR : option str -> continue_expression

with loop_types :=
  | INFINITE_LOOP_EXPRESSION : block_expression -> loop_types
  | PREDICATE_LOOP_EXPRESSION : expression -> block_expression -> loop_types
  | PREDICATE_PATTERN_LOOP_EXPRESSION : pattern -> scrutinee -> block_expression -> loop_types
  | ITERATOR_LOOP_EXPRESSION : pattern -> expression -> block_expression -> loop_types
  | LABEL_BLOCK_EXPRESSION : block_expression -> loop_types
      (*  Loop Expressions *)
      (* Range Expressions*)
with range_expression :=
  | RANGE_EXPR : expression -> expression -> range_expression
  | RANGE_FROM_EXPR : expression -> range_expression
  | RANGE_TO_EXPR : expression -> range_expression
  | RANGE_FULL_EXPR : range_expression
  | RANGE_INCLUSIVE_EXPR : expression -> expression -> range_expression
  | RANGE_TO_INCLUSIVE_EXPR : expression -> range_expression
      (* Range Expressions *)
      (* if and if let expressions *)
with if_expression :=
  | IfExpr : expression -> block_expression -> option choice -> if_expression

with choice :=
  | BLOCK : block_expression -> choice 
  | IF : if_expression -> choice 
  | IF_LET : if_let_expression -> choice

with if_let_expression :=
  | IF_LET_EXPR : pattern -> scrutinee -> block_expression -> option choice -> if_let_expression

      (* if and if let expressions *)
      (*Match expressions*)
with match_expression :=
  | MATCH_EXPR : scrutinee -> list inner_attribute -> option match_arms -> match_expression

with scrutinee :=
  | SCRUTINEE : expression -> scrutinee

with match_arm_guard :=
  | MATCHARMGUARD : expression -> match_arm_guard

with match_arm :=
  | MATCHARM : list outer_attribute -> pattern -> option match_arm_guard -> match_arm

with match_arms :=
  | MATCHARMS :
      list organized_matching -> match_arm -> expression -> match_arms

with organized_matching :=
  | ORGANIZE : match_arm -> block_or_not -> organized_matching

with block_or_not :=
  | YES_BLOCK : type_expr_with_block -> block_or_not
  | NO_BLOCK : type_expr_without_block -> block_or_not
      (*Match expressions*)
      (*Return expressions*)
with return_expression :=
  | RETURN_EXPR : option expression -> return_expression
      (*Return expressions*)
with await_expression :=
  | AWAIT_EXPR : expression -> await_expression
      (*Return expressions*)

      (*  Literal Expressions *)
with literal_expression :=
  | CHAR_LITERAL : char_code -> literal_expression
  | STRING_LITERAL : list char_code -> literal_expression
  | RAW_STRING_LITERAL : list char_code -> literal_expression
  | BYTE_LITERAL : char_code -> literal_expression
  | BYTE_STRING_LITERAL : list char_code -> literal_expression
  | RAW_BYTE_STRING_LITERAL : list char_code -> literal_expression
  | C_STRING_LITERAL : list char_code -> literal_expression
  | RAW_C_STRING_LITERAL : list char_code  -> literal_expression
  | INTEGER_LITERAL : str -> literal_expression
  | FLOAT_LITERAL : str -> literal_expression
  | TRUE
  | FALSE
(*  Literal Expressions *)

(* Path Expressions *)
with path_expression :=
  | PATH_EXPR : path_in_expression -> path_expression
  | QUALIFIED_PATH_EXPR : qualified_path_in_expression -> path_expression
(* Path Expressions *)

(* Block Expressions*)
with block_expression := 
  | BLOCK_EXPR : list inner_attribute -> option statements -> block_expression

with statements :=
  | STATEMENTS : list statement -> statements
  | STATEMENTS_EXPR_WITHOUT_BLOCK : list statement -> type_expr_without_block -> statements
  | S_EXPR_WITHOUT_BLOCK : type_expr_without_block -> statements

with async_block_expression :=
  | ASYNC_BLOCK_EXPR_MOVE : block_expression -> async_block_expression
  | ASYNC_BLOCK_EXPR_STILL : block_expression -> async_block_expression

with const_block_expression :=
  | CONST_BLOCK_EXPR : block_expression -> const_block_expression

with unsafe_block_expression :=
  | UNSAFE_BLOCK_EXPR : block_expression -> unsafe_block_expression
(*Block Expressions*)

(*Statements*)
with statement :=
  | STATEMENT_ITEM : item -> statement
  | STATEMENT_LET : let_statement -> statement
  | STATEMENT_EXPR : expr_statement -> statement
  | MACRO_INVOCATION_SEMI : macro_invocation_semi -> statement

with let_statement :=
  | LET_STATEMENT : list outer_attribute -> pattern_no_top_alt -> option type -> option eq_expr -> let_statement

with eq_expr :=
  | EQ_EXPR : expression -> option block_expression -> eq_expr

with expr_statement :=
  | EXPR_STATEMENT_NO_BLOCK : type_expr_without_block -> expr_statement
  | EXPR_STATEMENT_BLOCK : type_expr_with_block -> expr_statement
      (*Statements*) 

      (*Macros*)
with macro_invocation :=
  | MACRO_INVOC : simple_path -> delim_token_tree -> macro_invocation

with delim_token_tree :=
  | PAREN_TOKENS : list token_tree -> delim_token_tree
  | BRACK_TOKENS : list token_tree -> delim_token_tree 
  | BRACE_TOKENS : list token_tree -> delim_token_tree 

with token_tree :=
  | Token : str -> token_tree (*I believe all tokens can be represented as string, especially demonstrated by pretty printer*) -> token_tree
  | DELIM : delim_token_tree -> token_tree

with macro_invocation_semi :=
  | PAREN_MACRO : list token_tree -> macro_invocation_semi
  | BRACE_MACRO : list token_tree -> macro_invocation_semi
  | BRACK_MACRO : list token_tree -> macro_invocation_semi
      (*Macros*)
      (*Operator expression*)
with operator_expression :=
  | BORROW_EXPRESSION : borrow_kind -> expression -> operator_expression
  | DEREFERENCE_EXPRESSION : expression -> operator_expression
  | ERROR_PROPAGATION_EXPRESSION:  expression -> operator_expression
  | NEGATION_EXPRESSION : negation_expression -> operator_expression
  | ARITHMETIC_OR_LOGICAL_EXPRESSION :  expression -> arithmetic_or_logical_operation -> expression -> operator_expression
  | COMPARISON_EXPRESSION : expression -> comparison_operation -> expression -> operator_expression
  | LAZY_BOOLEAN_EXPRESSION : expression -> lazy_boolean_operation -> expression -> operator_expression
  | TYPE_CAST_EXPRESSION :  expression -> type_no_bounds -> operator_expression
  | ASSIGNMENT_EXPRESSION :  expression -> expression -> operator_expression
  | COMPOUND_ASSIGNMENT_EXPRESSION : expression -> compound_assignment_operation -> expression -> operator_expression

with borrow_kind :=
  | BK_Shared
  | BK_Mut
  | BK_RawConst
  | BK_RawMut

with negation_expression :=
  | NEG_EXPR : expression -> negation_expression
  | NOT_EXPR : expression -> negation_expression

with arithmetic_or_logical_operation :=
  | AOP_ADD | AOP_SUB | AOP_MUL | AOP_DIV | AOP_REM
  | AOP_AND | AOP_OR  | AOP_XOR | AOP_SHL | AOP_SHR

with comparison_operation :=
  | CMPOP_EQ | CMPOP_NE | CMPOP_GT | CMPOP_LT | CMPOP_GE | CMPOP_LE

with lazy_boolean_operation :=
  | LBOP_OR | LBOP_AND

with compound_assignment_operation :=
  | CAOP_ADDASSIGN | CAOP_SUBASSIGN | CAOP_MULASSIGN | CAOP_DIVASSIGN | CAOP_REMASSIGN
  | CAOP_ANDASSIGN | CAOP_ORASSIGN  | CAOP_XORASSIGN | CAOP_SHLASSIGN | CAOP_SHRASSIGN

      (*Operator expression*)

(* Paths *)
with simple_path :=
  | SIMPLE_PATH : list simple_path_segment -> simple_path

with simple_path_segment := 
  | SIMPLE_PATH_SEGMENT_IDENT : identifier ->  simple_path_segment
  | SIMPLE_PATH_SEGMENT_SUPER
  | SIMPLE_PATH_SEGMENT_SELF
  | SIMPLE_PATH_SEGMENT_CRATE
  | SIMPLE_PATH_SEGMENT_SCRATE

with path_in_expression :=
  | PATH_IN_EXPRESSION : list path_expr_segment -> path_in_expression

with path_expr_segment :=
  | PATH_EXPR_SEGMENT : path_ident_segment -> option generic_args -> path_expr_segment


with path_ident_segment :=
  | PATH_IDENT_SEGMENT_IDENT : identifier -> path_ident_segment
  | PATH_IDENT_SEGMENT_SUPER
  | PATH_IDENT_SEGMENT_SELF
  | PATH_IDENT_SEGMENT_self
  | PATH_IDENT_SEGMENT_CRATE
  | PATH_IDENT_SEGMENT_SCRATE

with qualified_path_in_expression :=
  | QUALIFIED_PATH_IN_EXPRESSION_CONSTRUCTOR : qualified_path_type -> list path_expr_segment -> qualified_path_in_expression

with qualified_path_type :=
  | QUALIFIED_PATH_TYPE : type -> option type_path -> qualified_path_type

with qualified_path_in_type :=
  | QUALIFIED_PATH_IN_TYPE : qualified_path_type -> list type_path_segment  -> qualified_path_in_type

with generic_args :=
  | EMPTY_GENERIC_ARGS 
  | GENERIC_ARGS : list generic_arg -> generic_arg -> generic_args

with generic_arg :=
  | GENERIC_ARG_LIFETIME : lifetime -> generic_arg
  | GENERIC_ARG_TYPE : type -> generic_arg
  | GENERIC_ARG_CONST : generic_args_const -> generic_arg
  | GENERIC_ARGS_BINDING : generic_args_binding -> generic_arg
  | GENERIC_ARGS_BOUNDS : generic_args_bounds -> generic_arg

with generic_args_const :=
  | GENERIC_ARGS_CONST_BLOCK : block_expression -> generic_args_const
  | GENERIC_ARGS_CONST_LIT : literal_expression -> generic_args_const
  | NEG_GENERIC_ARGS_CONST_LIT : literal_expression -> generic_args_const
  | GENERIC_ARGS_CONST_SIMPLE_PATH_SEG : simple_path_segment -> generic_args_const

with generic_args_binding :=
  | GENERIC_ARGS_BINDING_CONSTRUCTOR : identifier -> option generic_args -> type -> generic_args_binding

with generic_args_bounds :=
  | GENERIC_ARGS_BOUNDS_CONSTRUCTOR : identifier -> option generic_args -> type_param_bounds -> generic_args_bounds

with type_path :=
  | TYP_PATH : list type_path_segment -> type_path

with type_path_segment :=
  | TYPE_PATH_SEGMENT : path_ident_segment -> option genargs_or_type_path_fn -> type_path_segment

with genargs_or_type_path_fn :=
  | GENARGS_OPT : generic_args -> genargs_or_type_path_fn
  | TYPE_PATH_FN_OPT : type_path_fn -> genargs_or_type_path_fn

with type_path_fn :=
  | TYPE_PATH_FN : option type_path_fn_inputs -> option type_no_bounds -> type_path_fn

with type_path_fn_inputs :=
  | TYPE_PATH_FN_INPUTS : list type -> type_path_fn_inputs
      (*Path*)

(*Trait and Lifetime Bounds*)
with type_param_bounds :=
  | TYPE_PARAM_BOUNDS : list type_param_bound -> type_param_bounds
  | TYPE_PARAM_BOUNDS_PLUS : list type_param_bound -> type_param_bounds

with type_param_bound :=
  | TYPE_PARAM_BOUND_LIFETIME : lifetime -> type_param_bound
  | TYPE_PARAM_BOUND_TRAIT_BOUND : trait_bound -> type_param_bound
  | TYPE_PARAM_BOUND_USE_BOUND : use_bound -> type_param_bound

with trait_bound :=
  | ENCASED_TRAIT_BOUND : option question_or_for -> type_path -> trait_bound 
  | TRAIT_BOUND : option question_or_for -> type_path -> trait_bound

with lifetime_bounds := 
  | LIFETIME_BOUNDS : list lifetime -> option lifetime -> lifetime_bounds

with lifetime :=
  | LIFETIME : str -> lifetime
  | LIFETIME_STATIC
  | LIFETIME_UNDERSCORE

with use_bound :=
  | USE_BOUND : use_bound_generic_args -> use_bound

with use_bound_generic_args :=
  | USE_BOUND_GENERIC_ARGS_EMPTY
  | USE_BOUND_GENERIC_ARGS : list use_bound_generic_arg -> use_bound_generic_arg -> use_bound_generic_args

with use_bound_generic_arg :=
  | USE_BOUND_GENERIC_ARG_LIFETIME : lifetime -> use_bound_generic_arg
  | USE_BOUND_GENERIC_ARG_IDENT : identifier -> use_bound_generic_arg
  | USE_BOUND_GENERIC_ARG_SELF

with question_or_for :=
  | QUESTION 
  | FOR_LF : for_lifetimes -> question_or_for

with for_lifetimes :=
  | FOR_LIFETIMES : generic_params -> for_lifetimes

(*Trait and Lifetime Bounds*)
(*Item*)
with item :=
  | VISITEM : list outer_attribute -> visItem -> item
  
with outer_attribute :=
  | OUTER_ATTRIBUTE : attr -> outer_attribute

with inner_attribute :=
  | INNER_ATTRIBUTE : attr -> inner_attribute

with attr :=
  | SAFE_ATTR : simple_path -> option attr_input -> attr
  | UNSAFE_ATTR : simple_path -> option attr_input -> attr

with attr_input :=
  | ATTR_INPUT_EXP : expression -> attr_input

with visItem :=
  | MODULE : module -> visItem
  | EXTERN_CRATE : extern_crate -> visItem
  | USE_DECLARATION : use_declaration -> visItem
  | FUNCTION : function -> visItem
  | TYPE_ALIAS : type_alias -> visItem
  | STRUCT : _struct -> visItem
  | ENUM : enum -> visItem
  | UNION : union -> visItem
  | CONSTANT_ITEM : constant_Item -> visItem
  | STATIC_ITEM : static_item -> visItem
  | TRAIT : trait -> visItem
  | IMPLEMENTATION : implementation -> visItem
  | EXTERN_BLOCK : extern_block -> visItem

with module :=
  | MOD_BLOCK : (is_unsafe : bool) -> identifier -> module
  | MOD_DEC : (is_unsafe : bool) -> identifier -> list  -> module

with constant :=
  | INT_LIT : str -> constant
  | FLOAT_LIT : str -> constant

with identifier :=
  | RAW_IDENT : str -> identifier
  | IDENT : str -> identifier
(*Item*)

(* Pattern *)
with pattern :=
  | PATTERN : pattern_no_top_alt -> list pattern_no_top_alt -> pattern

with pattern_no_top_alt :=
  | PATTERN_NO_RANGE : pattern_without_range -> pattern_no_top_alt
  | PATTERN_RANGE : range_pattern -> pattern_no_top_alt

with pattern_without_range :=
  | LITERAL_PATTERN : literal_expression -> pattern_without_range
  | IDENTIFIER_PATTERN : identifier_pattern -> pattern_without_range
  | WILDCARD_PATTERN : pattern_without_range
  | REST_PATTERN : rest_pattern -> pattern_without_range
  | DOUBLE_REFERENCE_PATTERN : bool -> pattern -> pattern_without_range
  | SINGLE_REFERENCE_PATTERN : bool -> pattern -> pattern_without_range
  | STRUCT_PATTERN : path_in_expression -> option struct_pattern_elements -> pattern_without_range
  | TUPLE_STRUCT_PATTERN : path_in_expression -> option (pattern * list pattern) -> pattern_without_range
  | TUPLE_PATTERN : option tuple_pattern_items -> pattern_without_range
  | GROUPED_PATTERN : pattern -> pattern_without_range
  | SLICE_PATTERN : option slice_pattern_items -> pattern_without_range
  | PATH_PATTERN : path_expression -> pattern_without_range
  | PATTERN_MACRO_INVOCATION : macro_invocation -> pattern_without_range

with slice_pattern_items :=
  | SLICE_PATTERN_ITEMS : pattern -> list pattern -> slice_pattern_items

with rest_pattern :=
  | REST_PAT

with tuple_pattern_items :=
  | PATTERN_ITEM : pattern -> tuple_pattern_items
  | REST_ITEM : rest_pattern -> tuple_pattern_items
  | PATTERN_ITEMS : pattern -> list pattern -> tuple_pattern_items

with struct_pattern_elements :=
  | STRUCT_PATTERN_ELEMENTS_FIELDS : struct_pattern_fields -> option struct_pattern_etcetara -> struct_pattern_elements
  | STRUCT_PATTERN_ELEMENTS_ETCETERA : struct_pattern_etcetara -> struct_pattern_elements

with struct_pattern_fields :=
  | STRUCT_PATTERN_FIELDS : struct_pattern_field -> list struct_pattern_field -> struct_pattern_fields

with struct_pattern_field :=
  | STRUCT_PATTERN_FIELD : list outer_attribute -> tuple_or_idPat_or_id -> struct_pattern_field

with struct_pattern_etcetara :=
  | STRUCT_PATTERN_ETCETERA : list outer_attribute -> struct_pattern_etcetara

with tuple_struct_items :=
  | TUPLE_STRUCT_ITEMS : pattern -> list pattern -> tuple_struct_items

with tuple_or_idPat_or_id :=
  | TUPLE_PAT : str -> identifier -> tuple_or_idPat_or_id
  | ID_PAT : identifier -> tuple_or_idPat_or_id
  | ID : bool -> bool -> tuple_or_idPat_or_id

with range_pattern_bound :=
  | RANGE_PATTERN_BOUND_CHAR : str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_BYTE : str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_INTEGER : option neg_sign -> str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_FLOAT : option neg_sign -> str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_PATH : path_expression -> range_pattern_bound

with range_exclusive_pattern :=
  | RangeExclusivePattern : range_pattern_bound -> range_pattern_bound -> range_exclusive_pattern

with range_pattern :=
  | RANGE_INCLUSIVE_PATTERN : range_pattern_bound -> range_pattern_bound -> range_pattern
  | RANGE_FROM_PATTERN : range_pattern_bound -> range_pattern
  | RANGE_TO_INCLUSIVE_PATTERN : range_pattern_bound -> range_pattern
  | RANGE_OBSOLETE_PATTERN : range_pattern_bound -> range_pattern_bound -> range_pattern
(* Pattern *)
(* Types *)
with type :=
  | TYPE_NO_BOUNDS : type_no_bounds -> type
  | TRAIT_TYPE : trait_object_type -> type
  | IMPL_TYPE : impl_trait_type -> type

with type_no_bounds :=
  | PARENTHESIZED_TYPE : type -> type_no_bounds
  | IMPL_ONE_BOUND : trait_bound -> type_no_bounds
  | TRAIT_ONE_BOUND : bool (*dyn*) -> trait_bound -> type_no_bounds
  | TYPE_PATH : type_path -> type_no_bounds
  | TUPLE_TYPE : tuple_type -> type_no_bounds
  | NEVER_TYPE : type_no_bounds                        (* corresponds to `!` *)
  | RAW_POINTER_TYPE : raw_pointer_type -> type_no_bounds
  | REFERENCE_TYPE : reference_type -> type_no_bounds
  | ARRAY_TYPE : type -> expr -> type_no_bounds         (* e.g., [T; N] *)
  | SLICE_TYPE : type -> type_no_bounds                (* e.g., [T] *)
  | INFERRED_TYPE : type_no_bounds                     (* `_` *)
  | QUALIFIED_PATH : qualified_path_in_type -> type_no_bounds
  | BARE_FUNCTION_TYPE : bare_function_type -> type_no_bounds
  | NO_BOUND_MACRO_INVOCATION : macro_invocation -> type_no_bounds
(* Types *)

(*Raw Pointer Type*)
with raw_pointer_type :=
  | MUT_RAW_POINTER : type_no_bounds -> raw_pointer_type
  | CONST_RAW_POINTER : type_no_bounds -> raw_pointer_type
(*Raw Pointer Type*)

(* Pointer Type*)
with reference_type :=
  | REFER_TYP : option lifetime -> bool (*mut*) -> type_no_bounds -> reference_type
(*Pointer Type*)

(*Function Pointer Type*)

with bare_function_type :=
  | BARE_FUNC_TYPE : option for_lifetimes -> function_type_qualifiers -> 
      option function_parameters_maybe_named_variadic ->
      option bare_function_return_type -> bare_function_type

with function_type_qualifiers :=
  | FUNC_QUAL_NONE : function_type_qualifiers
  | FUNC_QUAL_UNSAFE : function_type_qualifiers
  | FUNC_QUAL_EXTERN : option abi -> function_type_qualifiers
  | FUNC_QUAL_UNSAFE_EXTERN : option abi -> function_type_qualifiers

with bare_function_return_type :=
  | BARE_RETURN_TYPE : type_no_bounds -> bare_function_return_type

with function_parameters_maybe_named_variadic :=
  | FN_PARAMS_NORMAL : maybe_named_function_parameters -> function_parameters_maybe_named_variadic
  | FN_PARAMS_VARIADIC : maybe_named_function_parameters_variadic -> function_parameters_maybe_named_variadic

with maybe_named_function_parameters :=
  | MAYBE_NAMED_FN_PARAMS : list maybe_named_param -> maybe_named_function_parameters

with maybe_named_param :=
  | MAYBE_NAMED_PARAM : list outer_attribute -> option id_or_underscore -> type -> maybe_named_param

with id_or_underscore :=
  | ID_OPT : identifier -> id_or_underscore
  | UNDERSCORE_OPT

with maybe_named_function_parameters_variadic :=
  | FN_PARAMS_VAR : list maybe_named_param ->  list outer_attribute  -> maybe_named_function_parameters_variadic
(*Function Pointer Type*)
(*Impl trait*)
with impl_trait_type :=
  | IMPL_TRAIT_TYPE : type_param_bounds -> impl_trait_type
(*Impl trait*)

(*Trait Object*)
with trait_object_type :=
  | TRAIT_OBJECT_TYPE : bool (*dyn*) -> type_param_bounds -> trait_object_type
(*Trait Object*)
(*Tuple Types*)
with tuple_type :=
  | TUPLE_TYPE_EMPTY
  | TUPLE_TYPE_FULL : list type -> tuple_type
(*Tuple Types*)

(*Visibility*)
with visibility :=
  | PUB 
  | PUB_CRATE
  | PUB_SUPER 
  | PUB_SELF
  | PUB_IN : simple_path -> visibility
(*Visibility*)

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".

