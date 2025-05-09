From Stdlib Require Import String.
From Stdlib Require Import Ascii.


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
  | GROUPED_EXPRESSION_ : expression -> grouped_expression
      (* Grouped Expressions *)
      (* Array and Index Expression*)
with array_expression :=
  | ARRAY_EXPRESSION_ : list array_elements -> array_expression

with array_elements :=
  | ARRAY_ElEMENT : list expression -> array_elements
  | SEMI_ARRAY_ELEMENT : expression -> expression -> array_elements

with index_expression :=
  | INDEX_EXPRESSION_ : expression -> expression -> index_expression

      (* Array and Index Expression*)
      (*Tuple and Tuple Indexing Expressions*)
with tuple_expression :=
  | TUPLE_EXPRESSION_ : option tuple_elements -> tuple_expression

with tuple_elements :=
  | TUPLE_ELEMENTS : list expression -> option expression -> tuple_elements

with tuple_indexing_expression :=
  | TUPLE_INDEXING_EXPRESSION_ : expression -> str (*tuple index is equivalent to string, for now...*) -> tuple_indexing_expression
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
  | CALL_EXPRESSION_ : expression -> call_params -> call_expression

with call_params :=
  | CALL_PARAMS : expression -> list expression -> call_params
      (*  Call Expressions *)
      (*  Method Expressions *)
with method_call_expression :=
  | METHOD_CALL_EXPRESSION_ : expression -> path_expr_segment -> option call_params -> method_call_expression
      (*  Method Expressions *)
      (*  Field Expressions *)
with field_expression :=
  | FIELD_EXPRESSION_ : expression -> identifier -> field_expression
      (*  Field Expressions *)
      (*  Closure Expressions *)
      with closure_expression :=
  | CLOSURE_EXPRESSION_ : bool -> bool -> option closure_params -> expr_or_type_no_bounds -> closure_expression

with expr_or_type_no_bounds :=
  | EXPR_OPT : expression -> expr_or_type_no_bounds
  | TYPE_BLOCK_OPT : type_no_bounds -> block_expression -> expr_or_type_no_bounds

with closure_params :=
  | CLOSURE_PARAMS : closure_param -> list closure_param -> closure_params

with closure_param :=
  | CLOSURE_PARAM : list outer_attribute -> pattern_no_top_alt -> option type_ -> closure_param
      (*  Closure Expressions *)
      (*  Loop Expressions *)
with loop_expression :=
  | LOOP_EXPRESSION_ : option loop_label -> loop_types -> loop_expression

with loop_label :=
  | LOOP_LABEL : str -> loop_label

with break_expression :=
  | BREAK_EXPRESSION_: option str -> option expression -> break_expression

with continue_expression :=
  | CONTINUE_EXPRESSION_ : option str -> continue_expression

with loop_types :=
  | INFINITE_LOOP_EXPRESSION : block_expression -> loop_types
  | PREDICATE_LOOP_EXPRESSION : expression -> block_expression -> loop_types
  | PREDICATE_PATTERN_LOOP_EXPRESSION : pattern -> scrutinee -> block_expression -> loop_types
  | ITERATOR_LOOP_EXPRESSION : pattern -> expression -> block_expression -> loop_types
  | LABEL_BLOCK_EXPRESSION : block_expression -> loop_types
      (*  Loop Expressions *)
      (* Range Expressions*)
with range_expression :=
  | RANGE_EXPRESSION_: expression -> expression -> range_expression
  | RANGE_FROM_EXPRESSION_: expression -> range_expression
  | RANGE_TO_EXPRESSION_: expression -> range_expression
  | RANGE_FULL_EXPRESSION_: range_expression
  | RANGE_INCLUSIVE_EXPRESSION_: expression -> expression -> range_expression
  | RANGE_TO_INCLUSIVE_EXPRESSION_: expression -> range_expression
      (* Range Expressions *)
      (* if and if let expressions *)
with if_expression :=
  | IfExpr : expression -> block_expression -> option choice -> if_expression

with choice :=
  | BLOCK : block_expression -> choice 
  | IF : if_expression -> choice 
  | IF_LET : if_let_expression -> choice

with if_let_expression :=
  | IF_LET_EXPRESSION_: pattern -> scrutinee -> block_expression -> option choice -> if_let_expression

      (* if and if let expressions *)
      (*Match expressions*)
with match_expression :=
  | MATCH_EXPRESSION_: scrutinee -> list inner_attribute -> option match_arms -> match_expression

with scrutinee :=
  | SCRUTINEE : expression -> scrutinee

with match_arm_guard :=
  | MATCH_ARM_GUARD : expression -> match_arm_guard

with match_arm :=
  | MATCH_ARM : list outer_attribute -> pattern -> option match_arm_guard -> match_arm

with match_arms :=
  | MATCH_ARMS : list organized_matching -> match_arm -> expression -> match_arms

with organized_matching :=
  | ORGANIZE : match_arm -> block_or_not -> organized_matching

with block_or_not :=
  | YES_BLOCK : type_expr_with_block -> block_or_not
  | NO_BLOCK : type_expr_without_block -> block_or_not
      (*Match expressions*)
      (*Return expressions*)
with return_expression :=
  | RETURN_EXPRESSION_: option expression -> return_expression
      (*Return expressions*)
with await_expression :=
  | AWAIT_EXPRESSION_: expression -> await_expression
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
  | PATH_EXPRESSION_ : path_in_expression -> path_expression
  | QUALIFIED_PATH_IN_EXPRESSION : qualified_path_in_expression -> path_expression
(* Path Expressions *)

(* Block Expressions*)
with block_expression := 
  | BLOCK_EXPRESSION_: list inner_attribute -> option statements -> block_expression

with statements :=
  | STATEMENTS : list statement -> statements
  | STATEMENTS_EXPR_WITHOUT_BLOCK : list statement -> type_expr_without_block -> statements
  | S_EXPR_WITHOUT_BLOCK : type_expr_without_block -> statements

with async_block_expression :=
  | ASYNC_BLOCK_EXPR_MOVE : block_expression -> async_block_expression
  | ASYNC_BLOCK_EXPR_STILL : block_expression -> async_block_expression

with const_block_expression :=
  | CONST_BLOCK_EXPRESSION_: block_expression -> const_block_expression

with unsafe_block_expression :=
  | UNSAFE_BLOCK_EXPRESSION_: block_expression -> unsafe_block_expression
(*Block Expressions*)

(*Statements*)
with statement :=
  | STATEMENT_ITEM : item -> statement
  | STATEMENT_LET : let_statement -> statement
  | STATEMENT_EXPRESSION_: expr_statement -> statement
  | STATEMENT_MACRO_INVOCATION_SEMI : macro_invocation_semi -> statement

with let_statement :=
  | LET_STATEMENT : list outer_attribute -> pattern_no_top_alt -> option type_ -> option eq_expr -> let_statement

with eq_expr :=
  | EQ_EXPRESSION_: expression -> option block_expression -> eq_expr

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
  | TOKEN : str -> token_tree (*I believe all tokens can be represented as string, especially demonstrated by pretty printer*) 
  | DELIM : delim_token_tree -> token_tree

with macro_invocation_semi :=
  | PAREN_MACRO : list token_tree -> macro_invocation_semi
  | BRACE_MACRO : list token_tree -> macro_invocation_semi
  | BRACK_MACRO : list token_tree -> macro_invocation_semi
      (*Macros*)
      (*Macros by Example*)
with macro_rules_definition :=
  | MACRO_RULES_DEF :
      identifier ->
      macro_rules_def ->
      macro_rules_definition

with macro_rules_def :=
  | MACRO_RULES_PARENS : macro_rules -> macro_rules_def
  | MACRO_RULES_BRACKETS : macro_rules -> macro_rules_def
  | MACRO_RULES_BRACES : macro_rules -> macro_rules_def

with macro_rules :=
  | MACRO_RULES : list macro_rule -> macro_rules

with macro_rule :=
  | MACRO_RULE : macro_matcher -> macro_transcriber -> macro_rule

with macro_matcher :=
  | MATCHER_PARENS   : list macro_match -> macro_matcher
  | MATCHER_BRACKETS : list macro_match -> macro_matcher
  | MATCHER_BRACES   : list macro_match -> macro_matcher

with macro_match :=
  | MM_TOKEN : str -> macro_match
  | MM_NESTED : macro_matcher -> macro_match
  | MM_FRAGMENT : identifier -> macro_frag_spec -> macro_match
  | MM_REPEAT : list macro_match -> option macro_rep_sep -> macro_rep_op ->  macro_match

with macro_frag_spec :=
  | FRAG_BLOCK
  | FRAG_EXPR
  | FRAG_EXPR_2021
  | FRAG_IDENT
  | FRAG_ITEM
  | FRAG_LIFETIME
  | FRAG_LITERAL
  | FRAG_META
  | FRAG_PAT
  | FRAG_PAT_PARAM
  | FRAG_PATH
  | FRAG_STMT
  | FRAG_TT
  | FRAG_TYPE
  | FRAG_VIS

with macro_rep_sep :=
  | MACRO_REP_SEP : str -> macro_rep_sep    

with macro_rep_op :=
  | REP_ZERO_OR_MORE    
  | REP_ONE_OR_MORE     
  | REP_ZERO_OR_ONE     

with macro_transcriber :=
  | MACRO_TRANSCRIBE : delim_token_tree -> macro_transcriber
      (*Macros by Example*)
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
  | NEG_EXPRESSION_: expression -> negation_expression
  | NOT_EXPRESSION_: expression -> negation_expression

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
  | QUALIFIED_PATH_IN_EXPRESSION_ : qualified_path_type -> list path_expr_segment -> qualified_path_in_expression

with qualified_path_type :=
  | QUALIFIED_PATH_TYPE : type_ -> option type_path -> qualified_path_type

with qualified_path_in_type :=
  | QUALIFIED_PATH_IN_TYPE : qualified_path_type -> list type_path_segment  -> qualified_path_in_type

with generic_args :=
  | EMPTY_GENERIC_ARGS 
  | GENERIC_ARGS : list generic_arg -> generic_arg -> generic_args

with generic_arg :=
  | GENERIC_ARG_LIFETIME : lifetime -> generic_arg
  | GENERIC_ARG_TYPE : type_ -> generic_arg
  | GENERIC_ARG_CONST : generic_args_const -> generic_arg
  | GENERIC_ARGS_BINDING : generic_args_binding -> generic_arg
  | GENERIC_ARGS_BOUNDS : generic_args_bounds -> generic_arg

with generic_args_const :=
  | GENERIC_ARGS_CONST_BLOCK : block_expression -> generic_args_const
  | GENERIC_ARGS_CONST_LIT : literal_expression -> generic_args_const
  | NEG_GENERIC_ARGS_CONST_LIT : literal_expression -> generic_args_const
  | GENERIC_ARGS_CONST_SIMPLE_PATH_SEG : simple_path_segment -> generic_args_const

with generic_args_binding :=
  | GENERIC_ARGS_BINDING_: identifier -> option generic_args -> type_ -> generic_args_binding

with generic_args_bounds :=
  | GENERIC_ARGS_BOUNDS_ : identifier -> option generic_args -> type_param_bounds -> generic_args_bounds

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
  | TYPE_PATH_FN_INPUTS : list type_ -> type_path_fn_inputs
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
  | MACRO_ITEM: macro_item -> item

with macro_item :=
  | MACRO_INVOCATION_SEMI : macro_invocation_semi -> macro_item
  | MACRO_RULES_DEFINITION : macro_rules_definition -> macro_item
 
with visItem :=
  | MODULE : module_ -> visItem
  | EXTERN_CRATE : extern_crate -> visItem
  | USE_DECLARATION : use_declaration -> visItem
  | FUNCTION : function -> visItem
  | TYPE_ALIAS : type_alias -> visItem
  | STRUCT : struct_ -> visItem
  | ENUM : enumeration -> visItem
  | UNION : union -> visItem
  | CONSTANT_ITEM : constant_item -> visItem
  | STATIC_ITEM : static_item -> visItem
  | TRAIT : trait -> visItem
  | IMPLEMENTATION : implementation -> visItem
  | EXTERN_BLOCK : extern_block -> visItem


with constant :=
  | INT_LIT : str -> constant
  | FLOAT_LIT : str -> constant
(*Item*)
(*Struct*)

with struct_ :=
  | STRUCT_STRUCT : 
      identifier -> 
      option generic_params -> 
      option where_clause -> 
      struct_struct_body -> 
      struct_

  | STRUCT_TUPLE : 
      identifier -> 
      option generic_params -> 
      option tuple_fields -> 
      option where_clause -> 
      struct_

with struct_struct_body :=
  | STRUCT_BODY_BRACED : option struct_fields -> struct_struct_body
  | STRUCT_BODY_SEMI

with struct_fields :=
  | STRUCT_FIELDS : list struct_field -> struct_fields

with struct_field :=
  | STRUCT_FIELD : 
      list outer_attribute -> 
      option visibility -> 
      identifier -> 
      type_ -> 
      struct_field

with tuple_fields :=
  | TUPLE_FIELDS : list tuple_field -> tuple_fields

with tuple_field :=
  | TUPLE_FIELD :
      list outer_attribute ->
      option visibility ->
      type_ ->
      tuple_field
(*Struct*)
(*Enum*)

with enumeration :=
  | ENUM_ :
      identifier ->
      option generic_params ->
      option where_clause ->
      option enum_items ->
      enumeration

with enum_items :=
  | ENUM_ITEMS : list enum_item -> enum_items

with enum_item :=
  | ENUM_ITEM :
      list outer_attribute ->
      option visibility ->
      identifier ->
      option enum_item_kind ->
      option enum_item_discriminant ->
      enum_item

with enum_item_kind :=
  | ENUM_ITEM_TUPLE : option tuple_fields -> enum_item_kind
  | ENUM_ITEM_STRUCT : option struct_fields -> enum_item_kind

with enum_item_discriminant :=
  | ENUM_DISCRIMINANT : expression -> enum_item_discriminant
(*Enum*)
(*Union*)
with union :=
  | UNION_ : identifier -> option generic_params -> option where_clause -> option struct_fields -> union
(*Union*)
(*Constant Item*)
with constant_item :=
  | CONSTANT_ITEM_ : ident_or_whitespace -> type_ -> option expression -> constant_item

with ident_or_whitespace :=
  | IDENT_OPT : identifier -> ident_or_whitespace
  | UNDERSCORE_OP
(*Constant Item*)
(*Type Alias*)
with type_alias :=
  | TYPE_ALIBI : identifier -> option generic_params -> 
      option type_param_bounds -> option where_clause -> 
      option type_and_where -> type_alias

with type_and_where :=
  | TYPE_AND_WHERE : type_ -> where_clause -> type_and_where
  | TYPE_OPT : type_ -> type_and_where
(*Type Alias*)
(*Static Item*)
with static_item :=
  | STATIC_ITEM_ : option item_safety -> bool -> identifier -> type_ -> option expression -> static_item
(*Static Item*)
(*Traits*)
with trait :=
  | TRAIT_ : bool -> identifier -> option generic_params -> option type_param_bounds ->
      option where_clause -> list inner_attribute -> list associated_item -> trait
(*Traits*)
(*Implementations *)
with implementation :=
  | INHERENT_IMPL :
      option generic_params ->
      type_ ->
      option where_clause ->
      list inner_attribute ->
      list associated_item ->
      implementation

  | TRAIT_IMPL :
      bool ->                    (* is_unsafe *)
      option generic_params ->
      bool ->                    (* is_negative (i.e., `!`) *)
      type_path ->              (* the trait being implemented *)
      type_ ->                   (* the type it's implemented for *)
      option where_clause ->
      list inner_attribute ->
      list associated_item ->
      implementation
(*Implementations*)
(*Extern block*)
with extern_block :=
  | EXTERN_BLOCK_ :
      bool ->                    (* is_unsafe *)
      option abi ->             (* optional ABI string *)
      list inner_attribute ->
      list external_item ->
      extern_block

with external_item :=
  | EXTERNAL_MACRO :
      list outer_attribute ->
      macro_invocation ->
      external_item
  | EXTERNAL_STATIC_OR_FN :
      list outer_attribute ->
      option visibility ->
      external_decl ->
      external_item

with external_decl :=
  | EXT_STATIC : static_item -> external_decl
  | EXT_FUNCTION : function -> external_decl
(*Extern block*)
(*Generic Params*)
with generic_params :=
  | GENERIC_PARAMS_EMPTY
  | GENERIC_PARAMS :
      list generic_param ->
      generic_params

with generic_param :=
  | GP_LIFETIME : list outer_attribute -> lifetime_param -> generic_param
  | GP_TYPE     : list outer_attribute -> type_param -> generic_param
  | GP_CONST    : list outer_attribute -> const_param -> generic_param

with lifetime_param :=
  | LIFETIME_PARAM :
      lifetime ->
      option lifetime_bounds ->
      lifetime_param

with type_param :=
  | TYPE_PARAM :
      identifier ->
      option type_param_bounds ->
      option type_ ->              (* default value, if any *)
      type_param

with const_param :=
  | CONST_PARAM :
      identifier ->
      type_ ->
      option const_default ->
      const_param

with const_default :=
  | CONST_DEFAULT_BLOCK : block_expression -> const_default
  | CONST_DEFAULT_IDENT : identifier -> const_default
  | CONST_DEFAULT_LIT   : literal_expression -> const_default
  | CONST_DEFAULT_NEG_LIT : literal_expression -> const_default  (* for -N literals *)

with where_clause :=
  | WHERE_CLAUSE : list where_clause_item -> where_clause

with where_clause_item :=
  | WC_LIFETIME :
      lifetime ->
      lifetime_bounds ->
      where_clause_item

  | WC_TYPE :
      option for_lifetimes ->
      type_ ->
      option type_param_bounds ->
      where_clause_item
(*Generic Params*)
(*Associated item*)

with associated_item :=
  | ASSOC_MACRO :
      list outer_attribute ->
      macro_invocation ->
      associated_item

  | ASSOC_ITEM :
      list outer_attribute ->
      option visibility ->
      assoc_decl ->
      associated_item

with assoc_decl :=
  | ASSOC_TYPE_ALIAS : type_alias -> assoc_decl
  | ASSOC_CONST_ITEM : constant_item -> assoc_decl
  | ASSOC_FUNCTION   : function -> assoc_decl

(*Associated item*)
(* Function *)
with function :=
  | FUNCTION_DEF :
      function_qualifiers ->
      identifier ->
      option generic_params ->
      option function_parameters ->
      option function_return_type ->
      option where_clause ->
      function_body ->
      function

with function_qualifiers :=
  | FUNCTION_QUALIFIERS :
      bool ->           (* is_const *)
      bool ->           (* is_async *)
      item_safety ->
      option abi ->
      function_qualifiers

with item_safety :=
  | SAFE
  | UNSAFE

with abi :=
  | ABI_STRING : list char_code -> abi

with function_parameters :=
  | FN_PARAMS_SELF : self_param -> function_parameters
  | FN_PARAMS_FULL :
      option self_param ->
      list function_param ->
      function_parameters

with self_param :=
  | SELF_SHORT : list outer_attribute -> shorthand_self -> self_param
  | SELF_TYPED : list outer_attribute -> typed_self -> self_param

with shorthand_self :=
  | SELF_SHORTHAND : bool -> shorthand_self
  | SELF_SHORTHAND_REF : bool -> shorthand_self
  | SELF_SHORTHAND_REF_LIFE : lifetime -> bool -> shorthand_self

with reference_modifier :=
  | REF_LF : option lifetime -> reference_modifier  (* & or &'a *)
  | REF : reference_modifier

with typed_self :=
  | TYPED_SELF : bool -> type_ -> typed_self

with function_param :=
  | FN_PARAM_PATTERN : list outer_attribute -> function_param_pattern -> function_param
  | FN_PARAM_DOTS : list outer_attribute -> function_param
  | FN_PARAM_TYPE : list outer_attribute -> type_ -> function_param

with function_param_pattern :=
  | FN_PARAM_PAT : pattern_no_top_alt -> function_param_pattern_rhs -> function_param_pattern

with function_param_pattern_rhs :=
  | FNPAT_TYPE : type_ -> function_param_pattern_rhs
  | FNPAT_DOTS : function_param_pattern_rhs

with function_return_type :=
  | FN_RETURN_TYPE : type_ -> function_return_type

with function_body :=
  | FN_BODY_BLOCK : block_expression -> function_body
  | FN_BODY_SEMI
(* Function *)
(* Modules *)
with module_ :=
  | MOD_BLOCK : bool -> identifier -> module_
  | MOD_DEC : bool -> identifier -> list inner_attribute -> list item -> module_

(* Modules *)
(*Extern Crate*)
with extern_crate :=
  | EXT_CRATE_CLAUSE : crate_ref -> option as_clause -> extern_crate
with crate_ref :=
  | ID_CRATE_REF : identifier -> crate_ref
  | SELF_CRATE_REF : crate_ref

with as_clause :=
  | ID_AS_CLAUSE : identifier -> as_clause
  | UNDERSCORE_AS_CLAUSE : as_clause

(*Extern Crate*)
(* Use Declaration*)
with use_declaration :=
  | USE_DECL : use_tree -> use_declaration

with use_tree :=
  | USE_TREE : option simple_path -> use_tree
  | USE_TREE_LIST : option simple_path -> list use_tree -> use_tree
  | USE_TREE_ID : simple_path -> option id_or_underscore -> use_tree

(* Use Declaration*)
(*Identifier*)
with identifier :=
  | RAW_IDENT : str -> identifier
  | IDENT : str -> identifier
(*Identifier*)

(*Attributes*)
with outer_attribute :=
  | OUTER_ATTRIBUTE : attr -> outer_attribute

with inner_attribute :=
  | INNER_ATTRIBUTE : attr -> inner_attribute

with attr :=
  | SAFE_ATTR : simple_path -> option attr_input -> attr
  | UNSAFE_ATTR : simple_path -> option attr_input -> attr

with attr_input :=
  | ATTR_INPUT_EXP : expression -> attr_input
(*Attributes*)

(* Pattern *)
with pattern :=
  | PATTERN : pattern_no_top_alt -> list pattern_no_top_alt -> pattern

with pattern_no_top_alt :=
  | PATTERN_NO_RANGE : pattern_without_range -> pattern_no_top_alt
  | PATTERN_RANGE : range_pattern -> pattern_no_top_alt

with pattern_without_range :=
  | LITERAL_PATTERN : literal_expression -> pattern_without_range
  | IDENTIFIER_PATTERN : bool -> bool -> identifier -> option pattern_no_top_alt -> pattern_without_range
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

with tuple_or_idPat_or_id :=
  | TUPLE_PAT : str -> identifier -> tuple_or_idPat_or_id
  | ID_PAT : identifier -> tuple_or_idPat_or_id
  | ID : bool -> bool -> tuple_or_idPat_or_id

with struct_pattern_etcetara :=
  | STRUCT_PATTERN_ETCETERA : list outer_attribute -> struct_pattern_etcetara

with tuple_struct_items :=
  | TUPLE_STRUCT_ITEMS : pattern -> list pattern -> tuple_struct_items

with range_pattern_bound :=
  | RANGE_PATTERN_BOUND_CHAR : str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_BYTE : str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_NEG_INTEGER : str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_INTEGER : str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_NEG_FLOAT : str -> range_pattern_bound
  | RANGE_PATTERN_BOUND_FLOAT : str -> range_pattern_bound
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
with type_ :=
  | TYPE_NO_BOUNDS : type_no_bounds -> type_
  | TRAIT_TYPE : trait_object_type -> type_
  | IMPL_TYPE : impl_trait_type -> type_

with type_no_bounds :=
  | PARENTHESIZED_TYPE : type_ -> type_no_bounds
  | IMPL_ONE_BOUND : trait_bound -> type_no_bounds
  | TRAIT_ONE_BOUND : bool (*dyn*) -> trait_bound -> type_no_bounds
  | TYPE_PATH : type_path -> type_no_bounds
  | TUPLE_TYPE : tuple_type -> type_no_bounds
  | NEVER_TYPE : type_no_bounds                        (* corresponds to `!` *)
  | RAW_POINTER_TYPE : raw_pointer_type -> type_no_bounds
  | REFERENCE_TYPE : reference_type -> type_no_bounds
  | ARRAY_TYPE : type_ -> expression -> type_no_bounds         (* e.g., [T; N] *)
  | SLICE_TYPE : type_ -> type_no_bounds                (* e.g., [T] *)
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
  | REFER_TYP : option lifetime -> bool(*mut*) -> type_no_bounds -> reference_type
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
  | MAYBE_NAMED_PARAM : list outer_attribute -> option id_or_underscore -> type_ -> maybe_named_param

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
  | TUPLE_TYPE_FULL : list type_ -> tuple_type
(*Tuple Types*)

(*Visibility*)
with visibility :=
  | PUB 
  | PUB_CRATE
  | PUB_SUPER 
  | PUB_SELF
  | PUB_IN : simple_path -> visibility.
(*Visibility*)

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".

