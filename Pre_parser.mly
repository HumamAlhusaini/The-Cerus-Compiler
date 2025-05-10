%{
  open Cabs
    %}

%token <Cabs.loc> AS BREAK CONST CONTINUE CRATE
%token <Cabs.loc> ELSE ENUM EXTERN FALSE FN
%token <Cabs.loc> FOR IF IMPL IN LET
%token <Cabs.loc> LOOP MATCH MOD MOVE MUT
%token <Cabs.loc> PUB REF RETURN 
%token <Cabs.loc> SELFVALUE SELFTYPE STATIC STRUCT SUPER
%token <Cabs.loc> TRAIT TRUE TYPE UNSAFE USE
%token <Cabs.loc> WHERE WHILE ASYNC AWAIT DYN 
%token <Cabs.loc> MACRO_RULES UNION STATICLIFETIME SAFE RAW

%token <Cabs.loc> PLUS MINUS STAR SLASH PERCENT    (* + - * / % *)
%token <Cabs.loc> CARET NOT AND OR ANDAND OROR      (* ^ ! & | && || *)
%token <Cabs.loc> SHL SHR                          (* << >> *)
%token <Cabs.loc> EQ PLUSEQ MINUSEQ STAREQ SLASHEQ  (* = += -= *= /= *)
%token <Cabs.loc> PERCENTEQ CARETEQ ANDEQ OREQ SHLEQ SHREQ (* %= ^= &= |= <<= >>= *)
%token <Cabs.loc> EQEQ NE LT GT LE GE              (* == != < > <= >= *)
%token <Cabs.loc> AT UNDERSCORE DOT DOTDOT DOTDOTDOT DOTDOTEQ (* @ _ . .. ... ..= *)
%token <Cabs.loc> COMMA SEMI COLON RESERVED_RAW_IDENTIFIER    (* , ; : *)
%token <Cabs.loc> PATHSEP RARROW FATARROW LARROW              (* :: -> => <- *)
%token <Cabs.loc> POUND DOLLAR QUESTION TILDE  DOLLAR_CRATE              (* # $ ? ~ *)
%token <Cabs.loc> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN

%token <string * Cabs.loc> IDENT RAW_IDENT
%token <(int64 list * Cabs.loc)> STRING_LIT RAW_STRING_LIT BYTE_STRING RAW_BYTE_STRING RAW_C_STRING C_STRING
%token <(int64 * Cabs.loc)> CHAR_LIT BYTE
%token <(Cabs.constant * Cabs.loc)> CONSTANT

%token EOF

%type <Cabs.item list> items
%type <Cabs.item> item
%type <Cabs.visItem> vis_item
%type <Cabs.extern_crate> extern_crate
%type <Cabs.crate_ref> crate_ref
%type <Cabs.as_clause option> as_clause
%type <Cabs.module_> unsafe_module safe_module
%type <(Cabs.identifier * Cabs.loc)> ident
%type <Cabs.outer_attribute list> outer_attrs
%type <Cabs.outer_attribute> outer_attr
%type <Cabs.inner_attribute list> inner_attrs
%type <Cabs.inner_attribute> inner_attr
%type <Cabs.attr> attr 
%type <Cabs.simple_path> simple_path
%type <Cabs.simple_path_segment> simple_path_segment
%type <Cabs.attr_input> attr_input
%type <Cabs.attr_input option> maybe_attr_input
%type <Cabs.expression> expression
%type <Cabs.type_expr_without_block> expression_no_block

%nonassoc LOWER
%nonassoc HIGHER
%right PATHSEP

%start<Cabs.item list> program
%%

program:
  | e = items EOF { e }

items:
  | /* empty */ { [] }
  | i = item rest = items { i :: rest }

item:
  | attrs = outer_attrs v = vis_item { Cabs.VISITEM (attrs, v) }

vis_item:
  | m = unsafe_module { Cabs.MODULE m }
  | e = extern_crate { Cabs.EXTERN_CRATE e }
  | decl = use_declaration { Cabs.USE_DECLARATION decl}

(*generic params*)
generic_params:
  | LT GT { GENERIC_PARAMS_EMPTY }
  | LT separated_nonempty_list(COMMA, generic_param) GT { GENERIC_PARAMS $2 }

generic_param:
  | outer_attrs lifetime_param {GP_LIFETIME ($1, $2)}
  | outer_attrs type_param {GP_TYPE ($1, $2)}
  | outer_attrs const_param {GP_CONST ($1, $2)}

lifetime_param:
  | lifetime option(col_life_bounds) {LIFETIME_PARAM($1,$2)}

col_life_bounds:
  | COLON lifetime_bounds { $2 }

type_param:
  | ident option(col_param_bounds) option(eq_typ) {TYPE_PARAM ($1, $2, $3) }

eq_typ:
  | EQ typ {$2}

col_param_bounds:
  | COLON option(type_param_bounds) { $2 }

const_param:
  | CONST ident COLON typ option(const_param_body) { CONST_PARAM ($2, $3, $4)}

const_param_body:
  | EQ block_expression { $2 }
  | ident { $1 }
  | literal_expression { $1 }

(*generic params*)
(*Functions*)

func:
  | function_qualifiers FN ident option(generic_params) LPAREN option(function_params) RPAREN 
option(function_return_type) option(where_clause) function_body { FUNCTION_DEF ($1, $3, $5, $7, $8, $9) }

function_body:
  | block_expression { FN_BODY_BLOCK $1 }
  | SEMI { FN_BODY_SEMI }

function_qualifiers: 
  | const = is_const async = is_async unsafe = is_unsafe opt = option(ex_op) 
    { FUNCTION_QUALIFIERS (const, async, unsafe, opt)}

ex_op:
  | EXTERN a = abi { a }
  | EXTERN { }

abi:
  | RAW_STRING_LIT { ABI_STRING $1 }
  | STRING_LIT { ABI_STRING $1 }

function_params:
  | self_param option(COMMA) { FN_PARAMS_SELF $1 }
  | option(self_param) option(COMMA) separated_nonempty_list(terminated(function_param,COMMA)) option(COMMA) 
      { FN_PARAMS_FULL ($1, $2) }

self_param:
  | outer_attrs shorthand_self { SELF_SHORT ($1, $2)}
  | outer_attrs typed_self { SELF_TYPED ($1, $2)}

shorthand_self:
  | AND is_mut SELFVALUE { SELF_SHORTHAND_REF $2 }
  | AND lifetime is_mut SELFVALUE { SELF_SHORTHAND_REF_LIFE ($2 $3)}
  | is_mut SELFVALUE { SELF_SHORTHAND $1 }

typed_self:
  | is_mut SELFVALUE COLON typ { TYPED_SELF ($1, $4)}

function_param:
  | outer_attrs DOTDOTDOT { FN_PARAM_DOTS $1 }
  | outer_attrs function_param_pattern { FN_PARAM_PATTERN ($1, $2) }
  | outer_attrs typ { FN_PARAM_TYPE ($1, $2) }

function_param_pattern:
  | pattern_no_top_alt COLON typ { FN_PARAM_PAT ($1, $2) }
  | pattern_no_top_alt COLON DOTDOTDOT { FN_PARAM_DOTDOTDOT $1 }

function_return_type:
  | LARROW typ { FN_RETURN_TYPE $2 }
 (* Functions*)
(*Patterns*)
pattern:
  | OR separated_nonempty_list(OR, pattern_no_top_alt) { PATTERN $2 }

pattern_no_top_alt:
  | no_range_pattern { PATTERN_NO_RANGE $1 } 
  | range_pattern { PATTERN_RANGE $1 }

no_range_pattern:
  | literal_pattern { LITERAL_PATTERN $1 }
  | identifier_pattern { $1 }
  | UNDERSCORE { WILDCARD_PATTERN }
  | DOTDOT { REST_PATTERN }
  | AND is_mut pattern_without_range { SINGLE_REFERENCE_PATTERN $1 }
  | ANDAND is_mut pattern_without_range { DOUBLE_REFERENCE_PATTERN $1 }
  | struct_pattern { $1 }
  | tuple_struct_pattern { $1 }
  | tuple_pattern { TUPLE_PATTERN $1 }
  | LPAREN pattern RPAREN { GROUPED_PATTERN $2 }
  | slice_pattern { SLICE_PATTERN $1 }
  | path_expression { PATH_PATTERN $1 }

literal_pattern:
  | FALSE { Cabs.FALSE_PAT }
  | TRUE { Cabs.TRUE_PAT }
  | CHAR_LIT { CHAR_LITERAL $1 }
  | BYTE_STRING { Cabs.BYTE_STRING $1 }
  | STRING_LIT { Cabs.STRING_LITERAL $1 }
  | RAW_BYTE_STRING { Cabs.RAW_BYTE_STRING_LITERAL $1 }
  | RAW_STRING_LIT { Cabs.RAW_STRING_LITERAL $1 }
  | C_STRING { Cabs.C_STRING_LITERAL $1 }
  | RAW_C_STRING { Cabs.C_STRING_LITERAL $1 }
  | neg INT_LIT { Cabs.INTEGER_LITERAL ($1, $2) }
  | neg FLOAT_LIT { Cabs.FLOAT_LITERAL ($1, $2) }

neg:
  | MINUS { true }
  | { false }

identifier_pattern:
  | is_ref is_mut ident option(pat_at)
{ IDENTIFIER_PATTERN ($1, $2, $3) }

pat_at:
  | AT pattern_no_top_alt { $2 }

struct_pattern:
  | path_in_expression LBRACK option(struct_pattern_elements) RBRACK { STRUCT_PATTERN ($1, $3)}

struct_pattern_elements:
  | struct_pattern_fields option(comma_or_etcetera) 
{ STRUCT_PATTERN_ELEMENTS_FIELDS ($1, $2)}
  | struct_pattern_etcetara { STRUCT_PATTERN_ELEMENTS_ETCETERA $1 }

comma_or_etcetera:
  | COMMA struct_pattern_etcetara { Some $2 }
  | COMMA { None }

struct_pattern_fields:
  | separated_nonempty_list(COMMA, struct_pattern_field) { STRUCT_PATTERN_FIELDS $1 }

struct_pattern_field:
  | outer_attrs struct_pattern_field_body { STRUCT_PATTERN_FIELD ($1, $2)}

struct_pattern_field_body:
  | INT_LIT COLON pattern { TUPLE_PAT ($1, $3) }
  | ident COLON pattern {ID_PAT ($1, $3) }
  | is_ref is_mut ident { Cabs.ID ($1, $2, $3)}

struct_pattern_etcetara:
  | outer_attrs DOTDOT { STRUCT_PATTERN_ETCETERA $1 }

tuple_struct_pattern:
  | path_in_expression LPAREN option(tuple_struct_items) RPAREN 
  { TUPLE_STRUCT_PATTERN ($1, $3)}

tuple_struct_items:
  | tuple_struct_item_list option(COMMA) { Stdlib.List.rev $1 }

tuple_struct_item_list:
  | pattern { [$1] }
  | pattern COMMA pattern { $3 :: $1 }

tuple_pattern:
  | LPAREN option(tuple_pattern_items) RPAREN { $2 }

tuple_pattern_items:
  | pattern COMMA { PATTERN_ITEM $1 }
  | rest_pattern { REST_ITEM $1 }
  | separated_nonempty_list(COMMA, pattern) { PATTERN_ITEMS $1 }

slice_pattern:
  | LBRACK option(slice_pattern_items) RBRACK { SLICE_PATTERN $2 }

slice_pattern_items:
  | separated_nonempty_list(COMMA, pattern) { }

(*Patterns*)
typ:
  | type_no_bounds { RAW_POINTER_TYPE $1 }

type_no_bounds:
  | type_path { TYPE_PATH $1 }

type_path:
  | option(PATHSEP) separated_nonempty_list(PATHSEP,type_path_segment) 
{ TYP_PATH $2 }

type_path_segment:
  | path_ident_segment { TYPE_PATH_SEGMENT ($1, None) }

path_ident_segment:
  | id = ident {PATH_IDENT_SEGMENT_IDENT id  }
  | SUPER { PATH_IDENT_SEGMENT_SUPER }
  | SELFVALUE { PATH_IDENT_SEGMENT_SELF }
  | SELFTYPE { PATH_IDENT_SEGMENT_self }
  | CRATE { PATH_IDENT_SEGMENT_CRATE }
  | DOLLAR_CRATE {PATH_IDENT_SEGMENT_SCRATE }

(*Helpers*)
is_const:
  | CONST { true }
  | { false }

is_async:
  | ASYNC { true }
  | { false }

is_unsafe:
  | UNSAFE { true }
  | SAFE { false }
  | { false }

is_mut:
  | MUT { true }
  | { false }

is_ref:
  | REF { true }
  | { false }

(*Helpers*)

(*use declaration*)
use_declaration:
  | USE use_tree SEMI { Cabs.USE_DECL $2}

use_tree:
  | skib = simple_path_special STAR { Cabs.USE_TREE skib }
  | skib = simple_path_special;
  LBRACE;
  trees = use_trees
  RBRACE
    { Cabs.USE_TREE_LIST (skib, trees) }
  | what = simple_path as_id = as_id_or_underscore { Cabs.USE_TREE_ID (what, as_id)}


use_trees:
  | use_tree_list option(COMMA) { Stdlib.List.rev $1 }

use_tree_list:
  | use_tree { [$1] }
  | use_tree_list COMMA use_tree { $3 :: $1 }

as_id_or_underscore:
  | AS id = ident { Some (Cabs.ID_OPT (fst id)) }
  | AS UNDERSCORE { Some (Cabs.UNDERSCORE_OPT) }
  | { None }

(*use declaration*)

(*Paths*)
simple_path_special:
  | segments = nonempty_list(terminated(simple_path_segment, PATHSEP)) { Some(Cabs.SIMPLE_PATH segments) }
  | PATHSEP; segments = nonempty_list(terminated(simple_path_segment, PATHSEP)) { Some (Cabs.SIMPLE_PATH segments) }
  | { None }

simple_path:
  | option(PATHSEP) segments = separated_nonempty_list(PATHSEP, simple_path_segment) { Cabs.SIMPLE_PATH segments }

simple_path_segment:
  | id = ident   { Cabs.SIMPLE_PATH_SEGMENT_IDENT (fst id) }
  | SUPER { SIMPLE_PATH_SEGMENT_SUPER  }
  | SELFVALUE { SIMPLE_PATH_SEGMENT_SELF  }
  | CRATE { SIMPLE_PATH_SEGMENT_CRATE  }
  | DOLLAR_CRATE { SIMPLE_PATH_SEGMENT_SCRATE }

path_in_expression:
  | option(PATHSEP) segments = separated_nonempty_list(PATHSEP, path_expr_segment) 
  { Cabs.PATH_IN_EXPRESSION segments }

path_expr_segment:
  | path_ident_segment option(path_genarg) { PATH_EXPR_SEGMENT $1 $2}

path_genarg:
  | PATHSEP generic_args { $2 }

generic_args:
  | LT GT { EMPTY_GENERIC_ARGS }
  | LT generic_arg_list option(COMMA) GT { Stdlib.List.rev $2 }

generic_arg_list:
  | generic_arg { [$1] }
  | generic_args COMMA generic_arg { $3 :: $1 }

generic_arg:
  | lifetime              { GENERIC_ARG_LIFETIME $1 }
  | typ                   {  GENERIC_ARG_TYPE $1 }
  | generic_args_const     { GENERIC_ARG_CONST $1 }
  | generic_args_binding   { GENERIC_ARGS_BINDING $1 }
  | generic_args_bounds    { GENERIC_ARGS_BOUNDS $1 }

generic_args_const:
  | block_expression             { GENERIC_ARGS_CONST_BLOCK $1 }
  | literal_expression           { GENERIC_ARGS_CONST_LIT $1 }
  | MINUS literal_expression     { NEG_GENERIC_ARGS_CONST_LIT $1 }
  | simple_path_segment          { GENERIC_ARGS_CONST_SIMPLE_PATH_SEG $1 }

generic_args_bounds:
  | ident option(generic_args) EQ type_param_bounds { GENERIC_ARGS_BOUNDS_ ($1, $2, $4) }

generic_args_binding:
  | ident option(generic_args) EQ typ { GENERIC_ARGS_BINDING_ ($1, $2, $4) }

(*Paths*)

(*Extern crate*)
extern_crate:
  | EXTERN CRATE ref = crate_ref clause = as_clause SEMI { Cabs.EXT_CRATE_CLAUSE (ref, clause) }

crate_ref:
  | id = ident { Cabs.ID_CRATE_REF (fst id) }
  |  SELFVALUE {Cabs.SELF_CRATE_REF}

as_clause:
  | AS id = ident { Some (Cabs.ID_AS_CLAUSE (fst id)) }
  | AS UNDERSCORE { Some (Cabs.UNDERSCORE_AS_CLAUSE) }
  | { None }
(*Extern crate*)

(*Modules*)
unsafe_module:
  | sm = safe_module { sm }
  | UNSAFE MOD name = ident SEMI
    { MOD_BLOCK (true, (fst name)) }
  | UNSAFE MOD name = ident LBRACE attrs = inner_attrs content = items RBRACE
    { MOD_DEC (true, (fst name), Stdlib.List.rev (attrs), content) }

safe_module:
  | MOD name = ident SEMI
    { MOD_BLOCK (false, (fst name)) }
  | MOD name = ident LBRACE attrs = inner_attrs content = items RBRACE
    { MOD_DEC (false, (fst name), Stdlib.List.rev (attrs), content) }

(*Modules*)

ident:
  | id = IDENT { (IDENT (fst id), snd id) }
  | raw = RAW_IDENT { (RAW_IDENT (fst raw), snd raw) }

(* ATTRIBUTE *)
outer_attrs:
  | /* empty */ { [] }
  | POUND a = outer_attr rest = outer_attrs { a :: rest }

outer_attr:
  | LBRACK a = attr RBRACK { OUTER_ATTRIBUTE a }

inner_attrs:
  | /* empty */ { [] }
  | rest = inner_attrs POUND a = inner_attr { a :: rest }

inner_attr:
  | NOT LBRACK a = attr RBRACK { INNER_ATTRIBUTE a }

attr:
  | path = simple_path input = maybe_attr_input { SAFE_ATTR (path, input) }
  | UNSAFE path = simple_path input = maybe_attr_input { UNSAFE_ATTR (path, input) }

maybe_attr_input:
  | a = attr_input { Some a }
  | /* empty */ { None }

attr_input:
  | EQ e = expression { ATTR_INPUT_EXP e }
(* ATTRIBUTE *)

expression:
  | attrs = outer_attrs expr = expression_no_block { EXPRESSION_WITHOUT_BLOCK (attrs, expr) }

expression_no_block:
  | UNDERSCORE { UNDERSCORE_EXPRESSION }
