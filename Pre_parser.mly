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

(*Functions*)
(*
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
  | is_mut SELFVALUE COLON typ {}

function_param:
  | outer_attrs DOTDOTDOT { }
  | outer_attrs function_param_pattern {}
  | outer_attrs typ { }

function_param_pattern:
  | pattern_no_top_alt COLON typ {}
  | pattern_no_top_alt COLON DOTDOTDOT {}

pattern_no_top_alt:
  | 

function_return_type:
  | LARROW typ { $2 }
  *)
(* Functions*)

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
  | use_tree_list option(COMMA) { $1 }

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
  | segments = separated_nonempty_list(PATHSEP, simple_path_segment) { Cabs.SIMPLE_PATH segments }
  | PATHSEP; segments = separated_nonempty_list(PATHSEP, simple_path_segment) { Cabs.SIMPLE_PATH segments }

simple_path_segment:
  | id = ident   { Cabs.SIMPLE_PATH_SEGMENT_IDENT (fst id) }
  | SUPER { SIMPLE_PATH_SEGMENT_SUPER  }
  | SELFVALUE { SIMPLE_PATH_SEGMENT_SELF  }
  | CRATE { SIMPLE_PATH_SEGMENT_CRATE  }
  | DOLLAR_CRATE { SIMPLE_PATH_SEGMENT_SCRATE }
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
