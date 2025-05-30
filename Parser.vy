%{
From Coq Require Extraction.
Extraction Language OCaml.
Require Import Ascii.
Require Import String.
Require Import List.
Require Import Proj.Cabs.

%}

%token<Cabs.loc> AS BREAK CONST CONTINUE CRATE
%token<Cabs.loc> ELSE ENUM EXTERN FALSE FN
%token<Cabs.loc> FOR IF IMPL IN LET
%token<Cabs.loc> LOOP MATCH MOD MOVE MUT
%token<Cabs.loc> PUB REF RETURN HASH
%token<Cabs.loc> SELFVALUE SELFTYPE STATIC STRUCT SUPER
%token<Cabs.loc> TRAIT TRUE TYPE UNSAFE USE
%token<Cabs.loc> WHERE WHILE ASYNC AWAIT DYN EXCLAMATION
%token<Cabs.loc> MACRO_RULES UNION STATICLIFETIME SAFE RAW

%token<Cabs.loc> PLUS MINUS STAR SLASH PERCENT              /* + - * / % */
%token<Cabs.loc> CARET NOT AND OR ANDAND OROR               /* ^ ! & | && || */
%token<Cabs.loc> SHL SHR DOLLAR_CRATE                                   /* << >> */
%token<Cabs.loc> EQ PLUSEQ MINUSEQ STAREQ SLASHEQ            /* = += -= *= /= */
%token<Cabs.loc> PERCENTEQ CARETEQ ANDEQ OREQ SHLEQ SHREQ    /* %= ^= &= |= <<= >>= */
%token<Cabs.loc> EQEQ NE LT GT LE GE                         /* == != < > <= >= */
%token<Cabs.loc> AT UNDERSCORE DOT DOTDOT DOTDOTDOT DOTDOTEQ /* @ _ . .. ... ..= */
%token<Cabs.loc> COMMA SEMI COLON RESERVED_RAW_IDENTIFIER   /* , ; : */
%token<Cabs.loc> PATHSEP RARROW FATARROW LARROW              /* :: -> => <- */
%token<Cabs.loc> POUND DOLLAR QUESTION TILDE               /* # $ ? ~ */
%token<Cabs.loc> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN 

%token<Cabs.str * Cabs.loc> IDENT RAW_IDENT
%token<list Cabs.char_code * Cabs.loc> STRING_LIT RAW_STRING_LIT BYTE_STRING RAW_BYTE_STRING RAW_C_STRING C_STRING
%token<Cabs.char_code * Cabs.loc> CHAR_LIT BYTE
%token<Cabs.constant * Cabs.loc> CONSTANT

%token EOF

%type <list Cabs.item> items
%type <Cabs.item> item
%type <Cabs.visItem> vis_item
%type <Cabs.extern_crate> extern_crate
%type <Cabs.crate_ref> crate_ref
%type <option Cabs.as_clause> as_clause
%type <Cabs.module_> unsafe_module safe_module
%type <(Cabs.identifier * Cabs.loc)> ident
%type <list Cabs.outer_attribute> outer_attrs
%type <Cabs.outer_attribute> outer_attr
%type <list Cabs.inner_attribute> inner_attrs
%type <Cabs.inner_attribute> inner_attr
%type <Cabs.attr> attr
%type <Cabs.attr_input> attr_input
%type <option Cabs.attr_input> maybe_attr_input
%type <Cabs.expression> expression
%type <Cabs.type_expr_without_block> expression_no_block
%type <Cabs.simple_path> simple_path
%type <Cabs.simple_path_segment> simple_path_segment
%type <list Cabs.simple_path_segment> simple_path_segments
%type <Cabs.use_declaration> use_declaration
%type <Cabs.use_tree> use_tree
%type <list use_tree> use_trees

%start<list Cabs.item> program
%%

program:
  | e = items EOF { e }

items:
  |                                { [] }
  | i = item rest = items       { i :: rest }

item:
  | attrs = outer_attrs v = vis_item      { Cabs.VISITEM attrs v }

vis_item:
  | m = unsafe_module { Cabs.MODULE m }
  | e = extern_crate { Cabs.EXTERN_CRATE e }

(*use declaration*)
use_declaration:
  | USE use_tree SEMI { Cabs.USE_DECL $2}

use_tree:
  | STAR { Cabs.USE_TREE None }
  | skib = simple_path PATHSEP STAR { Cabs.USE_TREE (Some skib) }
  | PATHSEP STAR { Cabs.USE_TREE None }
  | LBRACE trees = use_trees RBRACE { Cabs.USE_TREE_LIST (None, trees) }
  | PATHSEP LBRACE trees = use_trees RBRACE { Cabs.USE_TREE_LIST (None, trees) }
  | skib = simple_path PATHSEP LBRACE trees = use_trees RBRACE { Cabs.USE_TREE_LIST (None, trees) }
  | what = simple_path as_id = as_identifier { Cabs.USE_TREE_ID (what, as_id) }

use_trees:
  | use_tree use_trees { $1 :: $2 }
  | { [] }

 as_identifier:
  | AS id = ident { Some (Cabs.ID_OPT (fst id)) }
  | AS UNDERSCORE { Some (Cabs.UNDERSCORE_OPT)}
  | { None }
(*use declaration*)


simple_path:
  | segments = simple_path_segments         { Cabs.SIMPLE_PATH segments }

simple_path_segments:
  | seg = simple_path_segment                     { [seg] }
  | seg = simple_path_segment rest = simple_path_segments { seg :: rest }

simple_path_segment:
  | id = ident   { Cabs.SIMPLE_PATH_SEGMENT_IDENT (fst id) }
  | SUPER { SIMPLE_PATH_SEGMENT_SUPER  }
  | SELFVALUE { SIMPLE_PATH_SEGMENT_SELF  }
  | CRATE { SIMPLE_PATH_SEGMENT_CRATE  }
  | DOLLAR_CRATE { SIMPLE_PATH_SEGMENT_SCRATE }

extern_crate:
  | EXTERN CRATE ref = crate_ref clause = as_clause SEMI { Cabs.EXT_CRATE_CLAUSE ref clause }

crate_ref:
  | id = ident { Cabs.ID_CRATE_REF (fst id) }
  |  SELFVALUE {Cabs.SELF_CRATE_REF}

as_clause:
  | AS id = ident { Some (Cabs.ID_AS_CLAUSE (fst id)) }
  |  AS UNDERSCORE { Some (Cabs.UNDERSCORE_AS_CLAUSE) }
  | { None }

safe_module:
  | MOD name = ident SEMI
      { Cabs.MOD_BLOCK false (fst name) }
  | MOD name = ident LBRACE attrs = inner_attrs content = items RBRACE
      { Cabs.MOD_DEC false (fst name) attrs content }

unsafe_module:
  | safe_module { $1 }
  | UNSAFE MOD name = ident SEMI
      { Cabs.MOD_BLOCK true (fst name) }
  | UNSAFE MOD name = ident LBRACE attrs = inner_attrs content = items RBRACE
      { Cabs.MOD_DEC true (fst name) attrs content }

ident:
  | id = IDENT        { (Cabs.IDENT (fst id), snd id) }
  | raw = RAW_IDENT   { (Cabs.RAW_IDENT (fst raw), snd raw) }

outer_attrs:
  | /* empty */                             { [] }
  | HASH outer_attr outer_attrs                 { $2 :: $3 }

outer_attr:
  | LBRACK a = attr RBRACK             { Cabs.OUTER_ATTRIBUTE a }

inner_attrs:
  | /* empty */                             { [] }
  | inner_attrs HASH inner_attr                  { $3 :: $1 }

inner_attr:
  | EXCLAMATION LBRACK a = attr RBRACK      { Cabs.INNER_ATTRIBUTE a }

attr:
  | path = simple_path input = maybe_attr_input { Cabs.SAFE_ATTR path input }
  | UNSAFE path = simple_path input = maybe_attr_input { Cabs.UNSAFE_ATTR path input }

maybe_attr_input:
  | something = attr_input { Some something }
  |                    { None }

attr_input:
  | EQ e = expression { Cabs.ATTR_INPUT_EXP e }

expression:
  | attrs = outer_attrs expr = expression_no_block { Cabs.EXPRESSION_WITHOUT_BLOCK attrs expr }

expression_no_block:
  | UNDERSCORE        { Cabs.UNDERSCORE_EXPRESSION }
