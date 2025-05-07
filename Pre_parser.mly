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
%type <Cabs.simple_path_segment list> simple_path_segments
%type <Cabs.simple_path_segment> simple_path_segment
%type <Cabs.attr_input> attr_input
%type <Cabs.attr_input option> maybe_attr_input
%type <Cabs.expression> expression
%type <Cabs.type_expr_without_block> expression_no_block

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

(*use declaration*)
use_declaration:
  | USE use_tree_short SEMI { Cabs.USE_DECL $2}

use_tree_long:
  | use_tree_rootless { $1 }
  | use_tree_longer { $1 }

use_tree_rootless:
  | PATHSEP STAR { Cabs.USE_TREE None }
  | PATHSEP LBRACE trees = use_trees RBRACE { Cabs.USE_TREE_LIST (None, trees) }

use_tree_short:
  | STAR { Cabs.USE_TREE None }
  | LBRACE trees = use_trees RBRACE { Cabs.USE_TREE_LIST (None, trees) }
  | use_tree_long { $1 }

use_tree_longer:
  | skib = simple_path PATHSEP LBRACE trees = use_trees RBRACE { Cabs.USE_TREE_LIST (None, trees) }
  | skib = simple_path PATHSEP STAR { Cabs.USE_TREE (Some skib) }
  | what = simple_path as_id = as_identifier { }

use_trees:
  | use_tree_short COMMA use_trees { $1 :: $2 }
  | { [] }

 as_identifier:
  | AS id = ident { Some (Cabs.ID_OPT (fst id)) }
  | AS UNDERSCORE { Some (Cabs.UNDERSCORE_OPT)}
  | { None }
(*use declaration*)


simple_path:
  | segments = simple_path_segments         { Cabs.SIMPLE_PATH segments }

simple_path_segments:
  | PATHSEP seg = simple_path_segment                     { [seg] }
  | PATHSEP seg = simple_path_segment PATHSEP rest = simple_path_segments { seg :: rest }
  | seg = simple_path_segment                     { [seg] }                      
  | seg = simple_path_segment PATHSEP rest = simple_path_segments { seg :: rest }

simple_path_segment:
  | id = ident   { Cabs.SIMPLE_PATH_SEGMENT_IDENT (fst id) }
  | SUPER { SIMPLE_PATH_SEGMENT_SUPER  }
  | SELFVALUE { SIMPLE_PATH_SEGMENT_SELF  }
  | CRATE { SIMPLE_PATH_SEGMENT_CRATE  }
  | DOLLAR_CRATE { SIMPLE_PATH_SEGMENT_SCRATE }
extern_crate:
  | EXTERN CRATE ref = crate_ref clause = as_clause SEMI { Cabs.EXT_CRATE_CLAUSE (ref, clause) }

crate_ref:
  | id = ident { Cabs.ID_CRATE_REF (fst id) }
  |  SELFVALUE {Cabs.SELF_CRATE_REF}

as_clause:
  | AS id = ident { Some (Cabs.ID_AS_CLAUSE (fst id)) }
  | AS UNDERSCORE { Some (Cabs.UNDERSCORE_AS_CLAUSE) }
  | { None }

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

ident:
  | id = IDENT { (IDENT (fst id), snd id) }
  | raw = RAW_IDENT { (RAW_IDENT (fst raw), snd raw) }

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

expression:
  | attrs = outer_attrs expr = expression_no_block { EXPRESSION_WITHOUT_BLOCK (attrs, expr) }

expression_no_block:
  | UNDERSCORE { UNDERSCORE_EXPRESSION }
