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
%token<Cabs.loc> PUB REF RETURN
%token<Cabs.loc> SELFVALUE SELFTYPE STATIC STRUCT SUPER
%token<Cabs.loc> TRAIT TRUE TYPE UNSAFE USE
%token<Cabs.loc> WHERE WHILE ASYNC AWAIT DYN
%token<Cabs.loc> MACRO_RULES UNION STATICLIFETIME SAFE RAW

%token<Cabs.loc> PLUS MINUS STAR SLASH PERCENT              /* + - * / % */
%token<Cabs.loc> CARET NOT AND OR ANDAND OROR               /* ^ ! & | && || */
%token<Cabs.loc> SHL SHR                                     /* << >> */
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

%token EOF

%type <list Cabs.item> items
%type <Cabs.item> item
%type <Cabs.visItem> vis_item
%type <Cabs.module> module_
%type <(Cabs.identifier * Cabs.loc)> ident
%type <bool> unsafe_kw_opt
%type <list Cabs.outer_attribute> outer_attrs
%type <Cabs.outer_attribute> outer_attr
%type <list Cabs.inner_attribute> inner_attrs
%type <Cabs.inner_attribute> inner_attr
%type <Cabs.attr> attr
%type <Cabs.simple_path> simple_path
%type <list Cabs.simple_path_segment> simple_path_segments
%type <Cabs.simple_path_segment> simple_path_segment
%type <Cabs.attr_input> attr_input
%type <option Cabs.attr_input> maybe_attr_input
%type <Cabs.expression> expression
%type <Cabs.type_expr_without_block> expression_no_block

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
  | m = module_                           { Cabs.MODULE m }

module_:
  | is_unsafe = unsafe_kw_opt _mod_kw = MOD name = ident _semi = SEMI
      { Cabs.MOD_BLOCK is_unsafe (fst name) }
  | is_unsafe = unsafe_kw_opt _mod_kw = MOD name = ident _lbrace = LBRACE attrs = inner_attrs content = items _rbrace = RBRACE
      { Cabs.MOD_DEC is_unsafe (fst name) attrs content }

ident:
  | id = IDENT        { (Cabs.RAW_IDENT (fst id), snd id) }
  | raw = RAW_IDENT   { (Cabs.IDENT (fst raw), snd raw) }

unsafe_kw_opt:
  | UNSAFE            { true }
  |                   { false }

outer_attrs:
  | single = outer_attr                     { [single] }
  | first = outer_attr rest = outer_attrs   { first :: rest }

outer_attr:
  | a = attr             { Cabs.OUTER_ATTRIBUTE a }

inner_attrs:
  | single = inner_attr                     { [single] }
  | first = inner_attr rest = inner_attrs   { first :: rest }

inner_attr:
  | a = attr             { Cabs.INNER_ATTRIBUTE a }

attr:
  | path = simple_path input = maybe_attr_input { Cabs.SAFE_ATTR path input }
  | UNSAFE path = simple_path input = maybe_attr_input { Cabs.UNSAFE_ATTR path input }

maybe_attr_input:
  | something = attr_input { Some something }
  |                    { None }

simple_path:
  | segments = simple_path_segments         { Cabs.SIMPLE_PATH segments }

simple_path_segments:
  | seg = simple_path_segment                     { [seg] }
  | seg = simple_path_segment rest = simple_path_segments { seg :: rest }

simple_path_segment:
  | id = ident   { Cabs.SIMPLE_PATH_SEGMENT_IDENT (fst id) }

attr_input:
  | EQ e = expression { Cabs.ATTR_INPUT_EXP e }

expression:
  | attrs = outer_attrs expr = expression_no_block { Cabs.EXPRESSION_WITHOUT_BLOCK attrs expr }

expression_no_block:
  | UNDERSCORE        { Cabs.UNDERSCORE_EXPRESSION }
