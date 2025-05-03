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

%token<Cabs.identifier * Cabs.loc> IDENT
%token<list Cabs.char_code * Cabs.loc> STRING_LIT RAW_STRING_LIT BYTE_STRING RAW_BYTE_STRING RAW_C_STRING C_STRING
%token<Cabs.char_code * Cabs.loc> CHAR_LIT BYTE

%token EOF

%type<Cabs.top_level * Cabs.loc> id
%type< list (Cabs.top_level * Cabs.loc) > id_list

%start<list (Cabs.top_level * Cabs.loc) > program
%%

id:
| id = IDENT      { 
  (Cabs.IDENTIFIER (fst id), snd id) }

program:
| ids = id_list EOF { ids }

id_list:
| i = id             { [i] }
| i = id; rest = id_list { i :: rest }
