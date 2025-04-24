%{
open Ast 
%}

%token <string> IDENT STRING
%token <int> INT
%token <float> FLOAT
%token TRUE FALSE 
%token FN LET MUT RETURN STRUCT ENUM IF ELSE WHILE FOR LOOP MATCH IMPL TRAIT CONST STATIC
%token USE PUB MOD TYPE AS EXTERN CRATE MOVE REF SELF SUPER BOOL I32 U32 F32 F64 CHAR STR
%token PLUS MINUS STAR SLASH PERCENT EQ EQEQ NE LT LE GT GE ANDAND OROR NOT
%token DOT DOTDOT DOTDOTDOT COMMA SEMI COLON ARROW FAT_ARROW AMP AMPMUT
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PIPE UNDERSCORE PRINT
%token EOF

%start program
%type <program> program

%%

program:
  expr_list; EOF { $1 }

expr_list:
  /* empty */ { [] }
| e = expr; rest = expr_list { e :: rest }
;

param:
  separated_list(COMMA, IDENT) {
    match $1 with
    | [] -> None
    | lst -> Some lst
  }

expr:
 | FN; name = IDENT; LPAREN; parameters = param; RPAREN; 
 LBRACE; expressions = list(expr); RBRACE { Func ($startpos, name, parameters, expressions)}

 | PRINT; LPAREN; s = STRING; RPAREN; { Print ($startpos, s) }
