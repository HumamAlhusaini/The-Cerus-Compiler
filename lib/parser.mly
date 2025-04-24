%{
open Ast 
%}

%token <string> IDENT STRING
%token <int> INT
%token <float> FLOAT
%token <char> CHARLIT
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
  item_list; EOF { $1 }

item_list:
  /* empty */ { [] }
| e = item; rest = item_list { e :: rest }
;

lit_type:
    | I32        { TInt32 }
    | U32        { TUInt32 }
    | F32        { TFloat32 }
    | F64        { TFloat64 }
    | CHAR       { TChar }
    | BOOL       { Bool }

typ:
    | lit = lit_type { TLit lit }
    | id = IDENT    { TCustom id }

var_and_typ:
  | id = IDENT; COLON; t = typ { (id, t) }

params:
  separated_list(COMMA, var_and_typ) {
    match $1 with
    | [] -> None
    | lst -> Some lst
  }

literals:
    | i = INT     { LInt i }
    | i = FLOAT   { LFloat i }
    | c = CHARLIT { LChar c }
    | TRUE        { LTrue }
    | FALSE       { LFalse }

stmt:
    | LET; vt = var_and_typ; EQ; dec = literals; SEMI {
    let name, t = vt in
    Declaration ($startpos, name, t, dec)
  }
    | PRINT; LPAREN; s = STRING; RPAREN; SEMI; { Print ($startpos, s) }

item:
 | FN; name = IDENT; LPAREN; parameters = params; RPAREN; 
 LBRACE; body = list(stmt); RBRACE { Func ($startpos, name, parameters, body)}
