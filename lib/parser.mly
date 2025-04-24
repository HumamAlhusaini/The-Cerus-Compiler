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
  separated_list(COMMA, var_and_typ) { $1 }

expr:
    | i = INT     { IntLit i }
    | i = FLOAT   { FloatLit i }
    | c = CHARLIT { CharLit c }
    | s = STRING { StringLit s }
    | d = IDENT { Ident d }
    | TRUE        { LTrue }
    | FALSE       { LFalse }

block_element:
    | s = stmt { Stmt_block s }
    | i = item { Item_block i }

stmt:
    | LET; vt = var_and_typ; EQ; dec = expr; SEMI {
    let name, t = vt in
    Declaration ($startpos, name, t, dec)
  }
    | PRINT; LPAREN; s = STRING; RPAREN; SEMI; { Print ($startpos, s) }

enum:
  | id = IDENT; LPAREN; typs = separated_list(COMMA, typ); RPAREN { (id, typs) }
  | id = IDENT                                        { (id, []) }

item:
  | FN; name = IDENT; LPAREN; parameters = params; RPAREN;
    LBRACE; body = list(block_element); RBRACE {
      let params_opt = match parameters with
        | [] -> None
        | lst -> Some lst
      in
      Func ($startpos, name, params_opt, body)
  }
    | STRUCT; name = IDENT; LBRACE; p = params; RBRACE { Struct ($startpos, name, p)}
    | ENUM; name = IDENT; LBRACE; variants = separated_list(COMMA, enum); RBRACE {
      Enum ($startpos, name, variants)
      }
    | CONST; name = IDENT; COLON; t = typ; EQ; value = expr; SEMI {
      Const ($startpos, name, t, value)
      }
    | STATIC; name = IDENT; COLON; t = typ; EQ; value = expr; SEMI {
      Static ($startpos, name, t, value)
      }

