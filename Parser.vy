
%{

Require Import Ascii.
Require Import String.
Require Import List.
Require Import Main.Ast.

%}


%token <string> IDENT STRING 
%token <nat> INT             
%token <float> FLOAT         
%token <ascii> CHARLIT        
%token TRUE FALSE            

%token FN LET MUT RETURN STRUCT ENUM IF ELSE WHILE FOR LOOP MATCH IMPL TRAIT CONST STATIC

%token USE PUB MOD TYPE AS EXTERN CRATE MOVE REF SELF SUPER BOOL I32 U32 F32 F64 CHAR STR

%token PLUS MINUS STAR SLASH PERCENT EQ EQEQ NE LT LE GT GE ANDAND OROR NOT

%token DOT DOTDOT DOTDOTDOT COMMA SEMI COLON ARROW FAT_ARROW AMP AMPMUT
                              

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PIPE UNDERSCORE PRINT
                              

%token EOF      

%type <string * Ast.typ> var_and_typ
%type <typ> typ
%type <Ast.stmt> stmt
%type <simpl_typ> simpl_typ
%type <list Ast.typ> separated_list_typ
%type <list (string * typ)> separated_list_params
%type <string * list typ> enum
%type <list (string * list typ) > separated_list_enum
%type <bool> opt_mut
%type <Ast.typ_lit> lit_type
%type <list Ast.item> item_list
%type <Ast.item> item
%type <option Ast.expr> has_expr
%type <Ast.expr> expr
%type <list Ast.block_element> block_element_list
%type <Ast.block_element> block_element

%start program
%type <Ast.program> program

%%

program:
  i_ls = item_list; EOF { i_ls }

item_list:
  /* empty */ { [] }
| e = item; rest = item_list { e :: rest }

expr:
    | i = INT     { IntLit i }
    | i = FLOAT   { FloatLit i }
    | c = CHARLIT { CharLit c }
    | s = STRING  { StringLit s }
    | d = IDENT   { Ident d }
    | TRUE        { LTrue }
    | FALSE       { LFalse }

has_expr:
    | EQ; e = expr; SEMI { Some e }
    | SEMI    { None }
;

opt_mut:
  | MUT { true }
  |     { false }
;

var_and_typ:
  | id = IDENT; COLON; t = typ { (id, t) }

separated_list_typ:
| s = typ; COMMA; ls = separated_list_typ; { s :: ls }
| s = typ; { [s] }

separated_list_params:
| s = var_and_typ; COMMA; ls = separated_list_params; { s :: ls }
| s = var_and_typ; { [s] }

stmt:
    | LET; mut = opt_mut; name = IDENT; COLON; t = typ; dec = has_expr; {
        Let mut  name t dec
    }
 
block_element:
    | s = stmt { Stmt_block s }
    | i = item { Item_block i }

separated_list_enum: 
| e = enum; COMMA; ls = separated_list_enum { e :: ls }
| e = enum; { [e] }

enum:
  | id = IDENT; LPAREN; typs = separated_list_typ; RPAREN { (id, typs) }
  | id = IDENT                                        { (id, []) }

block_element_list:
| b = block_element; ls = block_element_list { b :: ls }
| b = block_element; { [b] }


%public
item:
  | FN; name = IDENT; LPAREN; parameters = separated_list_params; RPAREN; ARROW; t = typ;
    LBRACE; body = block_element_list; RBRACE {
      Func name parameters t body
  }
  | STRUCT; name = IDENT; LBRACE; p = separated_list_params; RBRACE { 
      Struct name p 
    }
  | ENUM; name = IDENT; LBRACE; variants = separated_list_enum; RBRACE {
      Enum name variants
  } 
  | CONST; name = IDENT; COLON; t = typ; EQ; value = expr; SEMI {
        Const name t value
}
  | STATIC; mut = opt_mut; name = IDENT; COLON; t = typ; value = has_expr {
      Static mut name t value
  }
    | IMPL; name = IDENT; LBRACE; body = item_list; RBRACE; {
        Impl name body
  }


lit_type:
    | I32        { TInt32 }
    | U32        { TUInt32 }
    | F32        { TFloat32 }
    | F64        { TFloat64 }
    | CHAR       { TChar }
    | BOOL       { TBool }

simpl_typ:
  | base = lit_type                { TLit base }
  | AMP t = simpl_typ              { TRef false t }
  | AMPMUT t = simpl_typ           { TRef true t }
  | LPAREN; s = separated_list_typ; RPAREN;       { TParen s }

typ:
  | s = simpl_typ { Typ s  }
  | s1 = simpl_typ; ARROW; s2 = simpl_typ { TArrow s1 s2 }


