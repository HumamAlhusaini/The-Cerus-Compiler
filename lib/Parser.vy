
%{

Require Import List.
Require Import Ast.

Require Extraction.
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

%type <string * Ast.typ> var_and_typ
%type <typ> typ
%type <Ast.stmt> stmt
%type <simpl_typ> simpl_typ
%type <list typ> separated_list_typ
%type <(string * typ) list> separated_list_params
%type <string * typ list> enum
%type <(string * typ list) list > separated_list_enum
%type <bool> opt_mut
%type <Ast.typ_lit> lit_type
%type <item list> item_list
%type <Ast.item> item
%type <option expr> has_expr
%type <Ast.expr> expr
%type <block_element list> block_element_list
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
| s = typ; { s }

separated_list_params:
| s = var_and_typ; COMMA; ls = separated_list_params; { s :: ls }
| s = var_and_typ; { s }

stmt:
    | LET; mut = opt_mut; name = IDENT; COLON; t = typ; dec = has_expr; {
         let n = Var_name.of_string name in
        Let (mut,  n, t, dec)
    }
 
block_element:
    | s = stmt { Stmt_block s }
    | i = item { Item_block i }

separated_list_enum: 
| e = enum; COMMA; ls = separated_list_enum { e :: ls }
| e = enum; { e }

enum:
  | id = IDENT; LPAREN; typs = separated_list_typ; RPAREN { (id, typs) }
  | id = IDENT                                        { (id, []) }

block_element_list:
| b = block_element; ls = block_element_list { b :: ls }
| b = block_element; { b }


%public
item:
  | FN; name = IDENT; LPAREN; parameters = separated_list_params; RPAREN; ARROW; t = typ;
    LBRACE; body = block_element_list; RBRACE {
      let n = Func_name.of_string name in
      let params_opt = match parameters with
        | [] -> None
        | lst -> Some lst
      in
      Func (n, params_opt, t, body)
  }
  | STRUCT; name = IDENT; LBRACE; p = separated_list_params; RBRACE { 
      let n = Struct_name.of_string name in
      Struct (n, p) 
    }
  | ENUM; name = IDENT; LBRACE; variants = separated_list_enum; RBRACE {
      let n = Enum_name.of_string name in
      Enum (n, variants)
  } 
  | CONST; name = IDENT; COLON; t = typ; EQ; value = expr; SEMI {
      let n = Var_name.of_string name in
        Const (n, t, value)
}
  | STATIC; mut = opt_mut; name = IDENT; COLON; t = typ; value = has_expr {
      let n = Var_name.of_string name in
      Static (mut, n, t, value)
  }
    | IMPL; name = IDENT; LBRACE; body = item_list; RBRACE; {
      if List.for_all is_valid_impl_item body then
        Impl (Impl_name.of_string name, body)
      else
        failwith "Invalid item found inside impl block"
  }


lit_type:
    | I32        { TInt32 }
    | U32        { TUInt32 }
    | F32        { TFloat32 }
    | F64        { TFloat64 }
    | CHAR       { TChar }
    | BOOL       { Bool }

simpl_typ:
  | base = lit_type                { TLit base }
  | AMP t = simpl_typ              { TRef (false, t) }
  | AMPMUT t = simpl_typ           { TRef (true, t) }
  | LPAREN; s = separated_list_typ; RPAREN;       { TParen s }

typ:
  | s = simpl_typ { Typ s  }
  | s1 = simpl_typ; ARROW; s2 = simpl_typ { TArrow (s1, s2)}


