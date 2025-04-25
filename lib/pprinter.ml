open Ast
open Program

let string_of_token = function
  | INT i        -> Printf.sprintf "INT(%d)" i
  | IDENT s      -> Printf.sprintf "IDENT(%s)" s
  | STRING s     -> Printf.sprintf "STRING(%S)" s
  | FLOAT f      -> Printf.sprintf "FLOAT(%f)" f
  | CHARLIT c    -> Printf.sprintf "CHARLIT(%c)" c

  | TRUE         -> "TRUE"
  | FALSE        -> "FALSE"
  | PRINT        -> "PRINT"

  | FN           -> "FN"
  | LET          -> "LET"
  | MUT          -> "MUT"
  | RETURN       -> "RETURN"
  | STRUCT       -> "STRUCT"
  | ENUM         -> "ENUM"
  | IF           -> "IF"
  | ELSE         -> "ELSE"
  | WHILE        -> "WHILE"
  | FOR          -> "FOR"
  | LOOP         -> "LOOP"
  | MATCH        -> "MATCH"
  | IMPL         -> "IMPL"
  | TRAIT        -> "TRAIT"
  | CONST        -> "CONST"
  | STATIC       -> "STATIC"
  | USE          -> "USE"
  | PUB          -> "PUB"
  | MOD          -> "MOD"
  | TYPE         -> "TYPE"
  | AS           -> "AS"
  | EXTERN       -> "EXTERN"
  | CRATE        -> "CRATE"
  | MOVE         -> "MOVE"
  | REF          -> "REF"
  | SELF         -> "SELF"
  | SUPER        -> "SUPER"

  (* Types *)
  | BOOL         -> "BOOL"
  | I32          -> "I32"
  | U32          -> "U32"
  | F32          -> "F32"
  | F64          -> "F64"
  | CHAR         -> "CHAR"
  | STR          -> "STR"

  (* Operators & Symbols *)
  | PLUS         -> "PLUS"
  | MINUS        -> "MINUS"
  | STAR         -> "STAR"
  | SLASH        -> "SLASH"
  | PERCENT      -> "PERCENT"

  | EQ           -> "EQ"
  | EQEQ         -> "EQEQ"
  | NE           -> "NE"
  | LT           -> "LT"
  | LE           -> "LE"
  | GT           -> "GT"
  | GE           -> "GE"

  | ANDAND       -> "ANDAND"
  | OROR         -> "OROR"
  | NOT          -> "NOT"

  | DOT          -> "DOT"
  | DOTDOT       -> "DOTDOT"
  | DOTDOTDOT    -> "DOTDOTDOT"
  | COMMA        -> "COMMA"
  | SEMI         -> "SEMI"
  | COLON        -> "COLON"

  | ARROW        -> "ARROW"
  | FAT_ARROW    -> "FAT_ARROW"
  | AMP          -> "AMP"
  | AMPMUT       -> "AMPMUT"

  | LPAREN       -> "LPAREN"
  | RPAREN       -> "RPAREN"
  | LBRACE       -> "LBRACE"
  | RBRACE       -> "RBRACE"
  | LBRACK       -> "LBRACK"
  | RBRACK       -> "RBRACK"
  | PIPE         -> "PIPE"
  | UNDERSCORE   -> "UNDERSCORE"
  | EOF          -> "EOF"

let rec string_of_typ = function
  | TLit TInt32 -> "i32"
  | TLit TUInt32 -> "u32"
  | TLit TFloat32 -> "f32"
  | TLit TFloat64 -> "f64"
  | TLit TChar -> "char"
  | TLit Bool -> "bool"
  | TCustom name -> name
  | TRef (mut, t) ->
      "&" ^ (if mut then "mut " else "") ^ string_of_typ t

let string_of_param (name, typ) =
  name ^ ": " ^ string_of_typ typ

let rec string_of_expr = function
  | IntLit i -> string_of_int i
  | FloatLit f -> string_of_float f
  | CharLit c -> "'" ^ String.make 1 c ^ "'"
  | StringLit s -> s
  | Ident s -> s
  | LTrue -> "true"
  | LFalse -> "false"
  | Ref (mut, e) -> "&" ^ (if mut then "mut " else "") ^ string_of_expr e

let string_of_statement = function
  | Let (_, mut, name, typ, expr) ->
      "let " ^ (if mut then "mut " else "") ^ Var_name.to_string name ^ ": " ^
      string_of_typ typ ^
      (match expr with
       | Some e -> " = " ^ string_of_expr e
       | None -> "") ^ ";"
  | Print (_, s) -> 
      Printf.sprintf "Print(%s)" s

let rec string_of_block_element = function
  | Stmt_block s -> string_of_statement s
  | Item_block i -> string_of_item i

and string_of_item = function
  | Func (_, name, args, typ, body) ->
      let args_str =
        match args with
        | None -> ""
        | Some lst -> String.concat ", " (List.map string_of_param lst)
      in
      let body_str = String.concat "\n  " (List.map string_of_block_element body) in
      "fn " ^ Func_name.to_string name ^ "(" ^ args_str ^ ") ->" ^ string_of_typ typ ^ " {\n  " ^ body_str ^ "\n}"

  | Struct (_, name, fields) ->
      let fields_str = String.concat ", " (List.map (fun (f, t) -> f ^ ": " ^ string_of_typ t) fields) in
      "struct " ^ Struct_name.to_string name ^ " { " ^ fields_str ^ " }"

  | Enum (_, name, variants) ->
      let variants_str = String.concat ", " (List.map (fun (v, t) -> v ^ " (" ^ String.concat ", " (List.map string_of_typ t) ^ ")") variants) in
      "enum " ^ Enum_name.to_string name ^ " { " ^ variants_str ^ " }"

  | Const (_, name, t, expr) ->
      Printf.sprintf "const " ^ Var_name.to_string name ^ ": " ^ string_of_typ t ^ " = " ^ string_of_expr expr ^ ";"

  | Static (_, mut, name, t, expr) ->
      "static " ^ (if mut then "mut " else "") ^ Var_name.to_string name ^ ": " ^
      string_of_typ t ^
      (match expr with
       | Some e -> " = " ^ string_of_expr e
       | None -> "") ^ ";"
  | Impl (_, name, body) -> 
      let body_str = String.concat "\n  " (List.map string_of_item body) in
      "fn " ^ Impl_name.to_string name ^ "{\n  " ^ body_str ^ "\n}"

let string_of_program (prog : program) : string =
  String.concat ";\n" (List.map string_of_item prog) ^ ";\n"
