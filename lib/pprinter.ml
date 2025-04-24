open Ast
open Parser

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

let string_of_typ = function
  | TLit TInt32 -> "i32"
  | TLit TUInt32 -> "u32"
  | TLit TFloat32 -> "f32"
  | TLit TFloat64 -> "f64"
  | TLit TChar -> "char"
  | TLit Bool -> "bool"
  | TCustom name -> name

let string_of_param (name, typ) =
  name ^ ": " ^ string_of_typ typ

let string_of_literal = function
  | LInt i -> string_of_int i
  | LFloat f -> string_of_float f
  | LChar c -> "'" ^ String.make 1 c ^ "'"
  | LTrue -> "true"
  | LFalse -> "false"

let string_of_statement = function
  | Declaration (_, name, typ, lit) ->  
      "let " ^ name ^ ": " ^ string_of_typ typ ^ " = " ^ string_of_literal lit ^ ";"
  | Print (_, s) ->
      Printf.sprintf "Print(%s);" s

let (*rec*) string_of_expr = function
  | Func (_, name, args, body) ->
    let args_str =
      match args with
        | None -> ""
        | Some lst -> String.concat ", " (List.map string_of_param lst)
    in
      "fn " ^ name ^ "(" ^ args_str ^ ") {\n  " ^
      String.concat "\n  " (List.map string_of_statement body) ^ "\n}"

let string_of_program (prog : program) : string =
  String.concat ";\n" (List.map string_of_expr prog) ^ ";\n"
