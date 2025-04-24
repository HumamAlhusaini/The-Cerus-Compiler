open Ast
open Parser

let string_of_token = function
  | INT i        -> Printf.sprintf "INT(%d)" i
  | IDENT s      -> Printf.sprintf "IDENT(%s)" s
  | STRING s     -> Printf.sprintf "STRING(%S)" s
  | FLOAT f      -> Printf.sprintf "FLOAT(%f)" f

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

let rec string_of_expr = function
  | Let (_, name, value) ->
      "let " ^ name ^ " = " ^ string_of_expr value
  | Func (_, name, args, body) ->
  let args_str = String.concat ", " (Option.value args ~default:[]) in
    "fn " ^ name ^ "(" ^ args_str ^ ") {\n  " ^
      String.concat "\n  " (List.map string_of_expr body) ^ "\n}"
  | Print (_, s) ->
      Printf.sprintf "Print(%s)" s

let string_of_program (prog : program) : string =
  String.concat ";\n" (List.map string_of_expr prog) ^ ";\n"
