{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+  (* regex for integers *)
let id = (alpha) (alpha|digit|'_')* (* regex for identifier *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read_token = parse
  | whitespace         { read_token lexbuf }
  | newline            { next_line lexbuf; read_token lexbuf }
  | "//"             { read_token lexbuf }  (* single-line comment *)
  | "/*"               { read_multi_line_comment lexbuf }
  | "fn"               { FN () }
  | "let"              { LET () }
  | "print!"          { PRINT () }
  | "mut"              { MUT () }
  | "return"           { RETURN () }
  | "struct"           { STRUCT () }
  | "enum"             { ENUM () }
  | "if"               { IF () }
  | "else"             { ELSE () }
  | "while"            { WHILE () }
  | "for"              { FOR () }
  | "loop"             { LOOP () }
  | "match"            { MATCH () }
  | "impl"             { IMPL () }
  | "trait"            { TRAIT () }
  | "const"            { CONST () }
  | "static"           { STATIC () }
  | "use"              { USE () }
  | "pub"              { PUB () }
  | "mod"              { MOD () }
  | "type"             { TYPE () }
  | "as"               { AS () }
  | "extern"           { EXTERN () }
  | "crate"            { CRATE () }
  | "move"             { MOVE () }
  | "ref"              { REF () }
  | "self"             { SELF () }
  | "super"            { SUPER () }
  | "true"             { TRUE () }
  | "false"            { FALSE () } 
  | int as i           { INT (int_of_string i) }
  | '"'                { read_string (Buffer.create 17) lexbuf }
  | '\'' ([^'\\'] as c) '\'' { CHARLIT(c) }        (* Simple char like 'a' *)
  | "+"                { PLUS () }
  | "-"                { MINUS () }
  | "*"                { STAR () }
  | "/"                { SLASH () }
  | "%"                { PERCENT () }
  | "="                { EQ () }
  | "=="               { EQEQ () }
  | "!="               { NE () }
  | "<"                { LT () }
  | "<="               { LE () }
  | ">"                { GT () }
  | ">="               { GE () }
  | "||"               { OROR () }
  | "!"                { NOT () }
  | "."                { DOT () }
  | ".."               { DOTDOT () }
  | "..."              { DOTDOTDOT () }
  | ","                { COMMA () }
  | ";"                { SEMI () }
  | ":"                { COLON () }
  | "->"               { ARROW () }
  | "=>"               { FAT_ARROW () }
  | "&mut"             { AMPMUT () }
  | "&"                { AMP () }
  | "|"                { PIPE () }
  | "("                { LPAREN () }
  | ")"                { RPAREN () }
  | "{"                { LBRACE () }
  | "}"                { RBRACE () }
  | "["                { LBRACK () }
  | "]"                { RBRACK () }
  | "_"                { UNDERSCORE () }
  | "i32"               { I32 () }
  | "f32"               { F32 () }
  | "f64"               { F64 () }
  | "char"              { CHAR () }
  | "bool"              { BOOL () }
  | id as ident        { IDENT ident }

  | eof                { EOF () }

  | _                  { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
  | "*/" { read_token lexbuf }
  | newline { next_line lexbuf; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

