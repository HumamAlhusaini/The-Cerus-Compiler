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

let explode s = List.init (String.length s) (String.get s)

let get_column pos =
  pos.Lexing.pos_cnum - pos.Lexing.pos_bol
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
  | newline            { next_line lexbuf; read_token lexbuf}
  | int           { CONSTANT0 (CONST_INT (Lexing.lexeme lexbuf), lexbuf.lex_curr_p) }
  | "+"                 { ADD0 (lexbuf.lex_curr_p) }
  | "-"                 { SUB0 (lexbuf.lex_curr_p) }
  | "*"                 { MUL0 (lexbuf.lex_curr_p) }
  | "/"                 { DIV0 (lexbuf.lex_curr_p) }
  | "="                  { EQ1 (lexbuf.lex_curr_p)}
  | eof                { EOF () }
  | _                  { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
