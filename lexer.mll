{
open Lexing
open Pre_parser
open Ast

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
  | newline            { next_line lexbuf; }
  | int as i           { CONSTANT ((CONST_INT i) lexbuf.lex_curr_p) }
  | "+"                 { ADD (currentLoc lexbuf) }
  | "-"                 { SUB (currentLoc lexbuf) }
  | "*"                 { MULT (currentLoc lexbuf) }
  | "/"                 { DIV (currentLoc lexbuf) }
  | "="                  { EQ (currentLoc lexbuf)}
  | eof                { EOF }
  | _                  { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }

