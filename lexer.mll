{
open Lexing
open Parser
open Ast

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let currentLoc =
  let nextident = ref 0 in
  let getident () =
    nextident := !nextident + 1;
    !nextident
  in
  fun lb ->
    let p = Lexing.lexeme_start_p lb in
    Cabs.({ lineno   = p.Lexing.pos_lnum;
            filename = p.Lexing.pos_fname;
            byteno   = p.Lexing.pos_cnum;
            ident    = getident ();})

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
  | int as i           { CONSTANT (CONST_INT i) currentLoc lexbuf) }
  | "+"                 { ADD (currentLoc lexbuf) }
  | "-"                 { SUB (currentLoc lexbuf) }
  | "*"                 { MULT (currentLoc lexbuf) }
  | "/"                 { DIV (currentLoc lexbuf) }
  | "="                  { EQ (currentLoc lexbuf)}
  | eof                { EOF }
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

