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
  | int           { CONSTANT (CONST_INT (Lexing.lexeme lexbuf), currentLoc lexbuf) }
  | "+"                 { PLUS (currentLoc lexbuf) }
  | "-"                 { MINUS (currentLoc lexbuf) }
  | "*"                 { STAR (currentLoc lexbuf) }
  | "/"                 { SLASH (currentLoc lexbuf) }
  | "="                  { EQ (currentLoc lexbuf)}
  | eof                { EOF () }
  | _                  { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
