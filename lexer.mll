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
  | int as i           { Parser.CONSTANT (Ast.CONST_INT i, lexbuf.lex_curr_p) }
  | "+"                 { Parser.ADD (lexbuf.position) }
  | "-"                 { SUB (currentLoc lexbuf) }
  | "*"                 { MULT (currentLoc lexbuf) }
  | "/"                 { DIV (currentLoc lexbuf) }
  | "="                  { EQ (currentLoc lexbuf)}
  | eof                { EOF }
  | _                  { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }

  {

  open Parser.MenhirLibParser.Inter

let lexer (tokens : Pre_parser.token Queue.t) (lexbuf : Lexing.lexbuf) : unit =
  let rec loop () =
    let token = read_token lexbuf in
    Queue.push token tokens;
    match token with
    | Pre_parser.EOF -> ()
    | _ -> loop ()
  in
  loop ()

(* Top-level function to read and convert tokens from a file *)
let tokens_stream (filename : string) : Parser.token list =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let token_queue = Queue.create () in
  lexer token_queue lexbuf;
  close_in chan;
  (* Convert queued tokens *)
  let rec compute_buffer () =
    let loop t = Buf_cons (t, Lazy.from_fun compute_buffer) in
    match Queue.pop tokens with 
    | Pre_parser.ADD loc -> loop (Parser.ADD loc)
    in
    Lazy.from_fun compute_buffer
  }
