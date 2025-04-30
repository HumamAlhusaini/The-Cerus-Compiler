open Lexer
open Pprinter
open Parser
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

let print_tokens_from_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
    let rec loop () =
      let token = read_token lexbuf in 
      match token with
    | EOF -> print_endline "End of file."
    | t ->
        Printf.printf "%s\n" (string_of_token t);
        loop ()
  in
  loop ()

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = program read_token lexbuf in
    print_endline (string_of_program ast);
    print_endline "Parsed successfully."
  with
  | Error ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      Printf.eprintf "Syntax error at line %d, column %d\n" line col

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    tokens_stream Sys.argv.(1);
