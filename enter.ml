open Lexer
open Parser
open Cabs
open Sedlexing

let string_of_loc loc =
  match loc with
  x,y -> Printf.sprintf "column: %d, offset: %n" x y

let string_of_token = function
  | Parser.IDENT (Cabs.Raw_Ident x, loc) -> Printf.sprintf "RAW_IDENT (%s), loc: %s" x (string_of_loc loc)
  | Parser.IDENT (Cabs.Ident x, loc) -> Printf.sprintf "IDENT (%s), loc: %s" x (string_of_loc loc)
  | Parser.EOF () -> "EOF"

let print_tokens_from_file filename =
  let chan = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel chan in
  let rec loop () =
    let token = token lexbuf in
    match token with
    | Parser.EOF _ -> print_endline "End of file."
    | t ->
        print_endline (string_of_token t);
        loop ()
  in
  loop ()


let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    print_tokens_from_file Sys.argv.(1);
