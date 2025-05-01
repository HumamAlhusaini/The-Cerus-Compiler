open Lexer
open Parser
open Cabs

let string_of_loc loc =
  Printf.sprintf "Filename:%s - LineNumber: %d (byte %d, id %d)" loc.filename loc.lineno loc.byteno loc.ident
let string_of_token = function

  | Parser.CONSTANT (CONST_INT s, loc) -> Printf.sprintf "CONSTANT(INT %s) at %s" s (string_of_loc loc)
  | Parser.PLUS loc -> Printf.sprintf "PLUS at %s" (string_of_loc loc)
  | Parser.MINUS loc -> Printf.sprintf "MINUS at %s" (string_of_loc loc)
  | Parser.STAR loc -> Printf.sprintf "STAR at %s" (string_of_loc loc)
  | Parser.SLASH loc -> Printf.sprintf "SLASH at %s" (string_of_loc loc)
  | Parser.EQ loc -> Printf.sprintf "EQ at %s" (string_of_loc loc)
  | Parser.EOF () -> "EOF"

let print_tokens_from_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let rec loop () =
    let token = read_token lexbuf in
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
