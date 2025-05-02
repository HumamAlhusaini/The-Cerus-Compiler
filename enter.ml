open Lexer
open Parser
open Cabs
open Sedlexing
  open Pprint

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
