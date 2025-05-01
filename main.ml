open Lexer
open Parser

let print_tokens_from_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
    let rec loop () =
      let token = read_token lexbuf in
      match token with
    | EOF _ -> print_endline "End of file."
    | t -> loop ()
  in
  loop ()


let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    print_tokens_from_file Sys.argv.(1);
