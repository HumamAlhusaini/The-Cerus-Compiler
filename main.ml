open Lexer
open Parser

  let string_of_token = function
    | ADD0 _ -> "ADD"
    | SUB0 _ -> "SUB"
    | MUL0 _ -> "MUL"
    | DIV0 _ -> "DIV"
    | EQ1 _ -> "EQ"
    | CONSTANT0 (_, _) -> "CONSTANT"

let print_tokens_from_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
    let rec loop () =
      let token = read_token lexbuf in
      match token with
    | EOF _ -> print_endline "End of file."
    | t ->
        Printf.printf "%s\n" (string_of_token t);
        loop ()
  in
  loop ()


let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    print_tokens_from_file Sys.argv.(1);
