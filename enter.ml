open Lexer
open Parser
open Pre_parser
open Cabs
open Sedlexing
open Pprint
open MenhirLib.Convert


let print_tokens_from_file filename =
  let chan = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel chan in
  let rec loop () =
    let token = token lexbuf in
      let c_token = Pprint.convert_token token in 
    match c_token with
    | Parser.EOF _ -> print_endline "End of file."
    | t ->
        print_endline (string_of_token t);
        loop ()
  in
  loop ()

let print_ast_from_file filename =
  let chan = open_in filename in
    let lexbuf = Sedlexing.Utf8.from_channel chan in
      let supplier = Sedlexing.with_tokenizer token lexbuf in
        let parser = MenhirLib.Convert.Simplified.traditional2revised Pre_parser.program in
          try 
            let ast = parser supplier in
              Pprint.print_program Format.std_formatter ast
            with
|             Lexing_error msg ->
                    print_endline msg
|             _ -> print_endline "Unknown error"

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    print_tokens_from_file Sys.argv.(1);
    print_ast_from_file Sys.argv.(1);
