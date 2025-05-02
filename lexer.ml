open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let underscore = '_'

let identifier_or_keyword =
  [%sedlex.regexp? xid_start, Star xid_continue]

let raw_identifier =
  [%sedlex.regexp? "r#", identifier_or_keyword]

(* You can refine this with an actual keyword check in the lexer logic *)
let reserved_raw_identifier =
  [%sedlex.regexp? "r#_"]
let rec token buf =
  match%sedlex buf with
    | white_space -> token buf
    | reserved_raw_identifier -> RESERVED_RAW_IDENTIFIER
    | raw_identifier ->
        let id = Sedlexing.lexeme lexbuf in
        if is_reserved_keyword (String.sub id 2 ((String.length id) - 2)) then
          RAW_IDENTIFIER id
        else
          IDENTIFIER id  (* discourage raw non-keywords, but it's legal *)

    | identifier_or_keyword ->
      let id = Sedlexing.lexeme lexbuf in
      if is_keyword id then
        keyword_token id
      else
        IDENTIFIER id
      | eof -> print_endline "\tEnd"
      | _ -> failwith "Internal failure: Reached impossible place"
