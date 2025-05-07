  open Parser
  open Pre_parser
  open Cabs
  open Uchar

let string_of_loc (loc : Lexing.position) =
  Printf.sprintf "line: %d, offset: %d" loc.pos_lnum (loc.pos_cnum - loc.pos_bol)

let uchar_of_int64 (i : int64) : Uchar.t =
  let n = Int64.to_int i in
  if not (Uchar.is_valid n) then
    failwith "int64 value out of valid Unicode scalar range"
  else
    Uchar.of_int n

let utf8_of_char_code_list (codes : int64 list) : string =
  let buf = Buffer.create 16 in
  Stdlib.List.iter
    (fun code ->
      let i = Int64.to_int code in
      if Uchar.is_valid i then
        Buffer.add_utf_8_uchar buf (Uchar.of_int i)
      else
        Buffer.add_utf_8_uchar buf (Uchar.of_int 0xFFFD)
    )
    codes;
  Buffer.contents buf

let convert_token = function
  | Pre_parser.RAW_IDENT (x, loc) -> Parser.RAW_IDENT (x, loc)
  | Pre_parser.IDENT (x, loc) -> Parser.IDENT (x, loc)

  | Pre_parser.AS loc -> Parser.AS loc
  | Pre_parser.BREAK loc -> Parser.BREAK loc
  | Pre_parser.CONST loc -> Parser.CONST loc
  | Pre_parser.CONTINUE loc -> Parser.CONTINUE loc
  | Pre_parser.CRATE loc -> Parser.CRATE loc
  | Pre_parser.ELSE loc -> Parser.ELSE loc
  | Pre_parser.ENUM loc -> Parser.ENUM loc
  | Pre_parser.EXTERN loc -> Parser.EXTERN loc
  | Pre_parser.FALSE loc -> Parser.FALSE loc
  | Pre_parser.FN loc -> Parser.FN loc
  | Pre_parser.FOR loc -> Parser.FOR loc
  | Pre_parser.IF loc -> Parser.IF loc
  | Pre_parser.IMPL loc -> Parser.IMPL loc
  | Pre_parser.IN loc -> Parser.IN loc
  | Pre_parser.LET loc -> Parser.LET loc
  | Pre_parser.LOOP loc -> Parser.LOOP loc
  | Pre_parser.MATCH loc -> Parser.MATCH loc
  | Pre_parser.MOD loc -> Parser.MOD loc
  | Pre_parser.MOVE loc -> Parser.MOVE loc
  | Pre_parser.MUT loc -> Parser.MUT loc
  | Pre_parser.PUB loc -> Parser.PUB loc
  | Pre_parser.REF loc -> Parser.REF loc
  | Pre_parser.RETURN loc -> Parser.RETURN loc
  | Pre_parser.SELFVALUE loc -> Parser.SELFVALUE loc
  | Pre_parser.SELFTYPE loc -> Parser.SELFTYPE loc
  | Pre_parser.STATIC loc -> Parser.STATIC loc
  | Pre_parser.STRUCT loc -> Parser.STRUCT loc
  | Pre_parser.SUPER loc -> Parser.SUPER loc
  | Pre_parser.TRAIT loc -> Parser.TRAIT loc
  | Pre_parser.TRUE loc -> Parser.TRUE loc
  | Pre_parser.TYPE loc -> Parser.TYPE loc
  | Pre_parser.UNSAFE loc -> Parser.UNSAFE loc
  | Pre_parser.USE loc -> Parser.USE loc
  | Pre_parser.WHERE loc -> Parser.WHERE loc
  | Pre_parser.WHILE loc -> Parser.WHILE loc
  | Pre_parser.ASYNC loc -> Parser.ASYNC loc
  | Pre_parser.AWAIT loc -> Parser.AWAIT loc
  | Pre_parser.DYN loc -> Parser.DYN loc
  | Pre_parser.MACRO_RULES loc -> Parser.MACRO_RULES loc
  | Pre_parser.UNION loc -> Parser.UNION loc
  | Pre_parser.STATICLIFETIME loc -> Parser.STATICLIFETIME loc
  | Pre_parser.SAFE loc -> Parser.SAFE loc
  | Pre_parser.RAW loc -> Parser.RAW loc

  (* Operators and punctuation *)
  | Pre_parser.PLUS loc -> Parser.PLUS loc
  | Pre_parser.MINUS loc -> Parser.MINUS loc
  | Pre_parser.STAR loc -> Parser.STAR loc
  | Pre_parser.SLASH loc -> Parser.SLASH loc
  | Pre_parser.PERCENT loc -> Parser.PERCENT loc
  | Pre_parser.CARET loc -> Parser.CARET loc
  | Pre_parser.NOT loc -> Parser.NOT loc
  | Pre_parser.AND loc -> Parser.AND loc
  | Pre_parser.OR loc -> Parser.OR loc
  | Pre_parser.ANDAND loc -> Parser.ANDAND loc
  | Pre_parser.OROR loc -> Parser.OROR loc
  | Pre_parser.SHL loc -> Parser.SHL loc
  | Pre_parser.SHR loc -> Parser.SHR loc
  | Pre_parser.EQ loc -> Parser.EQ loc
  | Pre_parser.PLUSEQ loc -> Parser.PLUSEQ loc
  | Pre_parser.MINUSEQ loc -> Parser.MINUSEQ loc
  | Pre_parser.STAREQ loc -> Parser.STAREQ loc
  | Pre_parser.SLASHEQ loc -> Parser.SLASHEQ loc
  | Pre_parser.PERCENTEQ loc -> Parser.PERCENTEQ loc
  | Pre_parser.CARETEQ loc -> Parser.CARETEQ loc
  | Pre_parser.ANDEQ loc -> Parser.ANDEQ loc
  | Pre_parser.OREQ loc -> Parser.OREQ loc
  | Pre_parser.SHLEQ loc -> Parser.SHLEQ loc
  | Pre_parser.SHREQ loc -> Parser.SHREQ loc
  | Pre_parser.EQEQ loc -> Parser.EQEQ loc
  | Pre_parser.NE loc -> Parser.NE loc
  | Pre_parser.LT loc -> Parser.LT loc
  | Pre_parser.GT loc -> Parser.GT loc
  | Pre_parser.LE loc -> Parser.LE loc
  | Pre_parser.GE loc -> Parser.GE loc
  | Pre_parser.AT loc -> Parser.AT loc
  | Pre_parser.UNDERSCORE loc -> Parser.UNDERSCORE loc
  | Pre_parser.DOT loc -> Parser.DOT loc
  | Pre_parser.DOTDOT loc -> Parser.DOTDOT loc
  | Pre_parser.DOTDOTDOT loc -> Parser.DOTDOTDOT loc
  | Pre_parser.DOTDOTEQ loc -> Parser.DOTDOTEQ loc
  | Pre_parser.COMMA loc -> Parser.COMMA loc
  | Pre_parser.SEMI loc -> Parser.SEMI loc
  | Pre_parser.COLON loc -> Parser.COLON loc
  | Pre_parser.RESERVED_RAW_IDENTIFIER loc -> Parser.RESERVED_RAW_IDENTIFIER loc
  | Pre_parser.PATHSEP loc -> Parser.PATHSEP loc
  | Pre_parser.RARROW loc -> Parser.RARROW loc
  | Pre_parser.FATARROW loc -> Parser.FATARROW loc
  | Pre_parser.LARROW loc -> Parser.LARROW loc
  | Pre_parser.POUND loc -> Parser.POUND loc
  | Pre_parser.DOLLAR loc -> Parser.DOLLAR loc
  | Pre_parser.QUESTION loc -> Parser.QUESTION loc
  | Pre_parser.TILDE loc -> Parser.TILDE loc
  | Pre_parser.LBRACE loc -> Parser.LBRACE loc
  | Pre_parser.RBRACE loc -> Parser.RBRACE loc
  | Pre_parser.LBRACK loc -> Parser.LBRACK loc
  | Pre_parser.RBRACK loc -> Parser.RBRACK loc
  | Pre_parser.LPAREN loc -> Parser.LPAREN loc
  | Pre_parser.RPAREN loc -> Parser.RPAREN loc

  (* Literals and constants *)
  | Pre_parser.STRING_LIT (s, loc) -> Parser.STRING_LIT (s, loc)
  | Pre_parser.BYTE_STRING (s, loc) -> Parser.BYTE_STRING (s, loc)
  | Pre_parser.RAW_STRING_LIT (s, loc) -> Parser.RAW_STRING_LIT (s, loc)
  | Pre_parser.RAW_BYTE_STRING (s, loc) -> Parser.RAW_BYTE_STRING (s, loc)
  | Pre_parser.RAW_C_STRING (s, loc) -> Parser.RAW_C_STRING (s, loc)
  | Pre_parser.C_STRING (s, loc) -> Parser.C_STRING (s, loc)
  | Pre_parser.BYTE (b, loc) -> Parser.BYTE (b, loc)
  | Pre_parser.CHAR_LIT (c, loc) -> Parser.CHAR_LIT (c, loc)
  | Pre_parser.CONSTANT (Cabs.INT_LIT s, loc) -> Parser.CONSTANT (Cabs.INT_LIT s, loc)
  | Pre_parser.CONSTANT (Cabs.FLOAT_LIT s, loc) -> Parser.CONSTANT (Cabs.FLOAT_LIT s, loc)

  | Pre_parser.EOF -> Parser.EOF ()


let string_of_token = function
  | Parser.RAW_IDENT  (x, loc) -> Printf.sprintf "RAW_IDENT (%s), loc: %s" x (string_of_loc loc)
  | Parser.IDENT (x, loc) -> Printf.sprintf "IDENT (%s), loc: %s" x (string_of_loc loc)

  | Parser.AS loc -> Printf.sprintf "AS, loc: %s" (string_of_loc loc)
  | Parser.BREAK loc -> Printf.sprintf "BREAK, loc: %s" (string_of_loc loc)
  | Parser.CONST loc -> Printf.sprintf "CONST, loc: %s" (string_of_loc loc)
  | Parser.CONTINUE loc -> Printf.sprintf "CONTINUE, loc: %s" (string_of_loc loc)
  | Parser.CRATE loc -> Printf.sprintf "CRATE, loc: %s" (string_of_loc loc)
  | Parser.ELSE loc -> Printf.sprintf "ELSE, loc: %s" (string_of_loc loc)
  | Parser.ENUM loc -> Printf.sprintf "ENUM, loc: %s" (string_of_loc loc)
  | Parser.EXTERN loc -> Printf.sprintf "EXTERN, loc: %s" (string_of_loc loc)
  | Parser.FALSE loc -> Printf.sprintf "FALSE, loc: %s" (string_of_loc loc)
  | Parser.FN loc -> Printf.sprintf "FN, loc: %s" (string_of_loc loc)
  | Parser.FOR loc -> Printf.sprintf "FOR, loc: %s" (string_of_loc loc)
  | Parser.IF loc -> Printf.sprintf "IF, loc: %s" (string_of_loc loc)
  | Parser.IMPL loc -> Printf.sprintf "IMPL, loc: %s" (string_of_loc loc)
  | Parser.IN loc -> Printf.sprintf "IN, loc: %s" (string_of_loc loc)
  | Parser.LET loc -> Printf.sprintf "LET, loc: %s" (string_of_loc loc)
  | Parser.LOOP loc -> Printf.sprintf "LOOP, loc: %s" (string_of_loc loc)
  | Parser.MATCH loc -> Printf.sprintf "MATCH, loc: %s" (string_of_loc loc)
  | Parser.MOD loc -> Printf.sprintf "MOD, loc: %s" (string_of_loc loc)
  | Parser.MOVE loc -> Printf.sprintf "MOVE, loc: %s" (string_of_loc loc)
  | Parser.MUT loc -> Printf.sprintf "MUT, loc: %s" (string_of_loc loc)
  | Parser.PUB loc -> Printf.sprintf "PUB, loc: %s" (string_of_loc loc)
  | Parser.REF loc -> Printf.sprintf "REF, loc: %s" (string_of_loc loc)
  | Parser.RETURN loc -> Printf.sprintf "RETURN, loc: %s" (string_of_loc loc)
  | Parser.SELFVALUE loc -> Printf.sprintf "SELFVALUE, loc: %s" (string_of_loc loc)
  | Parser.SELFTYPE loc -> Printf.sprintf "SELFTYPE, loc: %s" (string_of_loc loc)
  | Parser.STATIC loc -> Printf.sprintf "STATIC, loc: %s" (string_of_loc loc)
  | Parser.STRUCT loc -> Printf.sprintf "STRUCT, loc: %s" (string_of_loc loc)
  | Parser.SUPER loc -> Printf.sprintf "SUPER, loc: %s" (string_of_loc loc)
  | Parser.TRAIT loc -> Printf.sprintf "TRAIT, loc: %s" (string_of_loc loc)
  | Parser.TRUE loc -> Printf.sprintf "TRUE, loc: %s" (string_of_loc loc)
  | Parser.TYPE loc -> Printf.sprintf "TYPE, loc: %s" (string_of_loc loc)
  | Parser.UNSAFE loc -> Printf.sprintf "UNSAFE, loc: %s" (string_of_loc loc)
  | Parser.USE loc -> Printf.sprintf "USE, loc: %s" (string_of_loc loc)
  | Parser.WHERE loc -> Printf.sprintf "WHERE, loc: %s" (string_of_loc loc)
  | Parser.WHILE loc -> Printf.sprintf "WHILE, loc: %s" (string_of_loc loc)
  | Parser.ASYNC loc -> Printf.sprintf "ASYNC, loc: %s" (string_of_loc loc)
  | Parser.AWAIT loc -> Printf.sprintf "AWAIT, loc: %s" (string_of_loc loc)
  | Parser.DYN loc -> Printf.sprintf "DYN, loc: %s" (string_of_loc loc)
  | Parser.MACRO_RULES loc -> Printf.sprintf "MACRO_RULES, loc: %s" (string_of_loc loc)
  | Parser.UNION loc -> Printf.sprintf "UNION, loc: %s" (string_of_loc loc)
  | Parser.STATICLIFETIME loc -> Printf.sprintf "STATICLIFETIME, loc: %s" (string_of_loc loc)
  | Parser.SAFE loc -> Printf.sprintf "SAFE, loc: %s" (string_of_loc loc)
  | Parser.RAW loc -> Printf.sprintf "RAW, loc: %s" (string_of_loc loc)

  (* Operators and punctuation *)
  | Parser.PLUS loc -> Printf.sprintf "PLUS, loc: %s" (string_of_loc loc)
  | Parser.MINUS loc -> Printf.sprintf "MINUS, loc: %s" (string_of_loc loc)
  | Parser.STAR loc -> Printf.sprintf "STAR, loc: %s" (string_of_loc loc)
  | Parser.SLASH loc -> Printf.sprintf "SLASH, loc: %s" (string_of_loc loc)
  | Parser.PERCENT loc -> Printf.sprintf "PERCENT, loc: %s" (string_of_loc loc)
  | Parser.CARET loc -> Printf.sprintf "CARET, loc: %s" (string_of_loc loc)
  | Parser.NOT loc -> Printf.sprintf "NOT, loc: %s" (string_of_loc loc)
  | Parser.AND loc -> Printf.sprintf "AND, loc: %s" (string_of_loc loc)
  | Parser.OR loc -> Printf.sprintf "OR, loc: %s" (string_of_loc loc)
  | Parser.ANDAND loc -> Printf.sprintf "ANDAND, loc: %s" (string_of_loc loc)
  | Parser.OROR loc -> Printf.sprintf "OROR, loc: %s" (string_of_loc loc)
  | Parser.SHL loc -> Printf.sprintf "SHL, loc: %s" (string_of_loc loc)
  | Parser.SHR loc -> Printf.sprintf "SHR, loc: %s" (string_of_loc loc)
  | Parser.EQ loc -> Printf.sprintf "EQ, loc: %s" (string_of_loc loc)
  | Parser.PLUSEQ loc -> Printf.sprintf "PLUSEQ, loc: %s" (string_of_loc loc)
  | Parser.MINUSEQ loc -> Printf.sprintf "MINUSEQ, loc: %s" (string_of_loc loc)
  | Parser.STAREQ loc -> Printf.sprintf "STAREQ, loc: %s" (string_of_loc loc)
  | Parser.SLASHEQ loc -> Printf.sprintf "SLASHEQ, loc: %s" (string_of_loc loc)
  | Parser.PERCENTEQ loc -> Printf.sprintf "PERCENTEQ, loc: %s" (string_of_loc loc)
  | Parser.CARETEQ loc -> Printf.sprintf "CARETEQ, loc: %s" (string_of_loc loc)
  | Parser.ANDEQ loc -> Printf.sprintf "ANDEQ, loc: %s" (string_of_loc loc)
  | Parser.OREQ loc -> Printf.sprintf "OREQ, loc: %s" (string_of_loc loc)
  | Parser.SHLEQ loc -> Printf.sprintf "SHLEQ, loc: %s" (string_of_loc loc)
  | Parser.SHREQ loc -> Printf.sprintf "SHREQ, loc: %s" (string_of_loc loc)
  | Parser.EQEQ loc -> Printf.sprintf "EQEQ, loc: %s" (string_of_loc loc)
  | Parser.NE loc -> Printf.sprintf "NE, loc: %s" (string_of_loc loc)
  | Parser.LT loc -> Printf.sprintf "LT, loc: %s" (string_of_loc loc)
  | Parser.GT loc -> Printf.sprintf "GT, loc: %s" (string_of_loc loc)
  | Parser.LE loc -> Printf.sprintf "LE, loc: %s" (string_of_loc loc)
  | Parser.GE loc -> Printf.sprintf "GE, loc: %s" (string_of_loc loc)
  | Parser.AT loc -> Printf.sprintf "AT, loc: %s" (string_of_loc loc)
  | Parser.UNDERSCORE loc -> Printf.sprintf "UNDERSCORE, loc: %s" (string_of_loc loc)
  | Parser.DOT loc -> Printf.sprintf "DOT, loc: %s" (string_of_loc loc)
  | Parser.DOTDOT loc -> Printf.sprintf "DOTDOT, loc: %s" (string_of_loc loc)
  | Parser.DOTDOTDOT loc -> Printf.sprintf "DOTDOTDOT, loc: %s" (string_of_loc loc)
  | Parser.DOTDOTEQ loc -> Printf.sprintf "DOTDOTEQ, loc: %s" (string_of_loc loc)
  | Parser.COMMA loc -> Printf.sprintf "COMMA, loc: %s" (string_of_loc loc)
  | Parser.SEMI loc -> Printf.sprintf "SEMI, loc: %s" (string_of_loc loc)
  | Parser.COLON loc -> Printf.sprintf "COLON, loc: %s" (string_of_loc loc)
  | Parser.RESERVED_RAW_IDENTIFIER loc -> Printf.sprintf "RESERVED_RAW_IDENTIFIER, loc: %s" (string_of_loc loc)
  | Parser.PATHSEP loc -> Printf.sprintf "PATHSEP, loc: %s" (string_of_loc loc)
  | Parser.RARROW loc -> Printf.sprintf "RARROW, loc: %s" (string_of_loc loc)
  | Parser.FATARROW loc -> Printf.sprintf "FATARROW, loc: %s" (string_of_loc loc)
  | Parser.LARROW loc -> Printf.sprintf "LARROW, loc: %s" (string_of_loc loc)
  | Parser.POUND loc -> Printf.sprintf "POUND, loc: %s" (string_of_loc loc)
  | Parser.DOLLAR loc -> Printf.sprintf "DOLLAR, loc: %s" (string_of_loc loc)
  | Parser.QUESTION loc -> Printf.sprintf "QUESTION, loc: %s" (string_of_loc loc)
  | Parser.TILDE loc -> Printf.sprintf "TILDE, loc: %s" (string_of_loc loc)
  | Parser.LBRACE loc -> Printf.sprintf "LBRACE, loc: %s" (string_of_loc loc)
  | Parser.RBRACE loc -> Printf.sprintf "RBRACE, loc: %s" (string_of_loc loc)
  | Parser.LBRACK loc ->  Printf.sprintf"LBRACK, loc: %s" (string_of_loc loc)
  | Parser.RBRACK loc -> Printf.sprintf "RBRACK, loc: %s" (string_of_loc loc)
  | Parser.LPAREN loc ->  Printf.sprintf"LPAREN, loc: %s" (string_of_loc loc)
  | Parser.RPAREN loc -> Printf.sprintf "RPAREN, loc: %s" (string_of_loc loc)
  | Parser.STRING_LIT (str, loc) ->
    let utf8_str = utf8_of_char_code_list str in
      Printf.sprintf "STRING_LIT(%s), loc: %s" utf8_str (string_of_loc loc)
  | Parser.BYTE_STRING (str, loc) ->
    let utf8_str = utf8_of_char_code_list str in
      Printf.sprintf "BYTE_STRING(%s), loc: %s" utf8_str (string_of_loc loc)
  | Parser.RAW_STRING_LIT (str, loc) ->
    let utf8_str = utf8_of_char_code_list str in
      Printf.sprintf "RAW_STRING_LIT(%s), loc: %s" utf8_str (string_of_loc loc)
  | Parser.RAW_BYTE_STRING (str, loc) ->
    let utf8_str = utf8_of_char_code_list str in
      Printf.sprintf "RAW_BYTE_STRING(%s), loc: %s" utf8_str (string_of_loc loc)
  | Parser.RAW_C_STRING (str, loc) ->
    let utf8_str = utf8_of_char_code_list str in
      Printf.sprintf "RAW_C_STRING(%s), loc: %s" utf8_str (string_of_loc loc)
  | Parser.C_STRING (str, loc) ->
    let utf8_str = utf8_of_char_code_list str in
      Printf.sprintf "C_STRING(%s), loc: %s" utf8_str (string_of_loc loc)
  | Parser.BYTE (str, loc) ->
    let uchar = uchar_of_int64 str in
      Printf.sprintf "BYTE(%c), loc: %s" (Uchar.to_char uchar) (string_of_loc loc)
  | Parser.CHAR_LIT (str, loc) ->
    let uchar = uchar_of_int64 str in
      Printf.sprintf "CHAR_LIT(%c), loc: %s" (Uchar.to_char uchar) (string_of_loc loc)
  | Parser.CONSTANT (Cabs.INT_LIT str, loc) ->
      Printf.sprintf "INT_LIT(%s), loc %s" str (string_of_loc loc)
  | Parser.CONSTANT (Cabs.FLOAT_LIT str, loc) ->
      Printf.sprintf "FLOAT_LIT(%s), loc %s" str (string_of_loc loc)
  | Parser.EOF loc -> "EOF"

open Format

let rec print_program fmt items =
  Stdlib.List.iter (print_item fmt) items

and print_item fmt = function
  | Cabs.VISITEM (attrs, vis_item) ->
      print_outer_attrs fmt attrs;
      print_vis_item fmt vis_item

and print_vis_item fmt = function
  | Cabs.MODULE m -> print_module fmt m
  | Cabs.EXTERN_CRATE ext -> print_extern_crate fmt ext
  | Cabs.USE_DECLARATION tree -> print_use_decl fmt tree

(* Use declaration *)
and print_use_decl fmt = function
  | Cabs.USE_DECL tree -> fprintf fmt "use %a;\n" print_use_tree tree

and print_use_tree fmt = function
  | Cabs.USE_TREE None -> fprintf fmt "*"
  | Cabs.USE_TREE (Some path) -> fprintf fmt "%a::*" print_simple_path path
  | Cabs.USE_TREE_LIST (None, trees) ->
      fprintf fmt "{%a}" print_use_trees trees
  | Cabs.USE_TREE_LIST (Some path, trees) ->
      fprintf fmt "%a::{%a}" print_simple_path path print_use_trees trees
  | Cabs.USE_TREE_ID (path, as_ident) ->
      fprintf fmt "%a%a" print_simple_path path print_as_ident_opt as_ident

and print_use_trees fmt trees =
  Format.pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") print_use_tree fmt trees

and print_as_ident_opt fmt = function
  | Some (Cabs.ID_OPT id) -> fprintf fmt " as %s" (string_of_ident id)
  | Some Cabs.UNDERSCORE_OPT -> fprintf fmt " as _"
  | None -> ()
(*Use decl*)

and print_extern_crate fmt = function
  | Cabs.EXT_CRATE_CLAUSE (ref, clause) ->
      fprintf fmt "extern crate %s as %s;\n"
        (string_of_crate_ref ref)
        (string_of_as_clause clause)

and string_of_crate_ref = function
  | Cabs.ID_CRATE_REF id -> string_of_ident id
  | Cabs.SELF_CRATE_REF -> "self"

and string_of_as_clause = function
  | Some (Cabs.ID_AS_CLAUSE id) -> string_of_ident id
  | Some Cabs.UNDERSCORE_AS_CLAUSE -> "_"
  | None -> ""  (* or raise an error if this case shouldn't happen *)

and print_module fmt = function
  | MOD_BLOCK (unsafe, name) ->
      fprintf fmt "%smod %s;" (if unsafe then "unsafe " else "") (string_of_ident name)
  | MOD_DEC (unsafe, name, attrs, items) ->
      fprintf fmt "%smod %s {@." (if unsafe then "unsafe " else "") (string_of_ident name);
      print_inner_attrs fmt attrs;
      print_program fmt items;
      fprintf fmt "@.}"

and print_outer_attrs fmt = function
  | [] -> ()
  | attrs ->
      Stdlib.List.iter (function
        | OUTER_ATTRIBUTE attr -> fprintf fmt "#[%a]@." print_attr attr
      ) attrs

and print_inner_attrs fmt = function
  | [] -> ()
  | attrs ->
      Stdlib.List.iter (function
        | INNER_ATTRIBUTE attr -> fprintf fmt "#![%a]@." print_attr attr
      ) attrs

and print_attr fmt = function
  | SAFE_ATTR (path, input_opt) ->
      fprintf fmt "%a%a" print_simple_path path print_maybe_attr_input input_opt
  | UNSAFE_ATTR (path, input_opt) ->
      fprintf fmt "unsafe %a%a" print_simple_path path print_maybe_attr_input input_opt

and print_maybe_attr_input fmt = function
  | None -> ()
  | Some input -> fprintf fmt " = %a" print_attr_input input

and print_simple_path fmt (SIMPLE_PATH segments) =
  let print_segment fmt = function
    | SIMPLE_PATH_SEGMENT_IDENT id -> fprintf fmt "%s" (string_of_ident id)
    | SIMPLE_PATH_SEGMENT_SUPER -> fprintf fmt "super"
    | SIMPLE_PATH_SEGMENT_SELF -> fprintf fmt "self"
    | SIMPLE_PATH_SEGMENT_CRATE -> fprintf fmt "crate"
  in
  Format.pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "::") print_segment fmt segments

and print_attr_input fmt = function
  | ATTR_INPUT_EXP e -> print_expression fmt e

and print_expression fmt = function
  | EXPRESSION_WITHOUT_BLOCK (attrs, expr) ->
      print_outer_attrs fmt attrs;
      print_expression_no_block fmt expr

and print_expression_no_block fmt = function
  | UNDERSCORE_EXPRESSION -> fprintf fmt "_"

and string_of_ident = function
  | IDENT x -> x
  | RAW_IDENT x -> x
