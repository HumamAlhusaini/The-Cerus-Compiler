open List
open Sedlexing
open Parser
open Uchar

let oct_digit = [%sedlex.regexp? '0'..'7']


let keyword_token = function
  | "as", loc              -> AS loc
  | "break", loc           -> BREAK loc
  | "const", loc           -> CONST loc
  | "continue", loc        -> CONTINUE loc
  | "crate", loc           -> CRATE loc
  | "else", loc            -> ELSE loc
  | "enum", loc            -> ENUM loc
  | "extern", loc          -> EXTERN loc
  | "false", loc           -> FALSE loc
  | "fn", loc              -> FN loc
  | "for", loc             -> FOR loc
  | "if", loc              -> IF loc
  | "impl", loc            -> IMPL loc
  | "in", loc              -> IN loc
  | "let", loc             -> LET loc
  | "loop", loc            -> LOOP loc
  | "match", loc           -> MATCH loc
  | "mod", loc             -> MOD loc
  | "move", loc            -> MOVE loc
  | "mut", loc             -> MUT loc
  | "pub", loc             -> PUB loc
  | "ref", loc             -> REF loc
  | "return", loc          -> RETURN loc
  | "self", loc            -> SELFVALUE loc
  | "Self", loc            -> SELFTYPE loc
  | "static", loc          -> STATIC loc
  | "struct", loc          -> STRUCT loc
  | "super", loc           -> SUPER loc
  | "trait", loc           -> TRAIT loc
  | "true", loc            -> TRUE loc
  | "type", loc            -> TYPE loc
  | "unsafe", loc          -> UNSAFE loc
  | "use", loc             -> USE loc
  | "where", loc           -> WHERE loc
  | "while", loc           -> WHILE loc
  | "async", loc           -> ASYNC loc
  | "await", loc           -> AWAIT loc
  | "dyn", loc             -> DYN loc
  | "macro_rules", loc     -> MACRO_RULES loc
  | "union", loc           -> UNION loc
  | "static_lifetime", loc -> STATICLIFETIME loc
  | "safe", loc            -> SAFE loc
  | "raw", loc             -> RAW loc
  | _                      -> failwith "Unrecognized keyword"

(* Convert a Uchar.t array to a string *)
let uchar_array_to_string (arr: Uchar.t array) : string =
  Array.fold_left (fun acc uchar ->
    acc ^ (Uchar.to_char uchar |> String.make 1)
  ) "" arr

exception SyntaxError of string

let rust_keywords = [
  "as"; "break"; "const"; "continue"; "crate"; "else"; "enum"; "extern";
  "false"; "fn"; "for"; "if"; "impl"; "in"; "let"; "loop"; "match";
  "mod"; "move"; "mut"; "pub"; "ref"; "return"; "self"; "Self"; "static";
  "struct"; "super"; "trait"; "true"; "type"; "unsafe"; "use"; "where"; "while"
]

let ascii_escape = [%sedlex.regexp?
  "\\n" | "\\r" | "\\t" | "\\\\" | "\\0" | "\\x", hex_digit, hex_digit]

let byte_escape = [%sedlex.regexp?
  "\\n" | "\\r" | "\\t" | "\\\\" | "\\0" | "\\x", hex_digit, hex_digit]

let unicode_escape = [%sedlex.regexp? "\\u{" , Plus hex_digit , '}']

let quote_escape = [%sedlex.regexp? "\\\'" | "\\\""]

let is_keyword id = Stdlib.List.mem id rust_keywords

let is_reserved_keyword id = is_keyword id || id = "_"

let underscore = '_'

let identifier_or_keyword =
  [%sedlex.regexp? xid_start, Star xid_continue]

let raw_identifier =
  [%sedlex.regexp? "r#", identifier_or_keyword]

(* You can refine this with an actual keyword check in the lexer logic *)
let reserved_raw_identifier =
  [%sedlex.regexp? "r#_"]

let suffix = [%sedlex.regexp? identifier_or_keyword]

let suffix_no_e = [%sedlex.regexp? suffix]

let string_to_char_code_list (s : string) : Cabs.char_code list =
  let len = String.length s in
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (Int64.of_int (Char.code s.[i]) :: acc)
  in
  aux (len - 1) []

let rec token buf =
  match%sedlex buf with
    | white_space -> token buf
    | '\n'        -> new_line buf; token buf
    | reserved_raw_identifier -> RESERVED_RAW_IDENTIFIER (loc buf)
    | raw_identifier ->
        let uArr = Sedlexing.lexeme buf in
          let id = uchar_array_to_string uArr in
        if is_reserved_keyword (String.sub id 2 ((String.length id) - 2)) then
          IDENT (Raw_Ident id, loc buf)
        else
          IDENT (Ident id, loc buf)

    | identifier_or_keyword ->
      let uArr = Sedlexing.lexeme buf in
          let id = uchar_array_to_string uArr in
      if is_keyword id then
        keyword_token (id, loc buf)
      else
          IDENT (Ident id, loc buf)
    (* Operators and symbols *)
    | "==" -> EQEQ (loc buf)
    | "!=" -> NE (loc buf)
    | "<=" -> LE (loc buf)
    | ">=" -> GE (loc buf)
    | "<<=" -> SHLEQ (loc buf)
    | ">>=" -> SHREQ (loc buf)
    | "+=" -> PLUSEQ (loc buf)
    | "-=" -> MINUSEQ (loc buf)
    | "*=" -> STAREQ (loc buf)
    | "/=" -> SLASHEQ (loc buf)
    | "%=" -> PERCENTEQ (loc buf)
    | "^=" -> CARETEQ (loc buf)
    | "&=" -> ANDEQ (loc buf)
    | "|=" -> OREQ (loc buf)
    | "&&" -> ANDAND (loc buf)
    | "||" -> OROR (loc buf)
    | "<<" -> SHL (loc buf)
    | ">>" -> SHR (loc buf)
    | "->" -> RARROW (loc buf)
    | "=>" -> FATARROW (loc buf)
    | "<-" -> LARROW (loc buf)
    | "::" -> PATHSEP (loc buf)
    | "..." -> DOTDOTDOT (loc buf)
    | "..=" -> DOTDOTEQ (loc buf)
    | ".." -> DOTDOT (loc buf)
    | "+" -> PLUS (loc buf)
    | "-" -> MINUS (loc buf)
    | "*" -> STAR (loc buf)
    | "/" -> SLASH (loc buf)
    | "%" -> PERCENT (loc buf)
    | "^" -> CARET (loc buf)
    | "!" -> NOT (loc buf)
    | "&" -> AND (loc buf)
    | "|" -> OR (loc buf)
    | "=" -> EQ (loc buf)
    | "<" -> LT (loc buf)
    | ">" -> GT (loc buf)
    | "@" -> AT (loc buf)
    | "_" -> UNDERSCORE (loc buf)
    | "." -> DOT (loc buf)
    | "," -> COMMA (loc buf)
    | ";" -> SEMI (loc buf)
    | ":" -> COLON (loc buf)
    | "#" -> POUND (loc buf)
    | "$" -> DOLLAR (loc buf)
    | "?" -> QUESTION (loc buf)
    | "~" -> TILDE (loc buf)
    | "{" -> LBRACE (loc buf)
    | "}" -> RBRACE (loc buf)
    | "[" -> LBRACK (loc buf)
    | "]" -> RBRACK (loc buf)
    | "(" -> LPAREN (loc buf)
    | ")" -> RPAREN (loc buf)
    | '"' -> read_string (Buffer.create 17) buf 
    | eof -> EOF ()
    | _ -> failwith "Internal failure: Reached impossible place"

and read_string buffer buf =
  match%sedlex buf with
  | "\""   ->    STRING_LIT (string_to_char_code_list (Buffer.contents buffer), loc buf)
  | '\\', 'n'  -> Buffer.add_char buffer '\n'; read_string buffer buf
| Plus (Compl (Chars "\"\\")) ->
    Buffer.add_string buffer (Utf8.lexeme buf);
    read_string buffer buf
  | eof -> failwith "String is not terminated"
  | _ -> failwith "illegal string char"
