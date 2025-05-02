open List
open Sedlexing
open Parser
open Uchar

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
  (* Operators and symbols *)
  | "+", loc               -> PLUS loc
  | "-", loc               -> MINUS loc
  | "*", loc               -> STAR loc
  | "/", loc               -> SLASH loc
  | "%", loc               -> PERCENT loc
  | "^", loc               -> CARET loc
  | "!", loc               -> NOT loc
  | "&", loc               -> AND loc
  | "|", loc               -> OR loc
  | "&&", loc              -> ANDAND loc
  | "||", loc              -> OROR loc
  | "<<", loc              -> SHL loc
  | ">>", loc              -> SHR loc
  | "=", loc               -> EQ loc
  | "+=", loc              -> PLUSEQ loc
  | "-=", loc              -> MINUSEQ loc
  | "*=", loc              -> STAREQ loc
  | "/=", loc              -> SLASHEQ loc
  | "%=", loc              -> PERCENTEQ loc
  | "^=", loc              -> CARETEQ loc
  | "&=", loc              -> ANDEQ loc
  | "|=", loc              -> OREQ loc
  | "<<=", loc             -> SHLEQ loc
  | ">>=", loc             -> SHREQ loc
  | "==", loc              -> EQEQ loc
  | "!=", loc              -> NE loc
  | "<", loc               -> LT loc
  | ">", loc               -> GT loc
  | "<=", loc              -> LE loc
  | ">=", loc              -> GE loc
  | "@", loc               -> AT loc
  | "_", loc               -> UNDERSCORE loc
  | ".", loc               -> DOT loc
  | "..", loc              -> DOTDOT loc
  | "...", loc             -> DOTDOTDOT loc
  | "..=", loc             -> DOTDOTEQ loc
  | ",", loc               -> COMMA loc
  | ";", loc               -> SEMI loc
  | ":", loc               -> COLON loc
  | "::", loc              -> PATHSEP loc
  | "->", loc              -> RARROW loc
  | "=>", loc              -> FATARROW loc
  | "<-", loc              -> LARROW loc
  | "#", loc               -> POUND loc
  | "$", loc               -> DOLLAR loc
  | "?", loc               -> QUESTION loc
  | "~", loc               -> TILDE loc
  (* Parentheses and braces *)
  | "{", loc               -> LBRACE loc
  | "}", loc               -> RBRACE loc
  | "[", loc               -> LBRACK loc
  | "]", loc               -> RBRACK loc
  | "(", loc               -> LPAREN loc
  | ")", loc               -> RPAREN loc
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
      | eof -> EOF ()
      | _ -> failwith "Internal failure: Reached impossible place"


