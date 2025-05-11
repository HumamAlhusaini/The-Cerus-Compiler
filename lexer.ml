
exception Lexing_error of string

let underscore =  [%sedlex.regexp? '_']

let identifier_or_keyword =
  [%sedlex.regexp? xid_start, Star xid_continue]

let non_keyword_identifier =
  [%sedlex.regexp?
    xid_start, Star xid_continue, Compl (Chars " \t\n\r") |  (* Continue for any character that's not a space or newline *)
    Compl (Chars "as break const continue crate else enum extern false fn for if impl in let                   
     loop match mod move mut pub ref return selfvalue selftype static struct super trait true type unsafe use where while async await dyn           
     macro_rules union static lifetime safe")]

let suffix = [%sedlex.regexp? identifier_or_keyword]

(* Suffix must be an identifier-like string that does NOT begin with e or E *)
let suffix_start = [%sedlex.regexp? ('a'..'d' | 'f'..'z' | 'A'..'D' | 'F'..'Z' | '_')]
let suffix_rest = [%sedlex.regexp? 'a'..'z' | 'A'..'Z' | '_' | '0'..'9']

let suffix_no_e = [%sedlex.regexp? suffix_start, Star suffix_rest]

let dec_digit = [%sedlex.regexp? '0'..'9']
let bin_digit = [%sedlex.regexp? '0' | '1']
let oct_digit = [%sedlex.regexp? '0'..'7']

let dec_literal = [%sedlex.regexp? dec_digit, Star (dec_digit | underscore)]
let bin_literal = [%sedlex.regexp? "0b", Star (bin_digit | underscore), bin_digit, Star (bin_digit | underscore)]
let oct_literal = [%sedlex.regexp? "0o", Star (oct_digit | underscore), oct_digit, Star (oct_digit | underscore)]
let hex_literal = [%sedlex.regexp? "0x", Star (hex_digit | underscore), hex_digit, Star (hex_digit | underscore)]

let integer_literal = [%sedlex.regexp? (dec_literal | bin_literal | oct_literal | hex_literal), Opt suffix_no_e]
 
let tuple_index = [%sedlex.regexp? integer_literal]

let float_exponent =
  [%sedlex.regexp? ('e' | 'E'),  Opt ('+' | '-'), Star(dec_digit | white_space), dec_digit, Star (dec_digit | white_space)]

let float_literal = [%sedlex.regexp? dec_literal, '.' | dec_literal, '.', dec_literal, Opt suffix_no_e | dec_literal, Opt ('.',dec_literal),
  float_exponent, Opt suffix]

let reserved_number =
  [%sedlex.regexp?
    (bin_literal, ('2'..'9')) |
    (oct_literal, ('8'..'9')) |
    (bin_literal | oct_literal | hex_literal), '.', Compl (xid_start | Chars "._") |
    (bin_literal | oct_literal), ('e' | 'E') |
    "0b", Star '_', (Compl (Chars "0123456789")) |
    "0o", Star '_', (Compl (Chars "01234567")) |
    "0x", Star '_', (Compl (Chars "0123456789abcdefABCDEF")) |
    dec_literal, Opt ('.', dec_literal), ('e' | 'E'), Opt ('+' | '-')
  ]

let rust_keywords = [
  "as"; "break"; "const"; "continue"; "crate"; "else"; "enum"; "extern";
  "false"; "fn"; "for"; "if"; "impl"; "in"; "let"; "loop"; "match";
  "mod"; "move"; "mut"; "pub"; "ref"; "return"; "self"; "Self"; "static";
  "struct"; "super"; "trait"; "true"; "type"; "unsafe"; "use"; "where"; "while"
]

let ascii_escape =
  [%sedlex.regexp?
"\\n" | "\\r" | "\\t" | "\\\\" | "\\0" | "\\x", hex_digit, hex_digit]

let byte_escape = [%sedlex.regexp?
"\\n" | "\\r" | "\\t" | "\\\\" | "\\0" | "\\x", hex_digit, hex_digit]

let unicode_escape = [%sedlex.regexp? "\\u{" , Plus hex_digit , '}']

let quote_escape = [%sedlex.regexp? "\\\'" | "\\\""]

let is_keyword id = Stdlib.List.mem id rust_keywords

let is_reserved_keyword id = is_keyword id || id = "_"

let raw_identifier =[%sedlex.regexp? "r#", identifier_or_keyword]

let reserved_raw_identifier = [%sedlex.regexp? "r#_"]

let reserved_raw_lifetime = [%sedlex.regexp? "'r#_", Compl (Chars "'")]

let raw_lifetime = [%sedlex.regexp? "'r#", identifier_or_keyword, "'"]

let lifetime_or_label = [%sedlex.regexp? "'", non_keyword_identifier, Compl (Chars "'")]

let lifetime_token = [%sedlex.regexp? "'", identifier_or_keyword, Compl (Chars "'")
  | "'_", Compl( Chars "'") | raw_lifetime]

open List
open Sedlexing
open Pre_parser
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
  | "*mut", loc            -> RAW_MUT loc
  | "&mut", loc            -> AMPMUT loc
  | "*const", loc          -> RAW_CONST loc
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
  | "safe", loc            -> SAFE loc
  | "raw", loc             -> RAW loc
  | _                      -> failwith "Unrecognized keyword"
                              
(* Convert a Uchar.t array to a string *)
let uchar_array_to_string (arr: Uchar.t array) : string =
  Array.fold_left (fun acc uchar ->
    acc ^ (Uchar.to_char uchar |> Stdlib.String.make 1)
) "" arr                      
                              
                              
let string_to_char_code_list (s : string) : Cabs.char_code list =
  let len = Stdlib.String.length s in
  let rec aux i acc =         
    if i < 0 then acc         
    else aux (i - 1) (Stdlib.Int64.of_int (Stdlib.Char.code (Stdlib.String.get s i)) :: acc)
  in                          
aux (len - 1) []              
                              
let rec token buf =           
  match%sedlex buf with       
    | white_space -> token buf
    | '\n'        -> new_line buf; token buf
    | reserved_raw_identifier -> RESERVED_RAW_IDENTIFIER (lexing_position_start buf)
    | raw_identifier ->
        let uArr = Sedlexing.lexeme buf in
          let id = uchar_array_to_string uArr in
        if is_reserved_keyword (Stdlib.String.sub id 2 ((Stdlib.String.length id) - 2)) then
        keyword_token (id, lexing_position_start buf)
        else
          RAW_IDENT (id, lexing_position_start buf)

    | identifier_or_keyword ->
      let uArr = Sedlexing.lexeme buf in
          let id = uchar_array_to_string uArr in
      if is_keyword id then
        keyword_token (id, lexing_position_start buf)
      else
          IDENT (id, lexing_position_start buf)
    (* Operators and symbols *)
    | "==" -> EQEQ (lexing_position_start buf)
    | "!=" -> NE (lexing_position_start buf)
    | "<=" -> LE (lexing_position_start buf)
    | ">=" -> GE (lexing_position_start buf)
    | "<<=" ->SHLEQ (lexing_position_start buf)
    | ">>=" ->SHREQ (lexing_position_start buf)
    | "+=" -> PLUSEQ (lexing_position_start buf)
    | "-=" -> MINUSEQ (lexing_position_start buf)
    | "*=" -> STAREQ (lexing_position_start buf)
    | "/=" -> SLASHEQ (lexing_position_start buf)
    | "%=" -> PERCENTEQ (lexing_position_start buf)
    | "^=" -> CARETEQ (lexing_position_start buf)
    | "&=" -> ANDEQ (lexing_position_start buf)
    | "|=" -> OREQ (lexing_position_start buf)
    | "&&" -> ANDAND (lexing_position_start buf)
    | "||" -> OROR (lexing_position_start buf)
    | "<<" -> SHL (lexing_position_start buf)
    | ">>" -> SHR (lexing_position_start buf)
    | "->" -> RARROW (lexing_position_start buf)
    | "=>" -> FATARROW (lexing_position_start buf)
    | "<-" -> LARROW (lexing_position_start buf)
    | "::" -> PATHSEP (lexing_position_start buf)
    | "..." -> DOTDOTDOT (lexing_position_start buf)
    | "..=" -> DOTDOTEQ (lexing_position_start buf)
    | ".." -> DOTDOT (lexing_position_start buf)
    | "+" -> PLUS (lexing_position_start buf)
    | "-" -> MINUS (lexing_position_start buf)
    | "*" -> STAR (lexing_position_start buf)
    | "/" -> SLASH (lexing_position_start buf)
    | "%" -> PERCENT (lexing_position_start buf)
    | "^" -> CARET (lexing_position_start buf)
    | "!" -> NOT (lexing_position_start buf)
    | "&" -> AND (lexing_position_start buf)
    | "|" -> OR (lexing_position_start buf)
    | "=" -> EQ (lexing_position_start buf)
    | "<" -> LT (lexing_position_start buf)
    | ">" -> GT (lexing_position_start buf)
    | "@" -> AT (lexing_position_start buf)
    | "_" -> UNDERSCORE (lexing_position_start buf)
    | "." -> DOT (lexing_position_start buf)
    | "," -> COMMA (lexing_position_start buf)
    | ";" -> SEMI (lexing_position_start buf)
    | ":" -> COLON (lexing_position_start buf)
    | "#" -> POUND (lexing_position_start buf)
    | "$" -> DOLLAR (lexing_position_start buf)
    | "?" -> QUESTION (lexing_position_start buf)
    | "~" -> TILDE (lexing_position_start buf)
    | "{" -> LBRACE (lexing_position_start buf)
    | "}" -> RBRACE (lexing_position_start buf)
    | "[" -> LBRACK (lexing_position_start buf)
    | "]" -> RBRACK (lexing_position_start buf)
    | "(" -> LPAREN (lexing_position_start buf)
    | ")" -> RPAREN (lexing_position_start buf)
    | '^' -> XOR (lexing_position_start buf)
    | "^=" -> XOREQ (lexing_position_start buf)
    | "'static" -> STATIC_LIFETIME (lexing_position_start buf)
    | "'" -> read_char (Buffer.create 17) buf
    | "\"" -> read_string (Buffer.create 17) buf 
    | "r#\""-> read_raw_string (Buffer.create 17) buf
    | "b'" -> read_byte (Buffer.create 17) buf
    | "b\"" -> read_byte_string (Buffer.create 17) buf
    | "c\"" -> read_c_string (Buffer.create 17) buf
    | "br#\"" -> read_raw_byte_string (Buffer.create 17) buf
    | "cr#\"" -> read_raw_c_string (Buffer.create 17) buf
    | reserved_number -> failwith "issue with num literal"
    | integer_literal -> 
        let uArr = Sedlexing.lexeme buf in
          let x = uchar_array_to_string uArr in
    INT_LIT (x, lexing_position_start buf)
    | float_literal ->
        let uArr = Sedlexing.lexeme buf in
          let x = uchar_array_to_string uArr in
    FLOAT_LIT (x, lexing_position_start buf)
    | eof -> EOF 
    | _ ->
      let chr = Sedlexing.Utf8.lexeme buf in
        let x = (lexing_position_start buf).pos_lnum in
          let y = (lexing_position_start buf).pos_cnum - (lexing_position_start buf).pos_bol  in
            raise (Lexing_error (Printf.sprintf "Unexpected character: %S at line %d and %d" chr x y))

and read_raw_c_string buffer buf =
  match%sedlex buf with
    | "\"#"   -> RAW_C_STRING (string_to_char_code_list (Buffer.contents buffer), lexing_position_start buf)
    | Plus (Compl (Chars "\"\\\n\r\t")) -> 
      Buffer.add_string buffer (Utf8.lexeme buf);
      read_raw_c_string buffer buf

    (* Handle end of string or malformed string *)
    | eof -> failwith "raw string is not terminated"
    | _ -> failwith "illegal raw string char"

and read_raw_string buffer buf =
  match%sedlex buf with
    | "\"#"   -> RAW_STRING_LIT (string_to_char_code_list (Buffer.contents buffer), lexing_position_start buf)
    | Plus (Compl (Chars "\"\\\n\r\t")) -> 
      Buffer.add_string buffer (Utf8.lexeme buf);
      read_raw_string buffer buf

    (* Handle end of string or malformed string *)
    | eof -> failwith "raw string is not terminated"
    | _ -> failwith "illegal raw string char"


and read_raw_byte_string buffer buf =
  match%sedlex buf with
    | "\"#"   -> RAW_BYTE_STRING (string_to_char_code_list (Buffer.contents buffer), lexing_position_start buf)
    | Plus (Compl (Chars "\"\\\n\r\t")) -> 
      Buffer.add_string buffer (Utf8.lexeme buf);
      read_raw_byte_string buffer buf

    (* Handle end of string or malformed string *)
    | eof -> failwith "raw string is not terminated"
    | _ -> failwith "illegal raw string char"

and read_string buffer buf =
  match%sedlex buf with
  | "\""   -> STRING_LIT (string_to_char_code_list (Buffer.contents buffer), lexing_position_start buf)
  | quote_escape ->
      let lex = Utf8.lexeme buf in
      (* Here, we check the matched escape and handle accordingly *)
      if lex = "\\\"" then
        Buffer.add_char buffer '\"'
      else if lex = "\\\'" then
        Buffer.add_char buffer '\''
      else
        failwith "Unexpected quote escape sequence";
      read_string buffer buf

  (* Handle ASCII hexadecimal escape \xNN *)
  | ascii_escape ->
      let lex = Utf8.lexeme buf in
      (* Process the matched escape sequence and convert to the correct character *)
      (match lex with
       | "\\n" -> Buffer.add_char buffer '\n'
       | "\\r" -> Buffer.add_char buffer '\r'
       | "\\t" -> Buffer.add_char buffer '\t'
       | "\\\\" -> Buffer.add_char buffer '\\'
       | "\\0" -> Buffer.add_char buffer '\000'
       | _ ->
           (* Handle hexadecimal escape \xNN *)
           let hex_code = Stdlib.String.sub lex 2 2 in
           let code = int_of_string ("0x" ^ hex_code) in
           Stdlib.Buffer.add_char buffer (Stdlib.Char.chr code));
      read_string buffer buf

  (* Handle Unicode escape \u{NNNN} *)
  | unicode_escape ->
      let lex = Utf8.lexeme buf in
      let inner = Stdlib.String.sub lex 3 (Stdlib.String.length lex - 4) in
      let code = int_of_string ("0x" ^ inner) in
      Buffer.add_utf_8_uchar buffer (Uchar.of_int code);
      read_string buffer buf

  (* Handle any other non-special characters *)
  | Plus (Compl (Chars "\"\\\n\r\t")) -> 
      Buffer.add_string buffer (Utf8.lexeme buf);
      read_string buffer buf

  (* Handle end of string or malformed string *)
  | eof -> failwith "String is not terminated"
  | _ -> failwith "illegal strin char"

and read_char buffer buf =
  match%sedlex buf with
  (* Handle quote escapes *)
  | quote_escape, "'" ->
      let l = Utf8.lexeme buf in
      let lex = Stdlib.String.sub l 0 (Stdlib.String.length l - 1) in (* Remove the trailing quote mark *)

      (* Check the matched escape and handle accordingly *)
      if lex = "\\\"" then begin
        Buffer.add_char buffer '\"';
        CHAR_LIT (Int64.of_int (Char.code '\"'), lexing_position_start buf)
      end else if lex = "\\\'" then begin
        Buffer.add_char buffer '\'';
        CHAR_LIT (Int64.of_int (Char.code '\''), lexing_position_start buf)
      end else
        failwith "Unexpected quote escape sequence"

  (* Handle ASCII escape sequences like \n, \r, \t, etc. *)
  | ascii_escape, "'" ->
      let l = Utf8.lexeme buf in
      let lex = Stdlib.String.sub l 0 (Stdlib.String.length l - 1) in (* Remove the trailing quote mark *)
      (* Process the matched escape sequence and convert to the correct character *)
      (match lex with
       | "\\n" -> Buffer.add_char buffer '\n'; CHAR_LIT (Int64.of_int (Char.code '\n'), lexing_position_start buf)
       | "\\r" -> Buffer.add_char buffer '\r'; CHAR_LIT (Int64.of_int (Char.code '\r'), lexing_position_start buf)
       | "\\t" -> Buffer.add_char buffer '\t'; CHAR_LIT (Int64.of_int (Char.code '\t'), lexing_position_start buf)
       | "\\\\" -> Buffer.add_char buffer '\\'; CHAR_LIT (Int64.of_int (Char.code '\\'), lexing_position_start buf)
       | "\\0" -> Buffer.add_char buffer '\000'; CHAR_LIT (Int64.of_int (Char.code '\000'), lexing_position_start buf)
      | _ -> 
           (* Handle hexadecimal escape \xNN *)
           let hex_code = Stdlib.String.sub lex 2 2 in
           let code = int_of_string ("0x" ^ hex_code) in
           Buffer.add_char buffer (Char.chr code);
           CHAR_LIT (Int64.of_int code, lexing_position_start buf))

  (* Handle Unicode escape \u{NNNN} *)
  | unicode_escape, "'" ->
      let l = Utf8.lexeme buf in
      let lex = Stdlib.String.sub l 0 (Stdlib.String.length l - 1) in (* Remove the trailing quote mark *)
      let inner = Stdlib.String.sub lex 3 (Stdlib.String.length lex - 4) in
      let code = int_of_string ("0x" ^ inner) in
      Buffer.add_utf_8_uchar buffer (Uchar.of_int code);
      CHAR_LIT (Int64.of_int code, lexing_position_start buf)

  (* Handle normal, printable characters inside char literal *)
  | Compl (Chars "'\\\n\r\t"), "'" ->
      let l = Utf8.lexeme buf in
      let lex = Stdlib.String.sub l 0 (Stdlib.String.length l - 1) in (* Remove the trailing quote mark *)
      Buffer.add_char buffer (Stdlib.String.get lex 0);
      CHAR_LIT (Int64.of_int (Char.code (Stdlib.String.get lex 0)), lexing_position_start buf)

  (* Handle end-of-file or malformed char *)
  | eof -> failwith "Character literal is not terminated"
  
  (* Handle illegal character literal *)
  | _ -> failwith "Illegal character literal"

and read_byte buffer buf =
  match%sedlex buf with
  | byte_escape, "'" ->
    let l = Utf8.lexeme buf in
    let lex = Stdlib.String.sub l 0 (Stdlib.String.length l - 1) in (* Remove the trailing quote mark *)
    (match lex with
     | "\\n" -> Buffer.add_char buffer '\n'; BYTE (Int64.of_int (Char.code '\n'), lexing_position_start buf)
     | "\\r" -> Buffer.add_char buffer '\r'; BYTE (Int64.of_int (Char.code '\r'), lexing_position_start buf)
     | "\\t" -> Buffer.add_char buffer '\t'; BYTE (Int64.of_int (Char.code '\t'), lexing_position_start buf)
     | "\\\\" -> Buffer.add_char buffer '\\'; BYTE (Int64.of_int (Char.code '\\'), lexing_position_start buf)
     | "\\0" -> Buffer.add_char buffer '\000'; BYTE (Int64.of_int (Char.code '\000'), lexing_position_start buf)
     | _ -> 
         let hex_code = Stdlib.String.sub lex 2 2 in
         let code = int_of_string ("0x" ^ hex_code) in
         if code < 0 || code > 127 then
           failwith "Byte escape out of ASCII range";
         Buffer.add_char buffer (Char.chr code);
         BYTE (Int64.of_int code, lexing_position_start buf))
      
    | quote_escape, "'" ->
      let l = Utf8.lexeme buf in
      let lex = Stdlib.String.sub l 0 (Stdlib.String.length l - 1) in (* Remove the trailing quote mark *)

      (* Check the matched escape and handle accordingly *)
      if lex = "\\\"" then begin
        Buffer.add_char buffer '\"';
        CHAR_LIT (Int64.of_int (Char.code '\"'), lexing_position_start buf)
      end else if lex = "\\\'" then begin
        Buffer.add_char buffer '\'';
        CHAR_LIT (Int64.of_int (Char.code '\''), lexing_position_start buf)
      end else
        failwith "Unexpected quote escape sequence"
  (* Handle normal, printable characters inside char literal *)
  | Compl (Chars "'\\\n\r\t"), "'" ->
      let l = Utf8.lexeme buf in
      let lex = Stdlib.String.sub l 0 (Stdlib.String.length l - 1) in (* Remove the trailing quote mark *)
      Buffer.add_char buffer (Stdlib.String.get lex 0);
      BYTE (Int64.of_int (Char.code (Stdlib.String.get lex 0)), lexing_position_start buf)

  (* Handle end-of-file or malformed char *)
  | eof -> failwith "Character literal is not terminated"
  
  (* Handle illegal character literal *)
  | _ -> failwith "Illegal character literal"

and read_byte_string buffer buf =
  match%sedlex buf with
  | "\"" -> BYTE_STRING (string_to_char_code_list (Buffer.contents buffer), lexing_position_start buf)
  | byte_escape ->
      let lex = Utf8.lexeme buf in
      (* Process the matched escape sequence and convert to the correct character *)
      (match lex with
       | "\\n" -> Buffer.add_char buffer '\n'
       | "\\r" -> Buffer.add_char buffer '\r'
       | "\\t" -> Buffer.add_char buffer '\t'
       | "\\\\" -> Buffer.add_char buffer '\\'
       | "\\0" -> Buffer.add_char buffer '\000'
       | _ ->
           (* Handle hexadecimal escape \xNN *)
           let hex_code = Stdlib.String.sub lex 2 2 in
           let code = int_of_string ("0x" ^ hex_code) in
           Buffer.add_char buffer (Char.chr code));
      read_byte_string buffer buf
  | quote_escape ->
      let lex = Utf8.lexeme buf in
      (* Here, we check the matched escape and handle accordingly *)
      if lex = "\\\"" then
        Buffer.add_char buffer '\"'
      else if lex = "\\\'" then
        Buffer.add_char buffer '\''
      else
        failwith "Unexpected quote escape sequence";
      read_byte_string buffer buf
  | Plus (Compl (Chars "\"\\\n\r\t")) ->
      let s = Utf8.lexeme buf in
      if Stdlib.String.for_all (fun c -> Char.code c <= 127) s then
        Buffer.add_string buffer s
      else
        failwith "Non-ASCII character in byte string";
      read_byte_string buffer buf

  | eof -> failwith "String is not terminated"
  | _ -> failwith "Illegal string character";

and read_c_string buffer buf =
  match%sedlex buf with
  | "\"" -> C_STRING (string_to_char_code_list (Buffer.contents buffer), lexing_position_start buf)
  | byte_escape ->
      let lex = Utf8.lexeme buf in
      (* Process the matched escape sequence and convert to the correct character *)
      (match lex with
       | "\\n" -> Buffer.add_char buffer '\n'
       | "\\r" -> Buffer.add_char buffer '\r'
       | "\\t" -> Buffer.add_char buffer '\t'
       | "\\\\" -> Buffer.add_char buffer '\\'
       | "\\0" -> Buffer.add_char buffer '\000'
       | _ ->
           (* Handle hexadecimal escape \xNN *)
           let hex_code = Stdlib.String.sub lex 2 2 in
           let code = int_of_string ("0x" ^ hex_code) in
           Buffer.add_char buffer (Char.chr code));
      read_c_string buffer buf

  | quote_escape ->
      let lex = Utf8.lexeme buf in
      (* Here, we check the matched escape and handle accordingly *)
      if lex = "\\\"" then
        Buffer.add_char buffer '\"'
      else if lex = "\\\'" then
        Buffer.add_char buffer '\''
      else
        failwith "Unexpected quote escape sequence";
      read_c_string buffer buf
  | unicode_escape ->
      let lex = Utf8.lexeme buf in
      let inner = Stdlib.String.sub lex 3 (Stdlib.String.length lex - 4) in
      let code = int_of_string ("0x" ^ inner) in
      Buffer.add_utf_8_uchar buffer (Uchar.of_int code);
      read_c_string buffer buf

  | Plus (Compl (Chars "\"\\\n\r\t")) ->
      let s = Utf8.lexeme buf in
      if Stdlib.String.for_all (fun c -> Char.code c <= 127) s then
        Buffer.add_string buffer s
      else
        failwith "Non-ASCII character in byte string";
      read_c_string buffer buf

  | eof -> failwith "String is not terminated"
| _ -> failwith "Illegal c string character"
