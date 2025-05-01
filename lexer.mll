{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let explode s = List.init (String.length s) (String.get s)

let get_column pos =
  pos.Lexing.pos_cnum - pos.Lexing.pos_bol
}

(* Identifiers *)
let digit = ['0'-'9']
let hexadecimal_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let nondigit = ['_' 'a'-'z' 'A'-'Z']

let hex_quad = hexadecimal_digit hexadecimal_digit
                 hexadecimal_digit hexadecimal_digit
let universal_character_name =
    "\\u" (hex_quad as n)
  | "\\U" (hex_quad hex_quad as n)

let identifier_nondigit =
    nondigit
(*| universal_character_name*)
  | '$'

let identifier = identifier_nondigit (identifier_nondigit|digit)*

(* Whitespaces *)
let whitespace_char_no_newline = [' ' '\t'  '\011' '\012' '\r']

(* Integer constants *)
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*

let octal_digit = ['0'-'7']
let octal_constant = '0' octal_digit*

let hexadecimal_prefix = "0x" | "0X"
let hexadecimal_constant =
  hexadecimal_prefix hexadecimal_digit+

let unsigned_suffix = ['u' 'U']
let long_suffix = ['l' 'L']
let long_long_suffix = "ll" | "LL"
let integer_suffix =
    unsigned_suffix long_suffix?
  | unsigned_suffix long_long_suffix
  | long_suffix unsigned_suffix?
  | long_long_suffix unsigned_suffix?

let integer_constant =
    decimal_constant integer_suffix?
  | octal_constant integer_suffix?
  | hexadecimal_constant integer_suffix?

(* Floating constants *)
let sign = ['-' '+']
let digit_sequence = digit+
let floating_suffix = ['f' 'l' 'F' 'L'] as suffix

let fractional_constant =
    (digit_sequence as intpart)? '.' (digit_sequence as frac)
  | (digit_sequence as intpart) '.'
let exponent_part =
    'e' ((sign? digit_sequence) as expo)
  | 'E' ((sign? digit_sequence) as expo)
let decimal_floating_constant =
    fractional_constant exponent_part? floating_suffix?
  | (digit_sequence as intpart) exponent_part floating_suffix?

let hexadecimal_digit_sequence = hexadecimal_digit+
let hexadecimal_fractional_constant =
    (hexadecimal_digit_sequence as intpart)? '.' (hexadecimal_digit_sequence as frac)
  | (hexadecimal_digit_sequence as intpart) '.'
let binary_exponent_part =
    'p' ((sign? digit_sequence) as expo)
  | 'P' ((sign? digit_sequence) as expo)
let hexadecimal_floating_constant =
    hexadecimal_prefix hexadecimal_fractional_constant
        binary_exponent_part floating_suffix?
  | hexadecimal_prefix (hexadecimal_digit_sequence as intpart)
        binary_exponent_part floating_suffix?

(* Preprocessing numbers *)
let preprocessing_number =
  '.'? ['0'-'9']
  (['0'-'9' 'A'-'Z' 'a'-'z' '_' '.'] | ['e' 'E' 'p' 'P']['+' '-'])*

(* Character and string constants *)
let simple_escape_sequence =
  '\\' ( ['\''  '\"'  '?'  '\\'  'a'  'b'  'e'  'f'  'n'  'r'  't'  'v'] as c)
let octal_escape_sequence =
  '\\' ((octal_digit
         | octal_digit octal_digit
         | octal_digit octal_digit octal_digit) as n)
let hexadecimal_escape_sequence = "\\x" (hexadecimal_digit+ as n)

rule initial = parse
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline +  { initial lexbuf }
  | "/*"                          { multiline_comment lexbuf; initial lexbuf }
  | "//"                          { singleline_comment lexbuf; initial lexbuf }
  | integer_constant as s         { CONSTANT (Cabs.CONST_INT s, currentLoc lexbuf) }

  | decimal_floating_constant     { CONSTANT (Cabs.CONST_FLOAT
                                      {Cabs.isHex_FI = false;
                                       Cabs.integer_FI = intpart;
                                       Cabs.fraction_FI = frac;
                                       Cabs.exponent_FI = expo;
                                       Cabs.suffix_FI =
                                         match suffix with
                                         | None -> None
                                         | Some c -> Some (String.make 1 c) },
                                      currentLoc lexbuf) }
  | hexadecimal_floating_constant { CONSTANT (Cabs.CONST_FLOAT
                                      {Cabs.isHex_FI = true;
                                       Cabs.integer_FI = intpart;
                                       Cabs.fraction_FI = frac;
                                       Cabs.exponent_FI = Some expo;
                                       Cabs.suffix_FI =
                                         match suffix with
                                           | None -> None
                                           | Some c -> Some (String.make 1 c) },
                                      currentLoc lexbuf)}
  | preprocessing_number as s     { error lexbuf "invalid numerical constant '%s'@ These characters form a preprocessor number, but not a constant" s;
                                    CONSTANT (Cabs.CONST_INT "0", currentLoc lexbuf) }
  | (""|"L"|"u"|"U") as e "'"     { let enc = encoding_of e in
                                    let l = char_literal lexbuf.lex_start_p [] lexbuf in
                                    CONSTANT (Cabs.CONST_CHAR(enc, l), currentLoc lexbuf) }
  | (""|"L"|"u"|"U"|"u8") as e "\""
                                  { let enc = encoding_of e in
                                    let l = string_literal lexbuf.lex_start_p enc [] lexbuf in
                                    STRING_LITERAL(enc, l, currentLoc lexbuf) }
  | "..."                         { ELLIPSIS(currentLoc lexbuf) }
  | "+="                          { ADD_ASSIGN(currentLoc lexbuf) }
  | "-="                          { SUB_ASSIGN(currentLoc lexbuf) }
  | "*="                          { MUL_ASSIGN(currentLoc lexbuf) }
  | "/="                          { DIV_ASSIGN(currentLoc lexbuf) }
  | "%="                          { MOD_ASSIGN(currentLoc lexbuf) }
  | "|="                          { OR_ASSIGN(currentLoc lexbuf) }
  | "&="                          { AND_ASSIGN(currentLoc lexbuf) }
  | "^="                          { XOR_ASSIGN(currentLoc lexbuf) }
  | "<<="                         { LEFT_ASSIGN(currentLoc lexbuf) }
  | ">>="                         { RIGHT_ASSIGN(currentLoc lexbuf) }
  | "<<"                          { LEFT(currentLoc lexbuf) }
  | ">>"                          { RIGHT(currentLoc lexbuf) }
  | "=="                          { EQEQ(currentLoc lexbuf) }
  | "!="                          { NEQ(currentLoc lexbuf) }
  | "<="                          { LEQ(currentLoc lexbuf) }
  | ">="                          { GEQ(currentLoc lexbuf) }
  | "="                           { EQ(currentLoc lexbuf) }
  | "<"                           { LT(currentLoc lexbuf) }
  | ">"                           { GT(currentLoc lexbuf) }
  | "++"                          { INC(currentLoc lexbuf) }
  | "--"                          { DEC(currentLoc lexbuf) }
  | "->"                          { PTR(currentLoc lexbuf) }
  | "+"                           { PLUS(currentLoc lexbuf) }
  | "-"                           { MINUS(currentLoc lexbuf) }
  | "*"                           { STAR(currentLoc lexbuf) }
  | "/"                           { SLASH(currentLoc lexbuf) }
  | "%"                           { PERCENT(currentLoc lexbuf) }
  | "!"                           { BANG(currentLoc lexbuf) }
  | "&&"                          { ANDAND(currentLoc lexbuf) }
  | "||"                          { BARBAR(currentLoc lexbuf) }
  | "&"                           { AND(currentLoc lexbuf) }
  | "|"                           { BAR(currentLoc lexbuf) }
  | "^"                           { HAT(currentLoc lexbuf) }
  | "?"                           { QUESTION(currentLoc lexbuf) }
  | ":"                           { COLON(currentLoc lexbuf) }
  | "~"                           { TILDE(currentLoc lexbuf) }
  | "{"|"<%"                      { LBRACE(currentLoc lexbuf) }
  | "}"|"%>"                      { RBRACE(currentLoc lexbuf) }
  | "["|"<:"                      { LBRACK(currentLoc lexbuf) }
  | "]"|":>"                      { RBRACK(currentLoc lexbuf) }
  | "("                           { LPAREN(currentLoc lexbuf) }
  | ")"                           { RPAREN(currentLoc lexbuf) }
  | ";"                           { SEMICOLON(currentLoc lexbuf) }
  | ","                           { COMMA(currentLoc lexbuf) }
  | "."                           { DOT(currentLoc lexbuf) }
  | identifier as id              { if SSet.mem id !ignored_keywords
                                    then initial lexbuf
                                    else ident_or_keyword lexbuf id }
  | eof                           { EOF }
  | _ as c                        { fatal_error lexbuf "invalid symbol %C" c }

and initial_linebegin = parse
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline    { initial_linebegin lexbuf }
  | '#'                           { hash lexbuf }
  | ""                            { initial lexbuf }

and char = parse
  | universal_character_name
      { try
          check_universal_character lexbuf (int_of_string ("0x" ^ n))
        with Failure _ ->
          error lexbuf "overflow in universal character name";
          Chr 0
      }
  | hexadecimal_escape_sequence
      { try
          Esc (Int64.of_string ("0x" ^ n))
        with Failure _ ->
          error lexbuf "overflow in hexadecimal escape sequence";
          Esc 0L
      }
  | octal_escape_sequence
      { Esc (Int64.of_string  ("0o" ^ n)) }
  | simple_escape_sequence
      { Esc (convert_escape c) }
  | "\\u" | "\\U"
      { error lexbuf "incomplete universal character name";
        Chr 0 }
  | '\\' (_ as c)
      { error lexbuf "incorrect escape sequence '\\%c'" c;
        Esc (Int64.of_int (Char.code c)) }
  | ['\x00'-'\x7F'] as c1
      { Chr (Char.code c1) }
  | (['\xC0'-'\xDF'] as c1) (['\x80'-'\xBF'] as c2)
      { check_utf8 lexbuf 0x80
          ( (Char.code c1 land 0b00011111) lsl 6
          + (Char.code c2 land 0b00111111)) }
  | (['\xE0'-'\xEF'] as c1) (['\x80'-'\xBF'] as c2) (['\x80'-'\xBF'] as c3)
      { check_utf8 lexbuf 0x800
          ( (Char.code c1 land 0b00001111) lsl 12
          + (Char.code c2 land 0b00111111) lsl 6
          + (Char.code c3 land 0b00111111) ) }
  | (['\xF0'-'\xF7'] as c1) (['\x80'-'\xBF'] as c2) (['\x80'-'\xBF'] as c3) (['\x80'-'\xBF'] as c4)
     { check_utf8 lexbuf 0x10000
          ( (Char.code c1 land 0b00000111) lsl 18
          + (Char.code c2 land 0b00111111) lsl 12
          + (Char.code c3 land 0b00111111) lsl 6
          + (Char.code c4 land 0b00111111) ) }
  | _ as c
     { warning lexbuf Diagnostics.Invalid_UTF8
               "Invalid UTF8 encoding: byte 0x%02x" (Char.code c);
       Esc (Int64.of_int (Char.code c)) (* re-encode as-is *)
     }

and char_literal startp accu = parse
  | '\''       { lexbuf.lex_start_p <- startp;
                 List.rev accu }
  | '\n' | eof { fatal_error lexbuf "missing terminating \"'\" character" }
  | ""         { let c = char lexbuf in char_literal startp (add_char Cabs.EncU32 c accu) lexbuf }

and string_literal startp enc accu = parse
  | '\"'       { lexbuf.lex_start_p <- startp;
                 List.rev accu }
  | '\n' | eof { fatal_error lexbuf "missing terminating '\"' character" }
  | ""         { let c = char lexbuf in string_literal startp enc (add_char enc c accu) lexbuf }

(* We assume gcc -E syntax but try to tolerate variations. *)
and hash = parse
  | whitespace_char_no_newline +
    (digit + as n)
    whitespace_char_no_newline *
    "\"" ([^ '\n' '\"']* as file) "\""
    [^ '\n']* '\n'
      { let n =
          try
            int_of_string n
          with Failure _ ->
            warning lexbuf Diagnostics.Unnamed "invalid line number";
            lexbuf.lex_curr_p.pos_lnum
        in
        lexbuf.lex_curr_p <- {
          lexbuf.lex_curr_p with
            pos_fname = file;
            pos_lnum = n;
            pos_bol = lexbuf.lex_curr_p.pos_cnum
        };
        initial_linebegin lexbuf }
  | whitespace_char_no_newline *
    "pragma"
    whitespace_char_no_newline +
    ([^ '\n']* as s) '\n'
      { new_line lexbuf; PRAGMA (s, currentLoc lexbuf) }
  | [^ '\n']* '\n'
      { warning lexbuf Diagnostics.Unnamed "unrecognized '#' line";
        new_line lexbuf; initial_linebegin lexbuf }
  | [^ '\n']* eof
      { fatal_error lexbuf "unexpected end of file" }
  | _ as c
      { fatal_error lexbuf "invalid symbol %C" c }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"   { () }
  | eof    { error lexbuf "unterminated comment" }
  | '\n'   { new_line lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

