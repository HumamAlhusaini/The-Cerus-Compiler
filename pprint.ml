  open Parser
  open Cabs
  open Uchar

let string_of_loc loc =
  match loc with
  x,y -> Printf.sprintf "column: %d, offset: %n" x y

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

let string_of_token = function
  | Parser.IDENT (Cabs.Raw_Ident x, loc) -> Printf.sprintf "RAW_IDENT (%s), loc: %s" x (string_of_loc loc)
  | Parser.IDENT (Cabs.Ident x, loc) -> Printf.sprintf "IDENT (%s), loc: %s" x (string_of_loc loc)

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
  | Parser.CHAR_LIT (str, loc) ->
    let uchar = uchar_of_int64 str in
      Printf.sprintf "CHAR_LIT(%c), loc: %s" (Uchar.to_char uchar) (string_of_loc loc)
  | Parser.EOF loc -> "EOF"
