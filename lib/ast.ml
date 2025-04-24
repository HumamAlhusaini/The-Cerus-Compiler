type loc = Lexing.position

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv

type typ =
  | TLit of typ_lit
  | TCustom of string

and typ_lit =
  | TInt32
  | TUInt32
  | TFloat32
  | TFloat64
  | TChar
  | Bool

type literal = 
  | LInt of int
  | LFloat of float
  | LChar of char
  | LTrue
  | LFalse

type stmt =
| Declaration of loc * string * typ * literal
| Print       of loc * string

type item =
  | Func        of loc * string * (string * typ) list option * stmt list

type program = item list


