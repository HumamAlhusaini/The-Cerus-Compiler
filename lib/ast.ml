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

type stmt =
  | Let of loc * string * typ * expr option
  | Print       of loc * string

(* Top-level or block items *)
and item =
  | Func     of loc * string * (string * typ) list option * block_element list
  | Struct   of loc * string * (string * typ) list
  | Enum     of loc * string * (string * typ list) list
  | Const    of loc * string * typ * expr option
  | Static   of loc * string * typ * expr option

and expr =
  | IntLit    of int
  | FloatLit  of float
  | CharLit   of char
  | StringLit of string
  | LTrue
  | LFalse
  | Ident     of string

and block_element =
  | Item_block of item
  | Stmt_block of stmt

type program = item list
