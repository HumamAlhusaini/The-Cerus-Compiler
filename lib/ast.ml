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
  | Declaration of loc * string * typ * expr
  | Print       of loc * string

(* Top-level or block items *)
and item =
  | Func     of loc * string * (string * typ) list option * block_element list
  | Struct   of loc * string * (string * typ) list
  | Enum     of loc * string * (string * typ list) list
  | Const    of loc * string * typ * expr
  | Static   of loc * string * typ * expr

and expr =
  | IntLit    of int
  | FloatLit  of float
  | CharLit   of char
  | StringLit of string
  | LTrue
  | LFalse
  | Ident     of string
  | Binary    of expr * binop * expr
  | Unary     of unop * expr

and block_element =
  | Item_block of item
  | Stmt_block of stmt

type program = item list
