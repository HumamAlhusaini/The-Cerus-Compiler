type loc = Lexing.position


module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module Var_name : ID = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let ( = ) = String.equal
end


module Trait_name : ID = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let ( = ) = String.equal
end

module Impl_name : ID = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let ( = ) = String.equal
end

module Func_name : ID = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let ( = ) = String.equal
end

module Enum_name : ID = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let ( = ) = String.equal
end

module Struct_name : ID = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let ( = ) = String.equal
end

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv

type typ =
  | Typ of simpl_typ
  | TArrow of simpl_typ * simpl_typ               (* function type: T1 -> T2 *)

and simpl_typ =
  | TLit of typ_lit
  | TCustom of string
  | TRef of bool * simpl_typ
  | TParen of typ list

and typ_lit =
  | TInt32
  | TUInt32
  | TFloat32
  | TFloat64
  | TChar
  | Bool

type stmt =
  | Let of loc * bool * Var_name.t * typ * expr option
  | Print       of loc * string

(* Top-level or block items *)
and item =
  | Func     of loc * Func_name.t * (string * typ) list option * typ * block_element list
  | Struct   of loc * Struct_name.t * (string * typ) list
  | Enum     of loc * Enum_name.t * (string * typ list) list
  | Const    of loc * Var_name.t * typ * expr 
  | Static   of loc * bool * Var_name.t * typ * expr option
  | Impl     of loc * Impl_name.t * item list

and expr =
  | IntLit    of int
  | FloatLit  of float
  | CharLit   of char
  | StringLit of string
  | LTrue
  | LFalse
  | Ident     of string
  | Ref of bool * expr

and block_element =
  | Item_block of item
  | Stmt_block of stmt

type program = item list

