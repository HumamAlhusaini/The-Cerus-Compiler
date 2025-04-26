From Stdlib Require Import String.
From Stdlib Require Import Ascii.

(*temporary dummy float*)
Definition float := nat.

(* Location info *)
Definition loc := unit. 
(* NOTE: We don't have Lexing.position in Coq, so for now we use unit. 
   You can later define a better position type if needed. *)

(* ID interface is implicit: in Coq we don't have modules in the same way.
   We'll just define the IDs as strings. *)

Definition var_name := string.
Definition trait_name := string.
Definition impl_name := string.
Definition func_name := string.
Definition enum_name := string.
Definition struct_name := string.

(* Binary operations *)
Inductive bin_op : Type :=
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv.

(* Types *)

(* Forward mutual recursion: we declare them together *)
Inductive typ : Type :=
  | Typ : simpl_typ -> typ
  | TArrow : simpl_typ -> simpl_typ -> typ (* function type: T1 -> T2 *)

with simpl_typ : Type :=
  | TLit : typ_lit -> simpl_typ
  | TCustom : string -> simpl_typ
  | TRef : bool -> simpl_typ -> simpl_typ
  | TParen : list typ -> simpl_typ

with typ_lit : Type :=
  | TInt32
  | TUInt32
  | TFloat32
  | TFloat64
  | TChar
  | TBool. (* Was `Bool` in OCaml, but Coq reserves `bool`, so use TBool *)

(* Expressions *)

Inductive expr : Type :=
  | IntLit : nat -> expr
  | FloatLit : float -> expr
  | CharLit : ascii -> expr
  | StringLit : string -> expr
  | LTrue : expr
  | LFalse : expr
  | Ident : string -> expr
  | Ref : bool -> expr -> expr.

(* Statements *)

Inductive stmt : Type :=
  | Let : bool -> var_name -> typ -> option expr -> stmt
  | Print : string -> stmt.

(* Block elements: items or statements *)

Inductive block_element : Type :=
  | Item_block : item -> block_element
  | Stmt_block : stmt -> block_element

(* Top-level items *)

with item : Type :=
  | Func : func_name -> list (string * typ) -> typ -> list block_element -> item
  | Struct : struct_name -> list (string * typ) -> item
  | Enum : enum_name -> list (string * list typ) -> item
  | Const : var_name -> typ -> expr -> item
  | Static : bool -> var_name -> typ -> option expr -> item
  | Impl : impl_name -> list item -> item.

(* Whole program is just a list of items *)
Definition program : Type := list item.
