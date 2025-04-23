type loc = Lexing.position

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv

type type_expr =
  | TInt              (* i32 *)
  | TUint             (* u32 *)
  | TBool             (* bool *)
  | TChar             (* char *)
  | TStr              (* &str *)
  | TString           (* String *)
  | TFloat            (* f32/f64 *)
  | TUnit             (* () *)
  | TRef   of type_expr           (* &T *)
  | TMutRef of type_expr          (* &mut T *)
  | TPointer of type_expr         (* *const T or *mut T *)
  | TArray of type_expr * int     (* [T; N] *)
  | TFunc of type_expr list * type_expr  (* fn(T1, T2) -> T3 *)
  
type expr =
  | Let         of loc * string * expr
  | Func        of loc * string * string list * expr

type program = expr list


