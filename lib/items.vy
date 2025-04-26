
%{
  open Ast

let is_valid_impl_item item =
  match item with
  | Func _ -> true  (* Only functions are valid *)
  | _ -> false      (* Reject structs, enums, etc. *)
%}

%%

params:
  separated_list(COMMA, var_and_typ) {  $1 }

block_element:
    | s = stmt { Stmt_block s }
    | i = item { Item_block i }

enum:
  | id = IDENT; LPAREN; typs = separated_list(COMMA, typ); RPAREN { (id, typs) }
  | id = IDENT                                        { (id, []) }

%public
item:
  | FN; name = IDENT; LPAREN; parameters = params; RPAREN; ARROW; t = typ;
    LBRACE; body = list(block_element); RBRACE {
      let n = Func_name.of_string name in
      let params_opt = match parameters with
        | [] -> None
        | lst -> Some lst
      in
      Func ($startpos, n, params_opt, t, body)
  }
  | STRUCT; name = IDENT; LBRACE; p = params; RBRACE { 
      let n = Struct_name.of_string name in
      Struct ($startpos, n, p) 
    }
  | ENUM; name = IDENT; LBRACE; variants = separated_list(COMMA, enum); RBRACE {
      let n = Enum_name.of_string name in
      Enum ($startpos, n, variants)
  } 
  | CONST; name = IDENT; COLON; t = typ; EQ; value = expr; SEMI {
      let n = Var_name.of_string name in
        Const ($startpos, n, t, value)
}
  | STATIC; mut = opt_mut; name = IDENT; COLON; t = typ; value = has_expr {
      let n = Var_name.of_string name in
      Static ($startpos, mut, n, t, value)
  }
    | IMPL; name = IDENT; LBRACE; body = list(item); RBRACE; {
      if List.for_all is_valid_impl_item body then
        Impl ($startpos, Impl_name.of_string name, body)
      else
        failwith "Invalid item found inside impl block"
  }


(*
function_type ::= function_signature
function_signature ::= argument_type "->" return_type
argument_type ::= "(" argument_list? ")"
argument_list ::= type | argument_list "," type
return_type ::= function_signature | unit_type
unit_type ::= "()"
*)

