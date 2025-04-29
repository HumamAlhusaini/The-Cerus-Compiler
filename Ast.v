From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Parameter loc : Type.

Parameter string : Type.

Inductive expression :=
  | BINARY : binary_operator -> expression -> expression -> expression
  | CONSTANT : constant -> expression

with binary_operator :=
  | ADD | SUB | MUL | DIV | EQ

with constant := 
  | CONST_INT : string -> constant.


