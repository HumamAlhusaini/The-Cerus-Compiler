From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Parameter loc : Type.

Parameter str : Type.

Inductive expression :=
  | BINARY : binary_operator -> expression -> expression -> expression
  | CONSTANT : constant -> expression

with binary_operator :=
  | ADD | SUB | MUL | DIV | EQ

with constant := 
  | CONST_INT : str -> constant
  | DUMMY : loc -> constant.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.


Extract Constant loc => "Lexing.position".
Extract Constant str => "string".

Extraction "Ast.ml" constant expression binary_operator.
