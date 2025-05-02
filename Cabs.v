From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Parameter loc : Type.

Parameter str : Type.

Inductive top_level :=
  | IDENTIFIER : identifier -> top_level

with identifier :=
  | Raw_Ident : str -> identifier
  | Ident : str -> identifier.



Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "int * int".
Extract Constant str => "string".

