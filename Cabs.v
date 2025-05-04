From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Parameter loc : Type.

Parameter str : Type.

Parameter char_code : Type.

Inductive top_level :=
  | IDENTIFIER : identifier -> top_level
  | CONSTANT : constant -> top_level

with constant :=
  | INT_LIT : str -> constant
  | FLOAT_LIT : str -> constant

with identifier :=
  | Raw_Ident : str -> identifier
  | Ident : str -> identifier.



Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".

