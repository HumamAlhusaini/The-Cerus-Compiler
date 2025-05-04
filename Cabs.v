From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Parameter loc : Type.

Parameter str : Type.

Parameter char_code : Type.

Inductive top_level :=
  | IDENTIFIER : identifier -> top_level

with identifier :=
  | Raw_Ident : str -> identifier
  | Ident : str -> identifier.



Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".

