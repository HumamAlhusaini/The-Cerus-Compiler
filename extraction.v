Require Import Main.Parser.
Require Import Main.Ast.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extraction "Parser.ml" translation_unit.


Extraction "Ast.ml" constant expression binary_operator.
