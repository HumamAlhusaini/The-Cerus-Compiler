From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Parameter loc : Type.

Parameter string : Type.

Parameter char_code : Type
.
Inductive encoding :=
  | EncNone                             (* no prefix *)
  | EncWide                             (* 'L' prefix *)
  | EncU16                              (* 'u' prefix *)
  | EncU32                              (* 'U' prefix *)
  | EncUTF8.                            (* 'u8' prefix (strings only) *)

Record floatInfo := {
  isHex_FI:bool;
  integer_FI:option string;
  fraction_FI:option string;
  exponent_FI:option string;
  suffix_FI:option string
}.

Inductive expression :=
  | BINARY : binary_operator -> expression -> expression -> expression
  | CONSTANT : constant -> expression

with binary_operator :=
  | ADD | SUB | MUL | DIV | EQ

with constant := 
  | CONST_INT : string -> constant
  | CONST_FLOAT : floatInfo -> constant
  | CONST_CHAR : encoding -> list char_code -> constant
  | CONST_STRING : encoding -> list char_code -> constant.
