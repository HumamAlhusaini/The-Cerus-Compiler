%{
From Coq Require Extraction.
Extraction Language OCaml.
Require Import Ascii.
Require Import String.
Require Import List.
Require Import Main.Ast.
%}

%token<Ast.loc> ADD SUB EQ MUL DIV
%token<Ast.constant * Ast.loc> CONSTANT
%token EOF

%type<(Ast.expression * Ast.loc)> primary_expression
%type<(Ast.expression * Ast.loc)> postfix_expression
%type<(Ast.expression * Ast.loc)> unary_expression
%type<(Ast.expression * Ast.loc)> cast_expression
%type<(Ast.expression * Ast.loc)> multiplicative_expression
%type<(Ast.expression * Ast.loc)> additive_expression
%type<(Ast.expression * Ast.loc)> shift_expression
%type<list (Ast.expression * Ast.loc)> nonempty_translation_unit

%start <list (Ast.expression * Ast.loc)> translation_unit

%%

primary_expression:
| cst = CONSTANT
    { (Ast.CONSTANT (fst cst), snd cst) }

postfix_expression:
| expr = primary_expression
    { expr }

unary_expression:
| expr = postfix_expression
    { expr }

cast_expression:
| expr = unary_expression
    { expr }

multiplicative_expression:
| expr = cast_expression
    { expr }
| expr1 = multiplicative_expression MUL expr2 = cast_expression
    { (Ast.BINARY Ast.MUL (fst expr1) (fst expr2), snd expr1) }
| expr1 = multiplicative_expression DIV expr2 = cast_expression
    { (Ast.BINARY Ast.DIV (fst expr1) (fst expr2), snd expr1) }

additive_expression:
| expr = multiplicative_expression
    { expr }
| expr1 = additive_expression ADD expr2 = multiplicative_expression
    { (Ast.BINARY Ast.ADD (fst expr1) (fst expr2), snd expr1) }
| expr1 = additive_expression SUB expr2 = multiplicative_expression
    { (Ast.BINARY Ast.SUB (fst expr1) (fst expr2), snd expr1) }

shift_expression:
| expr = additive_expression { expr }

nonempty_translation_unit:
  | e = shift_expression; rest = nonempty_translation_unit { e :: rest }
  | e = shift_expression { [e] }

translation_unit:
  | e = nonempty_translation_unit EOF { e }
  | EOF { [] }



