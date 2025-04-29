%{
From Coq Require Extraction.
Extraction Language OCaml.
Require Import Ascii.
Require Import String.
Require Import List.
Require Import Main.Ast.
%}

%token ADD SUB EQ MUL DIV
%token<Ast.constant * Ast.loc> CONSTANT
%token EOF

%type <Ast.expression> expression
%type <list Ast.expression> translation_unit

%start<list Ast.expression> translation_unit_file

%%


expression:
  | CONSTANT { Ast.CONSTANT (fst $1) }
  | e1 = expression ADD e2 = expression { Ast.BINARY Ast.ADD e1 e2 }
  | e1 = expression SUB e2 = expression { Ast.BINARY Ast.SUB e1 e2 }
  | e1 = expression MUL e2 = expression { Ast.BINARY Ast.MUL e1 e2 }
  | e1 = expression DIV e2 = expression { Ast.BINARY Ast.DIV e1 e2 }
  | e1 = expression EQ  e2 = expression { Ast.BINARY Ast.EQ  e1 e2 }

translation_unit:
  | e = expression; rest = translation_unit { e :: rest }
  | /* empty */ { [] }

translation_unit_file:
  | e = translation_unit EOF { e }


