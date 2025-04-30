
%{
  open Ast
%}

%token<Ast.loc> ADD SUB EQ MUL DIV
%token<Ast.constant * Ast.loc> CONSTANT
%token EOF


%type<unit> primary_expression
%type<unit> postfix_expression
%type<unit> unary_expression
%type<unit> cast_expression
%type<unit> additive_operator
%type<unit> multiplicative_operator
%type<unit> multiplicative_expression
%type<unit> additive_expression
%type<unit> shift_expression
%type<unit> nonempty_translation_unit

%start <unit> translation_unit

%%

primary_expression:
| CONSTANT
    {}

postfix_expression:
| primary_expression 
  {}
unary_expression:
| postfix_expression
{}
cast_expression:
| unary_expression
{}
multiplicative_operator:
  MUL | DIV 
  {}

multiplicative_expression:
| cast_expression 
| multiplicative_expression multiplicative_operator cast_expression
    {}

additive_operator:
  ADD | SUB {}

additive_expression:
| multiplicative_expression
| additive_expression additive_operator multiplicative_expression
    {}

shift_expression:
| additive_expression
    {}

nonempty_translation_unit:
  | shift_expression nonempty_translation_unit 
  | shift_expression {  }

translation_unit:
  | nonempty_translation_unit EOF 
  | EOF {  }


