%{
  open Cabs
    %}

%token <Cabs.loc> AS BREAK CONST CONTINUE CRATE
%token <Cabs.loc> ELSE ENUM EXTERN FALSE FN
%token <Cabs.loc> FOR IF IMPL IN LET RAW_MUT RAW_CONST
%token <Cabs.loc> LOOP MATCH MOD MOVE MUT AMPMUT
%token <Cabs.loc> PUB REF RETURN XOR XOREQ
%token <Cabs.loc> SELFVALUE SELFTYPE STATIC STRUCT SUPER
%token <Cabs.loc> TRAIT TRUE TYPE UNSAFE USE
%token <Cabs.loc> WHERE WHILE ASYNC AWAIT DYN 
%token <Cabs.loc> MACRO_RULES UNION STATIC_LIFETIME ELIDED_LIFETIME SAFE RAW

%token <Cabs.loc> PLUS MINUS STAR SLASH PERCENT    (* + - * / % *)
%token <Cabs.loc> CARET NOT AND OR ANDAND OROR      (* ^ ! & | && || *)
%token <Cabs.loc> SHL SHR                          (* << >> *)
%token <Cabs.loc> EQ PLUSEQ MINUSEQ STAREQ SLASHEQ  (* = += -= *= /= *)
%token <Cabs.loc> PERCENTEQ CARETEQ ANDEQ OREQ SHLEQ SHREQ (* %= ^= &= |= <<= >>= *)
%token <Cabs.loc> EQEQ NE LT GT LE GE              (* == != < > <= >= *)
%token <Cabs.loc> AT UNDERSCORE DOT DOTDOT DOTDOTDOT DOTDOTEQ (* @ _ . .. ... ..= *)
%token <Cabs.loc> COMMA SEMI COLON RESERVED_RAW_IDENTIFIER    (* , ; : *)
%token <Cabs.loc> PATHSEP RARROW FATARROW LARROW              (* :: -> => <- *)
%token <Cabs.loc> POUND DOLLAR QUESTION TILDE  DOLLAR_CRATE              (* # $ ? ~ *)
%token <Cabs.loc> LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN

%token <string * Cabs.loc> IDENT RAW_IDENT INT_LIT FLOAT_LIT
%token <(int64 list * Cabs.loc)> STRING_LIT RAW_STRING_LIT BYTE_STRING RAW_BYTE_STRING RAW_C_STRING C_STRING
%token <(int64 * Cabs.loc)> CHAR_LIT BYTE
%token <string * Cabs.loc> LIFETIME_OR_LABEL

%token EOF

%type <Cabs.item list> items
%type <Cabs.item> item
%type <Cabs.visItem> vis_item
%type <Cabs.extern_crate> extern_crate
%type <Cabs.crate_ref> crate_ref
%type <Cabs.as_clause option> as_clause
%type <Cabs.module_> unsafe_module safe_module
%type <(Cabs.identifier * Cabs.loc)> ident
%type <Cabs.outer_attribute list> outer_attrs
%type <Cabs.outer_attribute> outer_attr
%type <Cabs.inner_attribute list> inner_attrs
%type <Cabs.inner_attribute> inner_attr
%type <Cabs.attr> attr 
%type <Cabs.simple_path> simple_path
%type <Cabs.simple_path_segment> simple_path_segment
%type <Cabs.attr_input> attr_input
%type <Cabs.attr_input option> maybe_attr_input
%type <Cabs.expression> expression

%nonassoc LOWER
%nonassoc HIGHER
%right PATHSEP

%start<Cabs.item list> program
%%

program:
  | e = items EOF { e }

items:
  | /* empty */ { [] }
  | i = item rest = items { i :: rest }

item:
  | attrs = outer_attrs v = vis_item { Cabs.VISITEM (attrs, v) }

vis_item:
  | m = unsafe_module { Cabs.MODULE m }
  | e = extern_crate { Cabs.EXTERN_CRATE e }
  | decl = use_declaration { Cabs.USE_DECLARATION decl}

(*statement*)
statement:
  | item              { STATEMENT_ITEM $1 }
  | let_statement   { STATEMENT_LET $1 }
  | expression_statement  {STATEMENT_EXPRESSION_ $1 }

let_statement:
  | outer_attrs LET pattern_no_top_alt COLON typ option(let_expr) SEMI 
  { LET_STATEMENT ($1, $3, Some typ,) }

let_expr:
  | EQ expression option(else_expr) { EQ_EXPRESSION_ ($2, $3)}

else_expr:
  | ELSE block_expression { ELSE_BLOCK_EXPR $2 }

expression_statement:
  | exprs_without_block SEMI { EXPR_STATEMENT_NO_BLOCK $1}
  | exprs_with_block option(SEMI) { EXPR_STATEMENT_BLOCK $1 }

(*statement*)
(*Expression*)
expression:
  | outer_attrs exprs_without_block {EXPRESSION_WITHOUT_BLOCK ($1, $2) }
  | outer_attrs exprs_with_block {EXPRESSION_WITH_BLOCK ($1, $2) }

exprs_without_block:
  | literal_expression { LITERAL_EXPRESSION $1 }
  | path_expression { PATH_EXPRESSION $1 }
  | operator_expression { OPERATOR_EXPRESSION $1 }
  | grouped_expression { GROUPED_EXPRESSION $1 }
  | array_expression { ARRAY_EXPRESSION $1 }
  | await_expression { AWAIT_EXPRESSION $1 }
  | index_expression { INDEX_EXPRESSION $1 }
  | tuple_expression { TUPLE_EXPRESSION $1 }
  | tuple_indexing_expression { TUPLE_INDEXING_EXPRESSION $1 }
  | struct_expression { STRUCT_EXPRESSION $1 }
  | call_expression { CALL_EXPRESSION $1 }
  | method_call_expression { METHOD_CALL_EXPRESSION $1 }
  | field_expression { FIELD_EXPRESSION $1 }
  | closure_expression { CLOSURE_EXPRESSION $1 }
  | async_block_expression { ASYNC_BLOCK_EXPRESSION $1 }
  | continue_expression { CONTINUE_EXPRESSION $1 }
  | break_expression { BREAK_EXPRESSION $1 }
  | range_expression { RANGE_EXPRESSION $1 }
  | return_expression { RETURN_EXPRESSION $1 }
  | UNDERSCORE { UNDERSCORE_EXPRESSION }

exprs_with_block:
  | block_expression { BLOCK_EXPRESSION $1 }
  | const_block_expression { CONST_BLOCK_EXPRESSION $1 }
  | unsafe_block_expression { UNSAFE_BLOCK_EXPRESSION $1 }
  | loop_expression { LOOP_EXPRESSION $1 }
  | if_expression { IF_EXPRESSION $1 }
  | if_let_expression { IF_LET_EXPRESSION $1 }
  | match_expression { MATCH_EXPRESSION $1 }
(*Return expression*)
return_expression:
  | RETURN option(expression) { RETURN_EXPRESSION $2 }
(*Return expression*)
(*Async expression*)
async_block_expression:
  | ASYNC block_expression { ASYNC_BLOCK_EXPR_STILL $2 }
  | ASYNC MOVE block_expression { ASYNC_BLOCK_EXPR_MOVE $3 }
(*Async expression*)
(*Await expression*)
await_expression:
    | expression DOT AWAIT { AWAIT_EXPRESSION_ $1 }
(*Await expression*)
(*Literal expression*)
literal_expression:
  | CHAR_LIT         { CHAR_LITERAL $1 }
  | STRING_LIT       { STRING_LITERAL $1 }
  | RAW_STRING_LIT   { RAW_STRING_LITERAL $1 }
  | BYTE         { BYTE_LITERAL $1 }
  | BYTE_STRING  { BYTE_STRING_LITERAL $1 }
  | RAW_BYTE_STRING { RAW_BYTE_STRING_LITERAL $1 }
  | C_STRING    { C_STRING_LITERAL $1 }
  | RAW_C_STRING { RAW_C_STRING_LITERAL $1 }
  | INT_LIT      { INTEGER_LITERAL $1 }
  | FLOAT_LIT        { FLOAT_LITERAL $1 }
  | TRUE                 { TRUE }
  | FALSE                { FALSE }
(*Literal expression*)

(*Path expression*)
path_expression:
  | path_in_expression                       { PATH_EXPRESSION_ $1 }
  | qualified_path_in_expression             { QUALIFIED_PATH_IN_EXPRESSION $1 }
(*Path expression*)

(*Grouped expression*)
grouped_expression:
  | LPAREN expression RPAREN                 { GROUPED_EXPRESSION_ $2 }
(*Grouped expression*)

(*Tuple Expression*)
tuple_expression:
  | LPAREN option(tuple_elements) RPAREN  { TUPLE_EXPRESSION_ $2 }

tuple_elements:
  | separated_nonempty_list(COMMA, expression) { $1 }

tuple_indexing_expression:
  | expression DOT INT_LIT     { TUPLE_INDEXING_EXPRESSION_ ($1, $3) }

(*Tuple Expression*)
(*Array expression*)
array_expression:
  | LBRACK array_elements RBRACK             { ARRAY_EXPRESSION_ $2 }

array_elements:
  | expression_list option(COMMA)                          { ARRAY_ElEMENT $1 }
  | expression SEMI expression               { SEMI_ARRAY_ELEMENT $1 $3 }

expression_list:
  | expression { [$1] }
  | expression_list COMMA expression { $3 :: $1 }

(*Array expression*)
(*Index expression*)
index_expression:
  | expression LBRACK expression RBRACK      { INDEX_EXPRESSION_ $1 $3 }
(*Struct Expression*)
struct_expression:
  | path_in_expression LBRACE struct_expr_field_or_struct_base_opt RBRACE
      { STRUCT_EXPRESSION_STRUCT ($1, $3) }
  | path_in_expression LPAREN option(expr_list) RPAREN
      { STRUCT_EXPRESSION_TUPLE ($1, $3) }
  | path_in_expression                       { STRUCT_EXPRESSION_UNIT $1 }

struct_expr_field_or_struct_base_opt:
  | struct_expr_fields                       { Some (STRUCT_EXPR_FIELD_OPT $1) }
  | DOTDOT expression                        { Some (STRUCT_BASE_OPT (STRUCT_BASE $2)) }
  |                                          { None }

struct_expr_fields:
  | struct_expr_field_list COMMA struct_base 
  { STRUCT_EXPR_FIELDS_BASE ((Stdlib.List.rev $1 ), $3)}
  | struct_expr_field_list option(COMMA) {STRUCT_EXPR_FIELDS (Stdlib.List.rev $1 ) }

struct_expr_field_list:
  | struct_expr_field { [$1] }
  | struct_expr_field_list COMMA struct_expr_field { $3 :: $1 } 

base_comma_none:
  | COMMA struct_base
  | COMMA {}
  | {}
struct_expr_field:
  | outer_attrs ident COLON expression {
      STRUCT_EXPR_FIELD $1
    }

struct_base:
  | DOTDOT expression                  { STRUCT_BASE $2 }

expr_list:
  | separated_nonempty_list(COMMA, expression)   { EXPR_LIST $1 }

(*Struct Expression*)
(*Call Expression*)

call_expression:
  | expression LPAREN call_params_opt RPAREN { CALL_EXPRESSION_ $1 $3 }

call_params_opt:
  | call_params                              { $1 }
  |                                          { CALL_PARAMS dummy_expr [] } (* or handle empty specially *)

call_params:
  |  expression_list { CALL_PARAMS $1 }

method_call_expression:
  | expression DOT ident LPAREN call_params_opt RPAREN 
  { METHOD_CALL_EXPRESSION_ ($1, (IDENTIFIER_SEGMENT $3), (Some $5)) }

(*Call Expression*)
(*Field Expression*)
field_expression:
  | expression DOT ident                { FIELD_EXPRESSION_ $1 $3 }
(*Field Expression*)
(*Closure Expression*)
closure_expression:
  | OR closure_params_opt OR expression {
      CLOSURE_EXPRESSION_ false false $2 (EXPR_OPT $4)
    }

closure_params_opt:
  | closure_params                           { Some $1 }
  |                                          { None }

closure_params:
  | closure_param COMMA closure_param_list   { CLOSURE_PARAMS $1 $3 }

closure_param_list:
  | closure_param                            { [$1] }
  | closure_param COMMA closure_param_list   { $1 :: $3 }

closure_param:
  | outer_attr pattern COLON typ {
      CLOSURE_PARAM $1 $2 (Some $4)
    }
(*Closure Expression*)
(*Block expression*)

const_block_expression:
  | CONST block_expression { CONST_BLOCK_EXPRESSION_ $2 }

unsafe_block_expression:
  | UNSAFE block_expression { UNSAFE_BLOCK_EXPRESSION_ $2 }

block_expression:
  | LBRACE inner_attrs option(statements) RBRACE {BLOCK_EXPRESSION_ ($2, $3) }
  
statements:
  | list(statement) { STATEMENTS $1 }
  | list(statement) exprs_without_block { STATEMENTS ($1, $2) }
  | exprs_without_block {STATEMENTS $1}

(*Block expression*)
(*Loop expression*)

loop_expression:
  | LOOP block_expression         { LOOP_EXPRESSION_ (None, (INFINITE_LOOP_EXPRESSION $2) }

break_expression:
  | BREAK                                    { BREAK_EXPRESSION_ None None }
  | BREAK ident                         { BREAK_EXPRESSION_ (Some $2) None }
  | BREAK expression                         { BREAK_EXPRESSION_ None (Some $2) }

continue_expression:
  | CONTINUE                                 { CONTINUE_EXPRESSION_ None }
  | CONTINUE ident                      { CONTINUE_EXPRESSION_ (Some $2) }

(*Loop expression*)
(*Range Expression*)

range_expression:
  | expression DOTDOT expression             { RANGE_EXPRESSION_ ($1, $3) }
  | expression DOTDOT                        { RANGE_FROM_EXPRESSION_ $1 }
  | DOTDOT expression                        { RANGE_TO_EXPRESSION_ $2 }
  | DOTDOT                                   { RANGE_FULL_EXPRESSION_ dummy_range }
  | expression DOTDOTEQ expression          { RANGE_INCLUSIVE_EXPRESSION_ ($1, $3) }
  | DOTDOTEQ expression                     { RANGE_TO_INCLUSIVE_EXPRESSION_ $2 }
(*Range Expression*)
(*If_let Expression*)
if_expression:
  | IF expression block_expression else_opt {
      IfExpr $2 $3 $4
    }

else_opt:
  | ELSE block_expression                    { Some (BLOCK $2) }
  | ELSE if_expression                       { Some (IF $2) }
  | ELSE if_let_expression                   { Some (IF_LET $2) }
  |                                          { None }

if_let_expression:
  | IF LET pattern EQ expression block_expression else_opt {
      IF_LET_EXPRESSION_ $3 (SCRUTINEE $5) $6 $7
    }
(*If_let Expression*)
(*Match Expression*)
match_expression:
  | MATCH scrutinee LBRACE inner_attrs option(match_arms) RBRACE 
  { MATCH_EXPRESSION_ ($2, $4, $5)}
  
scrutinee:
  | expression { SCRUTINEE $1 }

match_arms:
    | first_arms match_arm FATARROW expression option(COMMA) 
  { MATCH_ARMS ($1, $2, $4) }

first_arms:
  | match_arm FATARROW block_or_not { ORGANIZE ($1, $3)}

block_or_not:
  | exprs_without_block COMMA { $1 }
  | exprs_with_block option(COMMA) { $1 }

match_arm:
  | outer_attrs pattern option(match_arm_guard) { MATCH_ARM ($1, $2, $3) }

match_arm_guard:
    | IF expression { MATCH_ARM_GUARD ($1) }

(*Match Expression*)
(*Operator expression*)
operator_expression:
  | AND expression                          { BORROW_EXPRESSION (BK_Shared, $2) }
  | AMPMUT expression                       { BORROW_EXPRESSION (BK_Mut, $2) }
  | RAW_CONST expression                    { BORROW_EXPRESSION (BK_RawConst, $2) }
  | RAW_MUT expression                      { BORROW_EXPRESSION (BK_RawMut, $2) }

  | STAR expression                         { DEREFERENCE_EXPRESSION ($2) }

  | expression QUESTION                     { ERROR_PROPAGATION_EXPRESSION ($1) }

  | MINUS expression                        { NEGATION_EXPRESSION (NEG_EXPRESSION_ ($2)) }
  | NOT expression                          { NEGATION_EXPRESSION (NOT_EXPRESSION_ ($2)) }

  | expression PLUS expression              { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_ADD, $3) }
  | expression MINUS expression             { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_SUB, $3) }
  | expression STAR expression              { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_MUL, $3) }
  | expression SLASH expression             { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_DIV, $3) }
  | expression PERCENT expression           { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_REM, $3) }

  | expression AND expression               { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_AND, $3) }
  | expression OR expression                { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_OR, $3) }
  | expression XOR expression               { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_XOR, $3) }
  | expression SHL expression               { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_SHL, $3) }
  | expression SHR expression               { ARITHMETIC_OR_LOGICAL_EXPRESSION ($1, AOP_SHR, $3) }

  | expression EQEQ expression              { COMPARISON_EXPRESSION ($1, CMPOP_EQ, $3) }
  | expression NE expression                { COMPARISON_EXPRESSION ($1, CMPOP_NE, $3) }
  | expression LT expression                { COMPARISON_EXPRESSION ($1, CMPOP_LT, $3) }
  | expression GT expression                { COMPARISON_EXPRESSION ($1, CMPOP_GT, $3) }
  | expression LE expression                { COMPARISON_EXPRESSION ($1, CMPOP_LE, $3) }
  | expression GE expression                { COMPARISON_EXPRESSION ($1, CMPOP_GE, $3) }

  | expression ANDAND expression            { LAZY_BOOLEAN_EXPRESSION ($1, LBOP_AND, $3) }
  | expression OROR expression              { LAZY_BOOLEAN_EXPRESSION ($1, LBOP_OR, $3) }

  | expression AS type_no_bounds            { TYPE_CAST_EXPRESSION ($1, $3) }

  | expression EQ expression                { ASSIGNMENT_EXPRESSION ($1, $3) }

  | expression PLUSEQ expression            { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_ADDASSIGN, $3) }
  | expression MINUSEQ expression           { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_SUBASSIGN, $3) }
  | expression STAREQ expression            { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_MULASSIGN, $3) }
  | expression SLASHEQ expression           { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_DIVASSIGN, $3) }

  | expression ANDEQ expression             { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_ANDASSIGN, $3) }
  | expression OREQ expression              { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_ORASSIGN, $3) }
  | expression XOREQ expression             { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_XORASSIGN, $3) }
  | expression SHLEQ expression             { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_SHLASSIGN, $3) }
  | expression SHREQ expression             { COMPOUND_ASSIGNMENT_EXPRESSION ($1, CAOP_SHRASSIGN, $3) }

(*Operator expression*)
(*Expression*)
(*generic params*)
generic_params:
  | LT GT { GENERIC_PARAMS_EMPTY }
  | LT separated_nonempty_list(COMMA, generic_param) GT { GENERIC_PARAMS $2 }

generic_param:
  | outer_attrs lifetime_param {GP_LIFETIME ($1, $2)}
  | outer_attrs type_param {GP_TYPE ($1, $2)}
  | outer_attrs const_param {GP_CONST ($1, $2)}

lifetime_param:
  | lifetime option(col_life_bounds) {LIFETIME_PARAM($1,$2)}

col_life_bounds:
  | COLON lifetime_bounds { $2 }

type_param:
  | ident option(col_param_bounds) option(eq_typ) {TYPE_PARAM ($1, $2, $3) }

eq_typ:
  | EQ typ {$2}

col_param_bounds:
  | COLON option(type_param_bounds) { $2 }

const_param:
  | CONST ident COLON typ option(const_param_body) { CONST_PARAM ($2, $3, $4)}

const_param_body:
  | EQ block_expression { $2 }
  | ident { $1 }
  | literal_expression { $1 }

where_clause:
  | WHERE separated_nonempty_list(COMMA, where_clause_item) { WHERE_CLAUSE $2 }

where_clause_item:
  | lifetime_where_clause_item { $1 }
  | type_bound_where_clause_item { $1 }

lifetime_where_clause_item:
  | lifetime COLON lifetime_bounds {WC_LIFETIME ($1, $3) }

type_bound_where_clause_item:
  | option(for_lifetimes) typ COLON option(type_param_bounds) 
  { WC_TYPE ($1, $2, $4) }

(*generic params*)
(*Trait and lifetime bounds*)
type_param_bounds:
  | type_param_bound_list option(PLUS) { TYPE_PARAM_BOUNDS (Stdlib.List.rev $1) }

type_param_bound_list:
  | type_param_bound { [$1] }
  | type_param_bound_list PLUS type_param_bound { $3 :: $1 }

type_param_bound:
  | lifetime { TYPE_PARAM_BOUND_LIFETIME $1 }
  | trait_bound { TYPE_PARAM_BOUND_TRAIT_BOUND $1 }
  | use_bound { TYPE_PARAM_BOUND_USE_BOUND $1 }

trait_bound:
  | option (question_or_for) type_path { TRAIT_BOUND ($1, $2)}
  | LPAREN option (question_or_for) type_path RPAREN 
  { ENCASED_TRAIT_BOUND ($2, $3) }

lifetime_bounds:
  | lifetime_list option(PLUS) { LIFETIME_BOUNDS $1 }

lifetime_list:
  | lifetime { [$1] }
  | lifetime_list PLUS lifetime { $3 :: $1 }

lifetime:
  | LIFETIME_OR_LABEL { LIFETIME $1 }
  | STATIC_LIFETIME { LIFETIME_STATIC }
  | ELIDED_LIFETIME { LIFETIME_UNDERSCORE }

question_or_for:
  | QUESTION      { QUESTION }
  | for_lifetimes { FOR_LF $1 }

for_lifetimes:
  | FOR generic_params { FOR_LIFETIMES $2 }

use_bound:
  | USE use_bound_generic_args { USE_BOUND $2 }

use_bound_generic_args:
  | LT GT { USE_BOUND_GENERIC_ARGS_EMPTY }
  | LT use_bound_generic_arg_list option(COMMA) GT { USE_BOUND_GENERIC_ARGS $2 }  

use_bound_generic_arg_list:
  | use_bound_generic_arg { [$1] }
  | use_bound_generic_arg COMMA use_bound_generic_arg { $3 :: $1 }

use_bound_generic_arg:
  | lifetime          { USE_BOUND_GENERIC_ARG_LIFETIME $1}
  | ident             { USE_BOUND_GENERIC_ARG_IDENT $1 } 
  | SELFTYPE        { USE_BOUND_GENERIC_ARG_SELF }

(*Trait and lifetime bounds*)
(*Functions*)

func:
  | function_qualifiers FN ident option(generic_params) LPAREN option(function_params) RPAREN 
option(function_return_type) option(where_clause) function_body { FUNCTION_DEF ($1, $3, $5, $7, $8, $9) }

function_body:
  | block_expression { FN_BODY_BLOCK $1 }
  | SEMI { FN_BODY_SEMI }

function_qualifiers: 
  | const = is_const async = is_async unsafe = is_unsafe opt = option(ex_op) 
    { FUNCTION_QUALIFIERS (const, async, unsafe, opt)}

ex_op:
  | EXTERN a = abi { a }
  | EXTERN { }

abi:
  | RAW_STRING_LIT { ABI_STRING $1 }
  | STRING_LIT { ABI_STRING $1 }

function_params:
  | self_param option(COMMA) { FN_PARAMS_SELF $1 }
  | option(self_param) option(COMMA) nonempty_list(terminated(function_param,COMMA)) option(COMMA) 
      { FN_PARAMS_FULL ($1, $2) }

self_param:
  | outer_attrs shorthand_self { SELF_SHORT ($1, $2)}
  | outer_attrs typed_self { SELF_TYPED ($1, $2)}

shorthand_self:
  | AND is_mut SELFVALUE { SELF_SHORTHAND_REF $2 }
  | AND lifetime is_mut SELFVALUE { SELF_SHORTHAND_REF_LIFE ($2 $3)}
  | is_mut SELFVALUE { SELF_SHORTHAND $1 }

typed_self:
  | is_mut SELFVALUE COLON typ { TYPED_SELF ($1, $4)}

function_param:
  | outer_attrs DOTDOTDOT { FN_PARAM_DOTS $1 }
  | outer_attrs function_param_pattern { FN_PARAM_PATTERN ($1, $2) }
  | outer_attrs typ { FN_PARAM_TYPE ($1, $2) }

function_param_pattern:
  | pattern_no_top_alt COLON typ { FN_PARAM_PAT ($1, $2) }
  | pattern_no_top_alt COLON DOTDOTDOT { FN_PARAM_DOTDOTDOT $1 }

function_return_type:
  | RARROW typ { FN_RETURN_TYPE $2 }
 (* Functions*)
(*Patterns*)
pattern:
  | OR separated_nonempty_list(OR, pattern_no_top_alt) { PATTERN $2 }

pattern_no_top_alt:
  | no_range_pattern { PATTERN_NO_RANGE $1 } 
  | range_pattern { PATTERN_RANGE $1 }

no_range_pattern:
  | literal_pattern { LITERAL_PATTERN $1 }
  | identifier_pattern { $1 }
  | UNDERSCORE { WILDCARD_PATTERN }
  | DOTDOT { REST_PATTERN }
  | AND is_mut no_range_pattern { SINGLE_REFERENCE_PATTERN $1 }
  | ANDAND is_mut no_range_pattern { DOUBLE_REFERENCE_PATTERN $1 }
  | struct_pattern { $1 }
  | tuple_struct_pattern { $1 }
  | tuple_pattern { TUPLE_PATTERN $1 }
  | LPAREN pattern RPAREN { GROUPED_PATTERN $2 }
  | slice_pattern { SLICE_PATTERN $1 }
  | path_expression { PATH_PATTERN $1 }

(*Range pattern*)
range_pattern:
    range_inclusive_pattern       { $1 }
  | range_from_pattern            { $1 }
  | range_to_inclusive_pattern    { $1 }
  | obsolete_range_pattern        { $1 }

range_exclusive_pattern:
    range_pattern_bound DOTDOT range_pattern_bound
      { RangeExclusivePattern ($1, $3) }

range_inclusive_pattern:
    range_pattern_bound DOTDOTEQ range_pattern_bound
      { RangeInclusivePattern ($1, $3) }

range_from_pattern:
    range_pattern_bound DOTDOT
      { RangeFromPattern ($1) }

range_to_inclusive_pattern:
    DOTDOTEQ range_pattern_bound
      { RangeToInclusivePattern ($2) }

obsolete_range_pattern:
    range_pattern_bound DOTDOTDOT range_pattern_bound
      { ObsoleteRangePattern ($1, $3) }

range_pattern_bound:
    CHAR_LIT                  { CharLiteral($1) }
  | BYTE                  { ByteLiteral($1) }
  | MINUS INT_LIT         { NegIntLiteral($2) }
  | INT_LIT               { IntLiteral($1) }
  | MINUS FLOAT_LIT           { NegFloatLiteral($2) }
  | FLOAT_LIT                 { FloatLiteral($1) }
  | path_expression               { PathPatternBound($1) }
(*Range pattern*)

literal_pattern:
  | FALSE { Cabs.FALSE_PAT }
  | TRUE { Cabs.TRUE_PAT }
  | CHAR_LIT { CHAR_LITERAL $1 }
  | BYTE_STRING { Cabs.BYTE_STRING $1 }
  | STRING_LIT { Cabs.STRING_LITERAL $1 }
  | RAW_BYTE_STRING { Cabs.RAW_BYTE_STRING_LITERAL $1 }
  | RAW_STRING_LIT { Cabs.RAW_STRING_LITERAL $1 }
  | C_STRING { Cabs.C_STRING_LITERAL $1 }
  | RAW_C_STRING { Cabs.C_STRING_LITERAL $1 }
  | neg INT_LIT { Cabs.INTEGER_LITERAL ($1, $2) }
  | neg FLOAT_LIT { Cabs.FLOAT_LITERAL ($1, $2) }

neg:
  | MINUS { true }
  | { false }

identifier_pattern:
  | is_ref is_mut ident option(pat_at)
{ IDENTIFIER_PATTERN ($1, $2, $3) }

pat_at:
  | AT pattern_no_top_alt { $2 }

struct_pattern:
  | path_in_expression LBRACK option(struct_pattern_elements) RBRACK { STRUCT_PATTERN ($1, $3)}

struct_pattern_elements:
  | struct_pattern_fields option(comma_or_etcetera) 
{ STRUCT_PATTERN_ELEMENTS_FIELDS ($1, $2)}
  | struct_pattern_etcetara { STRUCT_PATTERN_ELEMENTS_ETCETERA $1 }

comma_or_etcetera:
  | COMMA struct_pattern_etcetara { Some $2 }
  | COMMA { None }

struct_pattern_fields:
  | separated_nonempty_list(COMMA, struct_pattern_field) { STRUCT_PATTERN_FIELDS $1 }

struct_pattern_field:
  | outer_attrs struct_pattern_field_body { STRUCT_PATTERN_FIELD ($1, $2)}

struct_pattern_field_body:
  | INT_LIT COLON pattern { TUPLE_PAT ($1, $3) }
  | ident COLON pattern {ID_PAT ($1, $3) }
  | is_ref is_mut ident { Cabs.ID ($1, $2, $3)}

struct_pattern_etcetara:
  | outer_attrs DOTDOT { STRUCT_PATTERN_ETCETERA $1 }

tuple_struct_pattern:
  | path_in_expression LPAREN option(tuple_struct_items) RPAREN 
  { TUPLE_STRUCT_PATTERN ($1, $3)}

tuple_struct_items:
  | tuple_struct_item_list option(COMMA) { Stdlib.List.rev $1 }

tuple_struct_item_list:
  | pattern { [$1] }
  | pattern COMMA pattern { $3 :: $1 }

tuple_pattern:
  | LPAREN option(tuple_pattern_items) RPAREN { $2 }

tuple_pattern_items:
  | pattern COMMA { PATTERN_ITEM $1 }
  | DOTDOT { REST_ITEM $1 }
  | separated_nonempty_list(COMMA, pattern) { PATTERN_ITEMS $1 }

slice_pattern:
  | LBRACK option(slice_pattern_items) RBRACK { SLICE_PATTERN $2 }

slice_pattern_items:
  | separated_nonempty_list(COMMA, pattern) { }

(*Patterns*)
typ:
  | type_no_bounds { RAW_POINTER_TYPE $1 }

type_no_bounds:
  | type_path { TYPE_PATH $1 }

type_path:
  | option(PATHSEP) separated_nonempty_list(PATHSEP,type_path_segment) 
{ TYP_PATH $2 }

type_path_segment:
  | path_ident_segment { TYPE_PATH_SEGMENT ($1, None) }

path_ident_segment:
  | id = ident {PATH_IDENT_SEGMENT_IDENT id  }
  | SUPER { PATH_IDENT_SEGMENT_SUPER }
  | SELFVALUE { PATH_IDENT_SEGMENT_SELF }
  | SELFTYPE { PATH_IDENT_SEGMENT_self }
  | CRATE { PATH_IDENT_SEGMENT_CRATE }
  | DOLLAR_CRATE {PATH_IDENT_SEGMENT_SCRATE }

(*Helpers*)
is_const:
  | CONST { true }
  | { false }

is_async:
  | ASYNC { true }
  | { false }

is_unsafe:
  | UNSAFE { true }
  | SAFE { false }
  | { false }

is_mut:
  | MUT { true }
  | { false }

is_ref:
  | REF { true }
  | { false }

(*Helpers*)

(*use declaration*)
use_declaration:
  | USE use_tree SEMI { Cabs.USE_DECL $2}

use_tree:
  | skib = simple_path_special STAR { Cabs.USE_TREE skib }
  | skib = simple_path_special;
  LBRACE;
  trees = use_trees
  RBRACE
    { Cabs.USE_TREE_LIST (skib, trees) }
  | what = simple_path as_id = as_id_or_underscore { Cabs.USE_TREE_ID (what, as_id)}


use_trees:
  | use_tree_list option(COMMA) { Stdlib.List.rev $1 }

use_tree_list:
  | use_tree { [$1] }
  | use_tree_list COMMA use_tree { $3 :: $1 }

as_id_or_underscore:
  | AS id = ident { Some (Cabs.ID_OPT (fst id)) }
  | AS UNDERSCORE { Some (Cabs.UNDERSCORE_OPT) }
  | { None }

(*use declaration*)

(*Paths*)
simple_path_special:
  | segments = nonempty_list(terminated(simple_path_segment, PATHSEP)) { Some(Cabs.SIMPLE_PATH segments) }
  | PATHSEP; segments = nonempty_list(terminated(simple_path_segment, PATHSEP)) { Some (Cabs.SIMPLE_PATH segments) }
  | { None }

simple_path:
  | option(PATHSEP) segments = separated_nonempty_list(PATHSEP, simple_path_segment) { Cabs.SIMPLE_PATH segments }

simple_path_segment:
  | id = ident   { Cabs.SIMPLE_PATH_SEGMENT_IDENT (fst id) }
  | SUPER { SIMPLE_PATH_SEGMENT_SUPER  }
  | SELFVALUE { SIMPLE_PATH_SEGMENT_SELF  }
  | CRATE { SIMPLE_PATH_SEGMENT_CRATE  }
  | DOLLAR_CRATE { SIMPLE_PATH_SEGMENT_SCRATE }

path_in_expression:
  | option(PATHSEP) segments = separated_nonempty_list(PATHSEP, path_expr_segment) 
  { Cabs.PATH_IN_EXPRESSION segments }

path_expr_segment:
  | path_ident_segment option(path_genarg) { PATH_EXPR_SEGMENT ($1, $2) }

path_genarg:
  | PATHSEP generic_args { $2 }

generic_args:
  | LT GT { EMPTY_GENERIC_ARGS }
  | LT generic_arg_list option(COMMA) GT { Stdlib.List.rev $2 }

generic_arg_list:
  | generic_arg { [$1] }
  | generic_args COMMA generic_arg { $3 :: $1 }

generic_arg:
  | lifetime              { GENERIC_ARG_LIFETIME $1 }
  | typ                   {  GENERIC_ARG_TYPE $1 }
  | generic_args_const     { GENERIC_ARG_CONST $1 }
  | generic_args_binding   { GENERIC_ARGS_BINDING $1 }
  | generic_args_bounds    { GENERIC_ARGS_BOUNDS $1 }

generic_args_const:
  | block_expression             { GENERIC_ARGS_CONST_BLOCK $1 }
  | literal_expression           { GENERIC_ARGS_CONST_LIT $1 }
  | MINUS literal_expression     { NEG_GENERIC_ARGS_CONST_LIT $1 }
  | simple_path_segment          { GENERIC_ARGS_CONST_SIMPLE_PATH_SEG $1 }

generic_args_bounds:
  | ident option(generic_args) EQ type_param_bounds { GENERIC_ARGS_BOUNDS_ ($1, $2, $4) }

generic_args_binding:
  | ident option(generic_args) EQ typ { GENERIC_ARGS_BINDING_ ($1, $2, $4) }

qualified_path_in_expression:
  | qualified_path_type separated_nonempty_list(PATHSEP, path_expr_segment) 
  { QUALIFIED_PATH_IN_EXPRESSION_ ($1, $2) }

qualified_path_type:
  | LT typ option(as_typath) GT { QUALIFIED_PATH_TYPE ($2, $3)}

as_typath:
  | AS type_path { $2 }

qualified_path_in_type:
  | qualified_path_type separated_nonempty_list(PATHSEP, type_path_segment) 
  { QUALIFIED_PATH_IN_TYPE ($1, $2) }
(*Paths*)

(*Extern crate*)
extern_crate:
  | EXTERN CRATE ref = crate_ref clause = as_clause SEMI { Cabs.EXT_CRATE_CLAUSE (ref, clause) }

crate_ref:
  | id = ident { Cabs.ID_CRATE_REF (fst id) }
  |  SELFVALUE {Cabs.SELF_CRATE_REF}

as_clause:
  | AS id = ident { Some (Cabs.ID_AS_CLAUSE (fst id)) }
  | AS UNDERSCORE { Some (Cabs.UNDERSCORE_AS_CLAUSE) }
  | { None }
(*Extern crate*)

(*Modules*)
unsafe_module:
  | sm = safe_module { sm }
  | UNSAFE MOD name = ident SEMI
    { MOD_BLOCK (true, (fst name)) }
  | UNSAFE MOD name = ident LBRACE attrs = inner_attrs content = items RBRACE
    { MOD_DEC (true, (fst name), Stdlib.List.rev (attrs), content) }

safe_module:
  | MOD name = ident SEMI
    { MOD_BLOCK (false, (fst name)) }
  | MOD name = ident LBRACE attrs = inner_attrs content = items RBRACE
    { MOD_DEC (false, (fst name), Stdlib.List.rev (attrs), content) }

(*Modules*)

ident:
  | id = IDENT { (IDENT (fst id), snd id) }
  | raw = RAW_IDENT { (RAW_IDENT (fst raw), snd raw) }

(* ATTRIBUTE *)
outer_attrs:
  | /* empty */ { [] }
  | POUND a = outer_attr rest = outer_attrs { a :: rest }

outer_attr:
  | LBRACK a = attr RBRACK { OUTER_ATTRIBUTE a }

inner_attrs:
  | /* empty */ { [] }
  | rest = inner_attrs POUND a = inner_attr { a :: rest }

inner_attr:
  | NOT LBRACK a = attr RBRACK { INNER_ATTRIBUTE a }

attr:
  | path = simple_path input = maybe_attr_input { SAFE_ATTR (path, input) }
  | UNSAFE path = simple_path input = maybe_attr_input { UNSAFE_ATTR (path, input) }

maybe_attr_input:
  | a = attr_input { Some a }
  | /* empty */ { None }

attr_input:
  | EQ e = expression { ATTR_INPUT_EXP e }
(* ATTRIBUTE *)

