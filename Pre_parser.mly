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
%type <Cabs.outer_attribute> outer_attr
%type <Cabs.inner_attribute> inner_attr
%type <Cabs.attr> attr 
%type <Cabs.simple_path> simple_path
%type <Cabs.simple_path_segment> simple_path_segment
%type <Cabs.attr_input> attr_input
%type <Cabs.attr_input option> maybe_attr_input
%type <Cabs.expression> expression


%start<Cabs.item list> program
%%

nonempty_separated_or_terminated_list(sep, elem):
  | nonempty_list(terminated(elem, sep)) { $1 }
  | separated_nonempty_list(sep, elem) { $1 }

separated_or_terminated_list(sep, elem):
  | list(terminated(elem, sep)) { $1 }
  | separated_list(sep, elem) { $1 }

program:
  | e = items EOF { e }

items:
  | /* empty */ { [] }
  | i = item rest = items { i :: rest }

item:
  | attrs = list(outer_attr) v = vis_item { Cabs.VISITEM (attrs, v) }

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
  | list(outer_attr) LET pattern_no_top_alt COLON typ option(let_expr) SEMI 
  { LET_STATEMENT ($1, $3, Some $5 , $6) }

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
  | nonempty_list(outer_attr) exprs_without_block {EXPRESSION_WITHOUT_BLOCK ($1, $2) }
  | exprs_without_block {EXPRESSION_WITHOUT_BLOCK ([], $1) }

  | nonempty_list(outer_attr) exprs_with_block {EXPRESSION_WITH_BLOCK ($1, $2) }
  | exprs_with_block {EXPRESSION_WITH_BLOCK ([], $1) }

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
  | RETURN option(expression) { RETURN_EXPRESSION_ $2 }
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
  | CHAR_LIT         { CHAR_LITERAL (fst $1) }
  | STRING_LIT       { STRING_LITERAL (fst $1) }
  | RAW_STRING_LIT   { RAW_STRING_LITERAL (fst $1) }
  | BYTE         { BYTE_LITERAL (fst $1) }
  | BYTE_STRING  { BYTE_STRING_LITERAL (fst $1) }
  | RAW_BYTE_STRING { RAW_BYTE_STRING_LITERAL (fst $1) }
  | C_STRING    { C_STRING_LITERAL (fst $1) }
  | RAW_C_STRING { RAW_C_STRING_LITERAL (fst $1) }
  | INT_LIT      { INTEGER_LITERAL (fst $1) }
  | FLOAT_LIT        { FLOAT_LITERAL (fst $1) }
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
  | separated_nonempty_list(COMMA, expression) { TUPLE_ELEMENTS $1 }

tuple_indexing_expression:
  | expression DOT INT_LIT     { TUPLE_INDEXING_EXPRESSION_ ($1, (fst $3)) }

(*Tuple Expression*)
(*Array expression*)
array_expression:
  | LBRACK array_elements RBRACK             { ARRAY_EXPRESSION_ $2 }

array_elements:
  | nonempty_separated_or_terminated_list(COMMA, expression)  { ARRAY_ElEMENT $1 }
  | expression SEMI expression               { SEMI_ARRAY_ELEMENT ($1, $3) }

(*Array expression*)
(*Index expression*)
index_expression:
  | expression LBRACK expression RBRACK      { INDEX_EXPRESSION_ ($1, $3) }
(*Struct Expression*)
struct_expression:
  | path_in_expression struct_expr_body     { STRUCT_EXPRESSION_ ($1, $2) }

struct_expr_body:
  | LBRACE struct_expr_fields RBRACE { STRUCT_EXPRESSION_FIELDS $2 }
  | LBRACE DOTDOT expression RBRACE { STRUCT_EXPRESSION_EXPR $3 }
  | LPAREN expr_list RPAREN { STRUCT_EXPRESSION_TUPLE $2 }

struct_expr_fields:
  | separated_list(COMMA, struct_expr_field) COMMA struct_base { STRUCT_EXPR_FIELDS_BASE ($1, $3)}
  | nonempty_separated_or_terminated_list(COMMA, struct_expr_field) {STRUCT_EXPR_FIELDS $1 }

struct_expr_field:
  | list(outer_attr) ident COLON expression { STRUCT_EXPR_FIELD $1 }

struct_base:
  | DOTDOT expression                  { STRUCT_BASE $2 }

expr_list:
  | separated_list(COMMA, expression)   { EXPR_LIST $1 }

(*Struct Expression*)
(*Call Expression*)

call_expression:
  | expression LPAREN call_params RPAREN { CALL_EXPRESSION_ ($1, $3) }

call_params:
  | separated_list(COMMA, expression)   { CALL_PARAMS $1 }

method_call_expression:
  | expression DOT path_expr_segment LPAREN call_params RPAREN 
  { METHOD_CALL_EXPRESSION_ ($1,  $3, $5) }

(*Call Expression*)
(*Field Expression*)
field_expression:
  | expression DOT ident                { FIELD_EXPRESSION_ ($1, fst $3) }
(*Field Expression*)
(*Closure Expression*)
closure_expression:
  | is_async is_move OR option(closure_params) OR expr_or_typ_block 
  { CLOSURE_EXPRESSION_ ($1, $2, $4, $6) }

expr_or_typ_block:
  | expression { EXPR_OPT $1 }
  | RARROW type_no_bounds block_expression { TYPE_BLOCK_OPT ($2, $3) }

closure_params:
  | nonempty_separated_or_terminated_list(COMMA, closure_param) { CLOSURE_PARAMS $1 }

closure_param:
  | list(outer_attr) pattern_no_top_alt option(typ_opt) { CLOSURE_PARAM ($1, $2, $3)}

typ_opt:
  | COLON typ { $2 }

(*Closure Expression*)
(*Block expression*)

const_block_expression:
  | CONST block_expression { CONST_BLOCK_EXPRESSION_ $2 }

unsafe_block_expression:
  | UNSAFE block_expression { UNSAFE_BLOCK_EXPRESSION_ $2 }

block_expression:
  | LBRACE list(inner_attr) option(statements) RBRACE {BLOCK_EXPRESSION_ ($2, $3) }
  
statements:
  | list(statement) { STATEMENTS $1 }
  | list(statement) exprs_without_block { STATEMENTS_EXPR_WITHOUT_BLOCK ($1, $2) }
  | exprs_without_block {S_EXPR_WITHOUT_BLOCK $1}

(*Block expression*)
(*Loop expression*)
loop_expression:
  | option(loop_label) loop_switch { LOOP_EXPRESSION_ ($1, $2) }

loop_label:
  | LIFETIME_OR_LABEL COLON { LOOP_LABEL (fst $1) }

loop_switch:
  | infinite_loop_expression { $1 }
  | predicate_loop_expression  { $1 }
  | predicate_pattern_loop_expression  { $1 }
  | iterator_loop_expression  { $1 }
  | label_block_expression  { $1 }

infinite_loop_expression:
  | LOOP block_expression { INFINITE_LOOP_EXPRESSION $2 }

predicate_loop_expression:
  | WHILE expression block_expression { PREDICATE_LOOP_EXPRESSION ($2, $3) }

predicate_pattern_loop_expression:
  | WHILE LET pattern EQ scrutinee block_expression { PREDICATE_PATTERN_LOOP_EXPRESSION ($3, $5, $6)}

iterator_loop_expression:
  | FOR pattern IN expression block_expression { ITERATOR_LOOP_EXPRESSION ($2, $4, $5) }

label_block_expression:
  | block_expression { LABEL_BLOCK_EXPRESSION $1 }

break_expression:
  | BREAK option(LIFETIME_OR_LABEL) option(expression) { BREAK_EXPRESSION_ (Option.map fst $2, $3) }

continue_expression:
  | CONTINUE option(LIFETIME_OR_LABEL) { CONTINUE_EXPRESSION_ (Option.map fst $2) }

(*Loop expression*)
(*Range Expression*)

range_expression:
  | expression DOTDOT expression             { RANGE_EXPRESSION_ ($1, $3) }
  | expression DOTDOT                        { RANGE_FROM_EXPRESSION_ $1 }
  | DOTDOT expression                        { RANGE_TO_EXPRESSION_ $2 }
  | DOTDOT                                   { RANGE_FULL_EXPRESSION_  }
  | expression DOTDOTEQ expression          { RANGE_INCLUSIVE_EXPRESSION_ ($1, $3) }
  | DOTDOTEQ expression                     { RANGE_TO_INCLUSIVE_EXPRESSION_ $2 }
(*Range Expression*)
(*If_let Expression*)
if_expression:
  | IF expression block_expression else_opt { IfExpr ($2, $3, $4) }

else_opt:
  | ELSE block_expression                    { Some (BLOCK $2) }
  | ELSE if_expression                       { Some (IF $2) }
  | ELSE if_let_expression                   { Some (IF_LET $2) }
  |                                          { None }

if_let_expression:
  | IF LET pattern EQ expression block_expression else_opt {
      IF_LET_EXPRESSION_ ($3, (SCRUTINEE $5), $6, $7)
    }
(*If_let Expression*)
(*Match Expression*)
match_expression:
  | MATCH scrutinee LBRACE list(inner_attr) option(match_arms) RBRACE 
  { MATCH_EXPRESSION_ ($2, $4, $5)}
  
scrutinee:
  | expression { SCRUTINEE $1 }

match_arms:
    | list(first_arms) match_arm FATARROW expression option(COMMA) 
  { MATCH_ARMS ($1, $2, $4) }

first_arms:
  | match_arm FATARROW block_or_not { ORGANIZE ($1, $3)}

block_or_not:
  | exprs_without_block COMMA { NO_BLOCK $1 }
  | exprs_with_block option(COMMA) { YES_BLOCK $1 }

match_arm:
  | list(outer_attr) pattern option(match_arm_guard) { MATCH_ARM ($1, $2, $3) }

match_arm_guard:
    | IF expression { MATCH_ARM_GUARD ($2) }

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
  | list(outer_attr) lifetime_param {GP_LIFETIME ($1, $2)}
  | list(outer_attr) type_param {GP_TYPE ($1, $2)}
  | list(outer_attr) const_param {GP_CONST ($1, $2)}

lifetime_param:
  | lifetime { LIFETIME_PARAM (fst $1)}
  | lifetime COLON option(lifetime_bounds) { LIFETIME_PARAM_BOUND (fst $1, $3) }

col_life_bounds:
  | COLON lifetime_bounds { $2 }

type_param:
  | ident option(eq_typ) {TYPE_PARAM (fst $1, $2) }
  | ident option(type_param_bounds) option(eq_typ) { TYPE_PARAM_BOUND (fst $1, $2, $3) }

eq_typ:
  | EQ typ {$2}

const_param:
  | CONST ident COLON typ option(const_param_body) { CONST_PARAM (fst $2, $4, $5)}

const_param_body:
  | EQ block_expression { CONST_PARAM_BLOCK $2 }
  | ident { CONST_PARAM_IDENT (fst $1) }
  | literal_expression { CONST_PARAM_LIT $1 }

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
  | nonempty_separated_or_terminated_list(PLUS, type_param_bound) { TYPE_PARAM_BOUNDS $1 }

type_param_bound:
  | lifetime { TYPE_PARAM_BOUND_LIFETIME (fst $1) }
  | trait_bound { TYPE_PARAM_BOUND_TRAIT_BOUND $1 }
  | use_bound { TYPE_PARAM_BOUND_USE_BOUND $1 }

trait_bound:
  | trait_bound_body { TRAIT_BOUND $1 }
  | LPAREN trait_bound_body RPAREN { ENCASED_TRAIT_BOUND $2 }

trait_bound_body:
  | for_lifetimes type_path { FOR_BODY ($1, $2) }
  | QUESTION type_path { QUESTION_BODY $2 }
  | type_path { EMPTY_BODY $1 }

lifetime_bounds:
  | separated_or_terminated_list(PLUS, lifetime) 
      { LIFETIME_BOUNDS (Stdlib.List.map fst $1) }

lifetime:
  | LIFETIME_OR_LABEL       { (LIFETIME (fst $1), $startpos) }
  | STATIC_LIFETIME        { (LIFETIME_STATIC, $startpos) }
  | ELIDED_LIFETIME        { (LIFETIME_UNDERSCORE, $startpos) }

for_lifetimes:
  | FOR generic_params { FOR_LIFETIMES $2 }

use_bound:
  | USE use_bound_generic_args { USE_BOUND $2 }

use_bound_generic_args:
  | LT GT { USE_BOUND_GENERIC_ARGS_EMPTY }
  | LT nonempty_separated_or_terminated_list(COMMA, use_bound_generic_arg) GT
      { USE_BOUND_GENERIC_ARGS $2 }  

use_bound_generic_arg:
  | lifetime          { USE_BOUND_GENERIC_ARG_LIFETIME (fst $1) }
  | ident             { USE_BOUND_GENERIC_ARG_IDENT (fst $1) } 
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
  | const = is_const async = is_async unsafe = is_unsafe 
    { FUNCTION_QUALIFIERS (const, async, unsafe)}    
  | const = is_const async = is_async unsafe = is_unsafe EXTERN opt = option(abi) 
    { FUNCTION_QUALIFIERS_EXTERN (const, async, unsafe, opt)}

abi:
  | RAW_STRING_LIT { ABI_STRING (fst $1) }
  | STRING_LIT { ABI_STRING (fst $1) }

function_params:
  | self_param option(COMMA) { FN_PARAMS_SELF $1 }
  | option(self_param) option(COMMA) nonempty_list(terminated(function_param,COMMA)) option(COMMA) 
      { FN_PARAMS_FULL ($1, $2) }

self_param:
  | list(outer_attr) shorthand_self { SELF_SHORT ($1, $2)}
  | list(outer_attr) typed_self { SELF_TYPED ($1, $2)}

shorthand_self:
  | AND is_mut SELFVALUE { SELF_SHORTHAND_REF $2 }
  | AND lifetime is_mut SELFVALUE { SELF_SHORTHAND_REF_LIFE ($2 $3)}
  | is_mut SELFVALUE { SELF_SHORTHAND $1 }

typed_self:
  | is_mut SELFVALUE COLON typ { TYPED_SELF ($1, $4)}

function_param:
  | list(outer_attr) DOTDOTDOT { FN_PARAM_DOTS $1 }
  | list(outer_attr) function_param_pattern { FN_PARAM_PATTERN ($1, $2) }
  | list(outer_attr) typ { FN_PARAM_TYPE ($1, $2) }

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
  | AND is_mut no_range_pattern { SINGLE_REFERENCE_PATTERN ($2, $3) }
  | ANDAND is_mut no_range_pattern { DOUBLE_REFERENCE_PATTERN ($2, $3) }
  | struct_pattern { $1 }
  | tuple_struct_pattern { $1 }
  | tuple_pattern { TUPLE_PATTERN $1 }
  | LPAREN pattern RPAREN { GROUPED_PATTERN $2 }
  | LBRACK slice_pattern_items RBRACK  { SLICE_PATTERN $2 }
  | path_expression { PATH_PATTERN $1 }

(*Range pattern*)
range_pattern:
    range_inclusive_pattern       { $1 }
  | range_from_pattern            { $1 }
  | range_to_inclusive_pattern    { $1 }
  | obsolete_range_pattern        { $1 }

range_exclusive_pattern:
    range_pattern_bound DOTDOT range_pattern_bound
      { RANGE_EXCLUSIVE_PATTERN ($1, $3) }

range_inclusive_pattern:
    range_pattern_bound DOTDOTEQ range_pattern_bound
      { RANGE_INCLUSIVE_PATTERN ($1, $3) }

range_from_pattern:
    range_pattern_bound DOTDOT
      { RANGE_FROM_PATTERN $1 }

range_to_inclusive_pattern:
    DOTDOTEQ range_pattern_bound
      { RANGE_TO_INCLUSIVE_PATTERN ($2) }

obsolete_range_pattern:
    range_pattern_bound DOTDOTDOT range_pattern_bound
      { RANGE_OBSOLETE_PATTERN ($1, $3) }

range_pattern_bound:
  | CHAR_LIT              { RANGE_PATTERN_BOUND_CHAR (fst $1) }
  | BYTE                  { RANGE_PATTERN_BOUND_BYTE (fst $1) }
  | MINUS INT_LIT         { RANGE_PATTERN_BOUND_NEG_INTEGER (fst $2) }
  | INT_LIT               { RANGE_PATTERN_BOUND_INTEGER (fst $1) }
  | MINUS FLOAT_LIT       { RANGE_PATTERN_BOUND_NEG_FLOAT (fst $2) }
  | FLOAT_LIT             { RANGE_PATTERN_BOUND_FLOAT (fst $1) }
  | path_expression       { RANGE_PATTERN_BOUND_PATH $1 }
(*Range pattern*)

literal_pattern:
  | FALSE { Cabs.FALSE_PAT }
  | TRUE { Cabs.TRUE_PAT }
  | CHAR_LIT { CHAR_PATTERN (fst $1) }
  | BYTE_STRING { Cabs.BYTE_STRING_PATTERN (fst $1) }
  | STRING_LIT { Cabs.STRING_PATTERN (fst $1) }
  | RAW_BYTE_STRING { Cabs.RAW_BYTE_STRING_PATTERN (fst $1) }
  | RAW_STRING_LIT { Cabs.RAW_STRING_PATTERN (fst $1) }
  | C_STRING { Cabs.C_STRING_PATTERN (fst $1) }
  | RAW_C_STRING { Cabs.C_STRING_PATTERN (fst $1) }
  | neg INT_LIT { Cabs.INTEGER_PATTERN ($1, fst $2) }
  | neg FLOAT_LIT { Cabs.FLOAT_PATTERN ($1, fst $2) }

%inline neg:
  | MINUS { true }
  | { false }

identifier_pattern:
  | is_ref is_mut ident option(pat_at)
{ IDENTIFIER_PATTERN ($1, $2, fst $3, $4) }

pat_at:
  | AT pattern_no_top_alt { $2 }

struct_pattern:
  | path_in_expression LBRACK option(struct_pattern_elements) RBRACK { STRUCT_PATTERN ($1, $3)}

struct_pattern_elements:
  | struct_pattern_fields COMMA struct_pattern_etcetara
{ STRUCT_PATTERN_ELEMENTS_FIELDS_ETC ($1, $3)}
  | struct_pattern_etcetara { STRUCT_PATTERN_ELEMENTS_ETCETERA $1 }
  | struct_pattern_fields option(COMMA) { STRUCT_PATTERN_ELEMENTS_FIELDS $1 }

struct_pattern_fields:
  | separated_nonempty_list(COMMA, struct_pattern_field) { STRUCT_PATTERN_FIELDS $1 }

struct_pattern_field:
  | list(outer_attr) struct_pattern_field_body { STRUCT_PATTERN_FIELD ($1, $2)}

struct_pattern_field_body:
  | INT_LIT COLON pattern { TUPLE_PAT (fst $1, $3) }
  | ident COLON pattern {ID_PAT (fst $1, $3) }
  | is_ref is_mut ident { Cabs.ID ($1, $2, fst $3)}

struct_pattern_etcetara:
  | list(outer_attr) DOTDOT { STRUCT_PATTERN_ETCETERA $1 }

tuple_struct_pattern:
  | path_in_expression LPAREN tuple_struct_items RPAREN 
  { TUPLE_STRUCT_PATTERN ($1, $3)}

tuple_struct_items:
  | separated_or_terminated_list(COMMA, pattern) { TUPLE_STRUCT_ITEMS $1 }

tuple_pattern:
  | LPAREN option(tuple_pattern_items) RPAREN { $2 }

tuple_pattern_items:
  | pattern COMMA { PATTERN_ITEM $1 }
  | DOTDOT { REST_ITEM }
  | separated_nonempty_list(COMMA, pattern) { PATTERN_ITEMS $1 }

slice_pattern_items:
  | separated_list(COMMA, pattern) { SLICE_PATTERN_ITEMS $1 }

(*Patterns*)
(*Type*)

typ:
    type_no_bounds                        { TYPE_NO_BOUNDS $1 }
  | impl_trait_type                       { IMPL_TYPE $1 }
  | trait_object_type                     { TRAIT_TYPE $1 }

type_no_bounds:
    LPAREN typ RPAREN                 { PARENTHESIZED_TYPE $2 }
  | impl_trait_type_one_bound               { IMPL_ONE_BOUND $1 }
  | is_dyn trait_bound                  { TRAIT_ONE_BOUND ($1, $2) }
  | type_path                            { TYPE_PATH $1 }
  | tuple_type                           { TUPLE_TYPE $1 }
  | NOT                                   { NEVER_TYPE }
  | raw_pointer_type                      { RAW_POINTER_TYPE $1 }
  | reference_type                       { REFERENCE_TYPE $1 }
  | LBRACK typ SEMI expression RBRACK   { ARRAY_TYPE ($2, $4) }
  | LBRACK typ RBRACK                  { SLICE_TYPE $2 }
  | UNDERSCORE                          { INFERRED_TYPE }
  | qualified_path_in_type                { QUALIFIED_PATH $1 }
  | bare_function_type                    { BARE_FUNCTION_TYPE $1 }

impl_trait_type:
  | IMPL type_param_bounds { IMPL_TRAIT_TYPE $2 }
(*Bare Function Type*)
bare_function_type:
  | option(for_lifetimes) function_type_qualifiers FN LPAREN
  option(function_parameters_maybe_named_variadic) RPAREN option(bare_function_return_type)
  { BARE_FUNC_TYPE ($1, $2, $5, $7)}

function_type_qualifiers:
    | is_unsafe { FUNC_TYPE_QUALIFIERS $1 }
    | is_unsafe EXTERN option(abi) { FUNC_TYPE_QUALIFIERS_EXTERN ($1, $3) }

bare_function_return_type:
    | RARROW type_no_bounds { BARE_RETURN_TYPE $2 }

function_parameters_maybe_named_variadic:
    | maybe_named_function_parameters  { FN_PARAMS_NORMAL $1 }
    | maybe_named_function_parameters_variadic { FN_PARAMS_VARIADIC $1 }

maybe_named_function_parameters:
    |  separated_or_terminated_list(COMMA, maybe_named_param)
      { MAYBE_NAMED_FN_PARAMS $1 }

maybe_named_param:
    | list(outer_attr) ident COLON typ { MAYBE_NAMED_PARAM_ID ($1, fst $2, $4) }
    | list(outer_attr) UNDERSCORE COLON typ { MAYBE_NAMED_PARAM_UNDERSCORE ($1, $4) }
    | list(outer_attr) typ { MAYBE_NAMED_PARAM ($1, $2) }

maybe_named_function_parameters_variadic:
  | nonempty_list(terminated(maybe_named_param, COMMA)) list(outer_attr) DOTDOTDOT 
      { FN_PARAMS_VAR ($1, $2) }

trait_object_type:
    | is_dyn type_param_bounds { TRAIT_OBJECT_TYPE_PARAM ($1, $2) }

%inline is_dyn:
  | DYN { true }
  |     { false }

(*Bare Function Type*)
raw_pointer_type:
  | STAR MUT type_no_bounds               { MUT_RAW_POINTER $3 }
  | STAR CONST type_no_bounds             { CONST_RAW_POINTER $3 }

reference_type:
  | AND option(lifetime) is_mut type_no_bounds { REFER_TYP (Option.map fst $2, $3, $4)}

impl_trait_type_one_bound:
    IMPL trait_bound                     { $2 }

tuple_type:
  | LPAREN separated_or_terminated_list(COMMA, typ) RPAREN { TUPLE_TYPE_ $2 }

type_list:
    typ                                { [$1] }
  | typ COMMA type_list                { $1 :: $3 }
(*Type*)
(*Helpers*)

%inline is_move:
  | MOVE { true }
  | { false }

%inline is_const:
  | CONST { true }
  | { false }

%inline is_async:
  | ASYNC { true }
  | { false }

%inline is_unsafe:
  | UNSAFE { true }
  | SAFE { false }
  | { false }

%inline is_mut:
  | MUT { true }
  | { false }

%inline is_ref:
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
  | nonempty_separated_or_terminated_list(COMMA, use_tree) { $1 }

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

path_ident_segment:
  | ident { PATH_IDENT_SEGMENT_IDENT (fst $1) }
  | SUPER { PATH_IDENT_SEGMENT_SUPER }
    | SELFVALUE  { PATH_IDENT_SEGMENT_SELF }
  | SELFTYPE { PATH_IDENT_SEGMENT_self }
  | CRATE  {PATH_IDENT_SEGMENT_CRATE }
  | DOLLAR_CRATE {PATH_IDENT_SEGMENT_SCRATE}

path_genarg:      
  | PATHSEP generic_args { $2 }

generic_args:      
  | LT separated_or_terminated_list(COMMA, generic_arg) GT { GENERIC_ARGS $2 }

generic_arg:
  | lifetime              { GENERIC_ARG_LIFETIME (fst $1) }
  | typ                   {  GENERIC_ARG_TYPE $1 }
  | generic_args_const     { GENERIC_ARG_CONST $1 }
  | generic_args_binding   { GENERIC_ARGS_BINDING $1 }
  | generic_args_bounds    { GENERIC_ARGS_BOUNDS $1 }

generic_args_const:
  | block_expression             { GENERIC_ARGS_CONST_BLOCK $1 }
  | literal_expression           { GENERIC_ARGS_CONST_LIT $1 }
  | MINUS literal_expression     { NEG_GENERIC_ARGS_CONST_LIT $2 }
  | simple_path_segment          { GENERIC_ARGS_CONST_SIMPLE_PATH_SEG $1 }

generic_args_bounds:
  | ident option(generic_args) EQ type_param_bounds { GENERIC_ARGS_BOUNDS_ (fst $1, $2, $4) }

generic_args_binding:
  | ident option(generic_args) EQ typ { GENERIC_ARGS_BINDING_ (fst $1, $2, $4) }

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

type_path:  
  | option(PATHSEP) segments = separated_nonempty_list(PATHSEP, type_path_segment)
      { Cabs.TYP_PATH segments }

type_path_segment:
  | path_ident_segment type_path_seg_body { TYPE_PATH_SEGMENT ($1, $2) }

type_path_seg_body:
  | PATHSEP generic_args { SEG_BODY_GEN_ARGS $2 }
  | generic_args { SEG_BODY_GEN_ARGS $1 }
  | PATHSEP type_path_fn { SEG_BODY_PATH_FN $2 }
  | type_path_fn { SEG_BODY_PATH_FN $1 }
  | { SEG_BODY_EMPTY }

type_path_fn:
  | LPAREN option(type_path_fn_inputs) RPAREN option(rarrow_no_bounds) { TYPE_PATH_FN ($2, $4) }

rarrow_no_bounds:
  | RARROW type_no_bounds { $2 }

type_path_fn_inputs:
  | nonempty_separated_or_terminated_list(COMMA, typ) { TYPE_PATH_FN_INPUTS $1}

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
  | UNSAFE MOD name = ident LBRACE attrs = list(inner_attr) content = items RBRACE
    { MOD_DEC (true, (fst name), Stdlib.List.rev (attrs), content) }

safe_module:
  | MOD name = ident SEMI
    { MOD_BLOCK (false, (fst name)) }
  | MOD name = ident LBRACE attrs = list(inner_attr) content = items RBRACE
    { MOD_DEC (false, (fst name), Stdlib.List.rev (attrs), content) }

(*Modules*)

ident:
  | id = IDENT { (IDENT (fst id), snd id) }
  | raw = RAW_IDENT { (RAW_IDENT (fst raw), snd raw) }

outer_attr:
  | POUND LBRACK a = attr RBRACK { OUTER_ATTRIBUTE a }

inner_attr:
  | POUND NOT LBRACK a = attr RBRACK { INNER_ATTRIBUTE a }

attr:
  | path = simple_path input = maybe_attr_input { SAFE_ATTR (path, input) }
  | UNSAFE path = simple_path input = maybe_attr_input { UNSAFE_ATTR (path, input) }

maybe_attr_input:
  | a = attr_input { Some a }
  | /* empty */ { None }

attr_input:
  | EQ e = expression { ATTR_INPUT_EXP e }
(* ATTRIBUTE *)

