
%token <string>  IDENT    STRING
%token <int>     INT
%token <float>   FLOAT
%token <char>    CHARLIT
%token           TRUE     FALSE 

(* Keywords *)
%token           FN       LET       MUT      RETURN    STRUCT    ENUM
%token           IF       ELSE      WHILE    FOR       LOOP      MATCH
%token           IMPL     TRAIT     CONST    STATIC

(* Primitive Types *)
%token           BOOL     I32       U32      F32       F64       CHAR     STR

(* Operators *)
%token           PLUS     MINUS     STAR     SLASH     PERCENT   EQ       EQEQ     NE
%token           LT       LE        GT       GE        ANDAND    OROR     NOT

(* Punctuation *)
%token           DOT      DOTDOT    DOTDOTDOT COMMA     SEMI      COLON    ARROW    FAT_ARROW
%token           AMP      AMPMUT    LPAREN    RPAREN    LBRACE    RBRACE   LBRACK   RBRACK
%token           PIPE     UNDERSCORE PRINT    EOF
