
%token <string> IDENT STRING // Identifiers and string literals
%token <int> INT             // Integer literals
%token <float> FLOAT           // Floating-point literals
%token <char> CHARLIT         // Character literals
%token TRUE FALSE            // Boolean literals

%token FN LET MUT RETURN STRUCT ENUM IF ELSE WHILE FOR LOOP MATCH IMPL TRAIT CONST STATIC
                               // Keywords related to function definition, variable binding, control flow, data structures, etc.

%token USE PUB MOD TYPE AS EXTERN CRATE MOVE REF SELF SUPER BOOL I32 U32 F32 F64 CHAR STR
                               // Keywords related to module system, type aliases, external code, memory management, and primitive types

%token PLUS MINUS STAR SLASH PERCENT EQ EQEQ NE LT LE GT GE ANDAND OROR NOT
                               // Arithmetic, comparison, and logical operators

%token DOT DOTDOT DOTDOTDOT COMMA SEMI COLON ARROW FAT_ARROW AMP AMPMUT
                               // Punctuation and symbols used in syntax

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PIPE UNDERSCORE PRINT
                               // Delimiters and other special symbols

%token EOF                     // End of file marker

%%
