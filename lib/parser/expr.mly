
%%
%public
expr:
    | i = INT     { IntLit i }
    | i = FLOAT   { FloatLit i }
    | c = CHARLIT { CharLit c }
    | s = STRING  { StringLit s }
    | d = IDENT   { Ident d }
    | TRUE        { LTrue }
    | FALSE       { LFalse }
