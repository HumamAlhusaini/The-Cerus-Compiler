
%%

%public
stmt:
    | LET; mut = opt_mut; name = IDENT; COLON; t = typ; dec = has_expr; {
        Let ($startpos, mut,  name, t, dec)
    }
    | PRINT; LPAREN; s = STRING; RPAREN; SEMI { Print ($startpos, s) }
