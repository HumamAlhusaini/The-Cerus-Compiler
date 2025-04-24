
%%
%public
stmt:
    | LET; vt = var_and_typ; EQ; dec = expr; SEMI {
        let name, t = vt in
        Declaration ($startpos, name, t, dec)
    }
    | PRINT; LPAREN; s = STRING; RPAREN; SEMI; { Print ($startpos, s) }
