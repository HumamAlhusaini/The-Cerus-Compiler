
%%
%public
stmt:
    | LET; vt = var_and_typ; dec = has_expr; {
        let name, t = vt in
        Let ($startpos, name, t, dec)
    }
    | PRINT; LPAREN; s = STRING; RPAREN; SEMI { Print ($startpos, s) }
