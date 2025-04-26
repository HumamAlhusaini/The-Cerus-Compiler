
%%

%public
stmt:
    | LET; mut = opt_mut; name = IDENT; COLON; t = typ; dec = has_expr; {
         let n = Var_name.of_string name in
        Let ($startpos, mut,  n, t, dec)
    }
    | PRINT; LPAREN; s = STRING; RPAREN; SEMI { Print ($startpos, s) }
