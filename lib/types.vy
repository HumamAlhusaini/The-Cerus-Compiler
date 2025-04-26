
%{
open Ast
%}

%%
lit_type:
    | I32        { TInt32 }
    | U32        { TUInt32 }
    | F32        { TFloat32 }
    | F64        { TFloat64 }
    | CHAR       { TChar }
    | BOOL       { Bool }

%public
simpl_typ:
  | base = lit_type                { TLit base }
  | AMP t = simpl_typ              { TRef (false, t) }
  | AMPMUT t = simpl_typ           { TRef (true, t) }
  | LPAREN; s = separated_list(COMMA, typ); RPAREN;       { TParen s }

%public 
typ:
  | s = simpl_typ { Typ s  }
  | s1 = simpl_typ; ARROW; s2 = simpl_typ { TArrow (s1, s2)}

%public
var_and_typ:
  | id = IDENT; COLON; t = typ { (id, t) }


