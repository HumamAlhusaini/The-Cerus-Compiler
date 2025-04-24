
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
typ:
    | lit = lit_type { TLit lit }
    | id = IDENT    { TCustom id }

%public
var_and_typ:
  | id = IDENT; COLON; t = typ { (id, t) }
