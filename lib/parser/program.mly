

%start program
%type <program> program

%%

program:
  item_list; EOF { $1 }

item_list:
  /* empty */ { [] }
| e = item; rest = item_list { e :: rest }
