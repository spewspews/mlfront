%token <Ast.sym> SYM
%token EOF LET LEQ FOO REC

%type <Ast.binding list> ml
%type <unit> exp
%start ml

%%

ml:
  top_bindings { List.rev $1 }

top_bindings:
|  top_bindings top_binding { $2 :: $1 }

top_binding:
  LET SYM LEQ exp { Ast.{sym = $2; exp = $4; is_rec = false} }
| LET REC SYM LEQ exp { Ast.{sym = $3; exp = $5; is_rec = true} }

exp:
  FOO { () }
