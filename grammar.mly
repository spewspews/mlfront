%token <Ast.sym> SYM
%token EOF LET LEQ FOO REC AND

%type <Ast.mutual_bind list> ml
%type <unit> exp
%start ml

%%

ml:
  top_bindings { List.rev $1 }

top_bindings:
   { [] }
|  top_bindings top_mutual_binding { $2 :: $1 }

top_mutual_binding:
  top_binding_head top_binding_tail
  {
    let (b, r) = $1 in
    Ast.{binds=b::$2; is_rec = r}
  }

top_binding_head:
  LET binding { ($2, false) }
| LET REC binding { ($3, true) }

top_binding_tail:
  { [] }
| top_binding_tail AND binding { $3 :: $1 }

binding:
  SYM LEQ exp { Ast.{sym = $1; exp = $3} }

exp:
  FOO { () }
