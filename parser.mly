%{
module A = Ast
%}

%token <Ast.sym> SYM
%token <int> INT
%token EOF LET EQ FOO REC AND PLUS

%type <Ast.mutual_bind list> ml
%type <Ast.exp> exp

%left PLUS

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
    A.{binds = b::$2; is_rec = r}
  }

top_binding_head:
  LET binding { ($2, false) }
| LET REC binding { ($3, true) }

top_binding_tail:
  { [] }
| top_binding_tail AND binding { $3 :: $1 }

binding:
  SYM symz EQ exp { A.{sym = $1; args = $2; exp = $4} }

symz:
  { [] }
| syms { List.rev $1 }

syms:
  SYM { [ $1 ] }
| syms SYM { $2 :: $1 }

exp:
  exp PLUS exp { A.Plus ($1, $3) }
| INT { A.Int $1 }
