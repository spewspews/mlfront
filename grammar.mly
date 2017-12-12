%token <Ast.sym> SYM
%token EOF

%start <Ast.t> ml

%%

ml:
  SYM { Ast.SYM $1 }
