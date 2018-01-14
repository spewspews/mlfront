%{
open Ast
%}

%token <Ast.sym> VALUE_NAME CONSTR_NAME
%token <int> INT
%token <string> STRING ERROR
%token EOF LET EQ REC AND PLUS TRUE FALSE COMMA LPAREN RPAREN EMPTY UNIT_VAL

%type <Ast.prog> ml
%type <Ast.exp> exp

%nonassoc below_COMMA
%left COMMA
%nonassoc above_COMMA
%left PLUS

%start ml

%%

ml:
  top_bindings EOF { List.rev $1 }

top_bindings:
   { [] }
|  top_bindings top_mutual_binding { $2 :: $1 }

top_mutual_binding:
  top_binding_head top_binding_tail {
    let (b, r) = $1 in
    {binds = b::$2; is_rec = r}
  }

top_binding_head:
  LET binding { ($2, false) }
| LET REC binding { ($3, true) }

top_binding_tail:
  { [] }
| top_binding_tail AND binding { $3 :: $1 }

binding:
  pattern EQ exp { Value {bound = $1; exp = $3} }
| VALUE_NAME syms EQ exp { Function {sym = $1; args = $2; exp = $4} }

pattern:
  VALUE_NAME { Name $1 }
| tuple_pattern %prec below_COMMA { Tuple (List.rev $1) }
| LPAREN pattern RPAREN { $2 }
| CONSTR_NAME pattern %prec above_COMMA {
    Type {constr=$1; body=$2}
  }

tuple_pattern:
  tuple_pattern COMMA pattern { $3 :: $1 }
| pattern COMMA pattern { [$3; $1] }

syms:
  rsyms { List.rev $1 }

rsyms:
  VALUE_NAME { [ $1 ] }
| syms VALUE_NAME { $2 :: $1 }

exp:
  exp PLUS exp { Plus ($1, $3) }
| INT { Int $1 }
| TRUE { Bool true }
| FALSE { Bool false }
| STRING { String $1 }
| ERROR { Error $1 }
