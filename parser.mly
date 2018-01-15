%{
open Ast
%}

%token <Ast.sym> LOWER_NAME UPPER_NAME
%token <int> INT
%token <string> STRING ERROR
%token EOF LET EQ REC AND PLUS TRUE FALSE COMMA LPAREN RPAREN EMPTY UNIT_VAL
%token AS COLON SINGLEQ UNDERSCORE ARROW COLONCOLON

%type <Ast.prog> ml
%type <Ast.exp> exp

%nonassoc AS
%left ALT
%nonassoc below_COMMA
%left COMMA
%nonassoc below_ARROW
%right ARROW
%nonassoc above_ARROW
%right COLONCOLON
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
| LOWER_NAME syms EQ exp { Function {sym = $1; args = $2; exp = $4} }

pattern:
  LOWER_NAME { Pattern.Name $1 }
| LPAREN pattern COLON type_exp RPAREN { Pattern.Typed {pattern=$2; typ=$4} }
| pattern AS LOWER_NAME { Pattern.As {pattern=$1; bound_var=$3} }
| patterns %prec below_COMMA { Pattern.Tuple (List.rev $1) }
| pattern_alt %prec below_COMMA { Pattern.Alt (List.rev $1) }
| LPAREN pattern RPAREN { $2 }
| UPPER_NAME pattern %prec below_COMMA { Pattern.Type_constr {constr=$1; body=$2} }
| const { Pattern.Const $1 }
| pattern COLONCOLON pattern { Pattern.Cons ($1, $3) }

patterns:
  patterns COMMA pattern { $3 :: $1 }
| pattern COMMA pattern { [$3; $1] }

pattern_alt:
  pattern_alt ALT pattern { $3 :: $1 }
| pattern ALT pattern { [$3; $1] }

type_exp:
  type_fun { Type_exp.Fun $1 }
| type_exp1 { $1 }

type_exp1:
  SINGLEQ name { Type_exp.Var $2 }
| UNDERSCORE { Type_exp.Anon }
| type_constr { Type_exp.Constr $1 }
| LPAREN type_exp RPAREN { $2 }

type_fun:
  type_fun ARROW type_exp1 { $3 :: $1 }
| type_exp1 ARROW type_exp1 { [$3; $1] }

type_constr:
  type_exp1 LOWER_NAME { Type_exp.{exp=$1; constr=$2} }

syms:
  rsyms { List.rev $1 }

rsyms:
  LOWER_NAME { [ $1 ] }
| syms LOWER_NAME { $2 :: $1 }

exp:
  exp PLUS exp { Plus ($1, $3) }
| const { Const $1 }

const:
  INT { Int $1 }
| TRUE { Bool true }
| FALSE { Bool false }
| STRING { String $1 }
| ERROR { Error $1 }

name:
  LOWER_NAME { $1 }
| UPPER_NAME { $1 }
