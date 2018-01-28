%{
open Ast
%}

%token <Ast.sym> LOWER_NAME UPPER_NAME
%token <int> INT
%token <float> FLOAT
%token <string> STRING ERROR
%token EOF LET EQ REC AND PLUS TRUE FALSE COMMA LPAREN RPAREN EMPTY UNIT_VAL
%token AS COLON SINGLEQ UNDERSCORE ARROW COLONCOLON ASTERISK BEGIN END

%type <Ast.prog> prog

%nonassoc AS
%left ALT
%nonassoc below_COMMA
%left COMMA
%right ARROW
%right COLONCOLON
%left PLUS

%start prog

%%

prog:
| top_bindings EOF { List.rev $1 }

top_bindings:
|  { [] }
|  top_bindings top_mutual_binding { $2 :: $1 }

top_mutual_binding:
| top_binding_head top_binding_tail {
    let (b, recur) = $1 in
    let binds = b::$2 in
    if recur then Rec_bind binds else Let_bind binds
  }

top_binding_head:
| LET binding { ($2, false) }
| LET REC binding { ($3, true) }

top_binding_tail:
| { [] }
| top_binding_tail AND binding { $3 :: $1 }

binding:
| pattern EQ exp { Value {bound = $1; exp = $3} }
| LOWER_NAME syms EQ exp { Function {sym = $1; args = $2; exp = $4} }

pattern:
| LOWER_NAME { Pattern.Name $1 }
| LPAREN pattern COLON type_exp RPAREN { Pattern.Typed {pattern=$2; typ=$4} }
| pattern AS LOWER_NAME { Pattern.As {pattern=$1; bound_var=$3} }
| patterns %prec below_COMMA { Pattern.Tuple (List.rev $1) }
| pattern_alt %prec below_COMMA { Pattern.Alt (List.rev $1) }
| LPAREN pattern RPAREN { $2 }
| UPPER_NAME pattern %prec below_COMMA { Pattern.Type_constr {constr=$1; body=$2} }
| const { Pattern.Const $1 }
| pattern COLONCOLON pattern { Pattern.Cons ($1, $3) }

patterns:
| patterns COMMA pattern { $3 :: $1 }
| pattern COMMA pattern { [$3; $1] }

pattern_alt:
| pattern_alt ALT pattern { $3 :: $1 }
| pattern ALT pattern { [$3; $1] }

type_exp:
| type_fun { Type_exp.Fun (List.rev $1) }
| type_tuple { Type_exp.Tuple (List.rev $1) }
| type_exp1 { $1 }

type_exp1:
| SINGLEQ name { Type_exp.Var $2 }
| UNDERSCORE { Type_exp.Anon }
| LPAREN type_exp RPAREN { $2 }
| type_constr { $1 }

type_fun:
| type_fun ARROW type_exp1 { $3 :: $1 }
| type_exp1 ARROW type_exp1 { [$3; $1] }

type_tuple:
| type_tuple ASTERISK type_exp1 { $3 :: $1 }
| type_exp1 ASTERISK type_exp1 { [$3; $1] }

type_constr:
| type_exp1 LOWER_NAME { Type_exp.Constr {exps=[$1]; constr=$2} }
| LPAREN type_exps RPAREN LOWER_NAME { Type_exp.Constr {exps=(List.rev $2); constr=$4} }

type_exps:
| type_exps COMMA type_exp { $3 :: $1 }
| type_exp COMMA type_exp { [$3; $1] }

syms:
| rsyms { List.rev $1 }

rsyms:
| LOWER_NAME { [ $1 ] }
| rsyms LOWER_NAME { $2 :: $1 }

exp:
| LOWER_NAME { Exp.Var $1 }
| const { Exp.Const $1 }
| BEGIN exp END { $2 }
| LPAREN exp COLON type_exp RPAREN { Exp.Typed ($2, $4) }
| exp PLUS exp { Exp.Plus ($1, $3) }

const:
| INT { Int $1 }
| FLOAT { Float $1 }
| TRUE { Bool true }
| FALSE { Bool false }
| STRING { String $1 }
| ERROR { Error $1 }

name:
| LOWER_NAME { $1 }
| UPPER_NAME { $1 }
