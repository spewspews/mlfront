%{
open Ast
%}

%token <Ast.sym> LOWER_NAME UPPER_NAME
%token <int> INT
%token <float> FLOAT
%token <string> STRING ERROR
%token EOF LET EQ REC AND PLUS TRUE FALSE COMMA LPAREN RPAREN EMPTY UNIT_VAL
%token AS COLON SINGLEQ UNDERSCORE ARROW COLONCOLON ASTERISK BEGIN END
%token MATCH WITH IN

%type <Ast.prog> prog

%nonassoc AS
%left ALT
%right THEN ELSE
%right ASSIGN
%nonassoc below_COMMA
%left COMMA
%left ARROW
%right COLONCOLON
%left PLUS

%start prog

%%

prog:
| top_bindings EOF { List.rev $1 }

top_bindings:
|  { [] }
|  top_bindings top_binding { $2 :: $1 }

top_binding:
| binding_head binding_tail {
    let (b, recur) = $1 in
    let binds = b::$2 in
    if recur then Rec_bind binds else Let_bind binds
  }

binding_head:
| LET binding { ($2, false) }
| LET REC binding { ($3, true) }

binding_tail:
| { [] }
| binding_tail AND binding { $3 :: $1 }

binding:
| pattern EQ exp { Value {bound = $1; exp = $3} }
| LOWER_NAME parameters EQ exp { Function {sym = $1; args = $2; body = $4} }

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
| type_exp_fun { Type_exp.Fun (List.rev $1) }
| type_exp_tuple { Type_exp.Tuple (List.rev $1) }
| type_exp1 { $1 }

type_exp1:
| SINGLEQ name { Type_exp.Var $2 }
| UNDERSCORE { Type_exp.Anon }
| LPAREN type_exp RPAREN { $2 }
| type_constr { $1 }

type_exp_fun:
| type_exp_fun ARROW type_exp1 { $3 :: $1 }
| type_exp1 ARROW type_exp1 { [$3; $1] }

type_exp_tuple:
| type_exp_tuple ASTERISK type_exp1 { $3 :: $1 }
| type_exp1 ASTERISK type_exp1 { [$3; $1] }

type_constr:
| type_exp1 LOWER_NAME { Type_exp.Constr {exps=[$1]; constr=$2} }
| LPAREN type_exps RPAREN LOWER_NAME { Type_exp.Constr {exps=(List.rev $2); constr=$4} }

type_exps:
| type_exps COMMA type_exp { $3 :: $1 }
| type_exp COMMA type_exp { [$3; $1] }

parameters: rparameters { List.rev $1 }

rparameters:
| parameter { [$1] }
| rparameters parameter { $2 :: $1 }

parameter:
| LOWER_NAME { Untyped_parameter $1 }
| LPAREN LOWER_NAME COLON type_exp { Typed_parameter {parameter=$2; typ=Some $4} }

syms: rsyms { List.rev $1 }

rsyms:
| LOWER_NAME { [ $1 ] }
| rsyms LOWER_NAME { $2 :: $1 }

exp:
| binding_head binding_tail IN exp {
    let (b, recur) = $1 in
    let binds = b::$2 in
    let binding = if recur then Rec_bind binds else Let_bind binds in
    Exp.Let {binding; body=$4}
  }
| MATCH exp WITH pattern_matching { Exp.Match {exp=$2; patterns=$4} }
| FUN parameters ARROW exp { Exp.Fun {parameters=$2; body=$4} }
| FUNCTION pattern_matching { Exp.Function $2 }
| exp_sequence { Exp.Sequence (List.rev $1) }
| exp1 { $1 }

exp_sequence:
| exp1 { [$1] }
| exp1 SEMICOLON { [$1] }
| exp_sequence SEMICOLON exp1 { $3 :: $1 }

pattern_matching:
| ALT pattern_match pattern_matching_tail { $2 :: (List.rev $3) }
| pattern pattern_matching_tail { $1 :: (List.rev $2) }

pattern_matching_tail:
| { [] }
| pattern_matching_tail ALT pattern_match { $3 :: $1 }

pattern_match: pattern ARROW exp { Exp.{pattern=$1; body=$3} }

exp1:
| IF exp1 THEN exp1 ELSE exp1 { Exp.If {ante=$2; cons=$4; alt=$6} }
| IF exp1 THEN exp1 { Exp.If {ante=$2; cons=$4; alt=Exp.Const Exp.Unit} }
| exp1 ASSIGN exp1 { Exp.Assign {lhs=$1; rhs=$3} }
| tuple_exp { Exp.Tuple (List.rev $1) }
| exp2 { $1 }

tuple_exp:
| exp2 COMMA exp2 { [$3; $1] }
| tuple_exp COMMA exp2 { $3 :: $1 }

exp2:
| LPAREN exp RPAREN { $2 }
| BEGIN exp END { $2 }
| const { Exp.Const $1 }


const:
| INT { Exp.Int $1 }
| FLOAT { Exp.Float $1 }
| TRUE { Exp.Bool true }
| FALSE { Exp.Bool false }
| UNIT_VAL { Exp.Unit }
| STRING { Exp.String $1 }
| ERROR { Exp.Error $1 }

name:
| LOWER_NAME { $1 }
| UPPER_NAME { $1 }
