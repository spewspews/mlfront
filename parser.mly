%{
open Ast
%}

%token <Ast.sym> LOWER_NAME UPPER_NAME
%token <int> INT
%token <float> FLOAT
%token <string> STRING ERROR
%token EOF LET EQ REC AND PLUS TRUE FALSE COMMA LPAREN RPAREN EMPTY UNIT_VAL
%token AS COLON SINGLEQ UNDERSCORE ARROW COLONCOLON MUL BEGIN END
%token MATCH WITH IN FUN FUNCTION SEMICOLON IF PLUS EQEQ LESS GREATER
%token LESSEQ GREATEREQ MINUS NOTEQ DIV NOT LNOT

%type <Ast.prog> prog

%nonassoc AS
%left SEMICOLON
%nonassoc below_ALT
%right ALT
%right THEN ELSE
%right ASSIGN
%nonassoc below_COMMA
%left COMMA
%right OR
%right AND
%left ARROW
%nonassoc LESS GREATER LESSEQ GREATEREQ NOTEQ EQ
%right COLONCOLON
%left PLUS MINUS
%left MUL DIV MOD LAND LOR LXOR LNOT
%right LSL LSR ASR

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
    if recur then Exp.Rec_binding binds else Exp.Let_binding binds
  }

binding_head:
| LET binding { ($2, false) }
| LET REC binding { ($3, true) }

binding_tail:
| { [] }
| binding_tail AND binding { $3 :: $1 }

binding:
| pattern EQ exp { Exp.Value {bound = $1; exp = $3} }
| LOWER_NAME parameters EQ exp { Exp.Function {sym = $1; parameters = $2; body = $4} }

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
| type_exp_tuple MUL type_exp1 { $3 :: $1 }
| type_exp1 MUL type_exp1 { [$3; $1] }

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
| LOWER_NAME { Parameter.Untyped $1 }
| LPAREN LOWER_NAME COLON type_exp RPAREN { Parameter.Typed {parameter=$2; typ=$4} }

exp:
| binding_head binding_tail IN exp {
    let (b, recur) = $1 in
    let binds = b::$2 in
    let binding = if recur then Exp.Rec_binding binds else Exp.Let_binding binds in
    Exp.Let {binding; body=$4}
  }
| MATCH exp WITH pattern_matching { Exp.Match {exp=$2; pattern_matches=$4} }
| FUN parameters ARROW exp { Exp.Fun {parameters=$2; body=$4} }
| FUNCTION pattern_matching { Exp.Pattern_fun $2 }
| exp_sequence { Exp.Sequence (List.rev $1) }
| exp1 { $1 }

exp_sequence:
| exp1 SEMICOLON { [$1] }
| exp_sequence SEMICOLON exp_sequence { $3 @ $1 }
| exp_sequence SEMICOLON exp1 { $3 :: $1 }

pattern_matching:
| ALT pattern_matchings { List.rev $2 }
| pattern_matchings %prec below_ALT { List.rev $1 }

pattern_matchings:
| pattern_match { [$1] }
| pattern_matchings ALT pattern_match { $3 :: $1 }

pattern_match: pattern ARROW exp { Exp.{pattern=$1; body=$3} }

exp1:
| IF exp1 THEN exp1 ELSE exp1 { Exp.If {ante=$2; cons=$4; alt=$6} }
| IF exp1 THEN exp1 { Exp.If {ante=$2; cons=$4; alt=Exp.Const Const.Unit} }
| exp1 ASSIGN exp1 { Exp.Assign Exp.{lhs=$1; rhs=$3} }
| tuple_exp { Exp.Tuple (List.rev $1) }
| exp2 { $1 }

tuple_exp:
| exp2 COMMA exp2 { [$3; $1] }
| tuple_exp COMMA exp2 { $3 :: $1 }

exp2:
| LNOT exp3 { Exp.Lnot $2 }
| NOT exp3 { Exp.Not $2 }
| exp3 ASR exp3 { Exp.Asr Exp.{lhs=$1; rhs=$3} }
| exp3 DIV exp3 { Exp.Divide Exp.{lhs=$1; rhs=$3} }
| exp3 EQ exp3 { Exp.Equals Exp.{lhs=$1; rhs=$3} }
| exp3 GREATER exp3 { Exp.Greater Exp.{lhs=$1; rhs=$3} }
| exp3 GREATEREQ exp3 { Exp.Greater_eq Exp.{lhs=$1; rhs=$3} }
| exp3 LAND exp3 { Exp.Land Exp.{lhs=$1; rhs=$3} }
| exp3 LESS exp3 { Exp.Less Exp.{lhs=$1; rhs=$3} }
| exp3 LESSEQ exp3 { Exp.Less_eq Exp.{lhs=$1; rhs=$3} }
| exp3 LOR exp3 { Exp.Lor Exp.{lhs=$1; rhs=$3} }
| exp3 LSL exp3 { Exp.Lsl Exp.{lhs=$1; rhs=$3} }
| exp3 LSR exp3 { Exp.Lsr Exp.{lhs=$1; rhs=$3} }
| exp3 LXOR exp3 { Exp.Lxor Exp.{lhs=$1; rhs=$3} }
| exp3 MINUS exp3 { Exp.Minus Exp.{lhs=$1; rhs=$3} }
| exp3 MOD exp3 { Exp.Mod Exp.{lhs=$1; rhs=$3} }
| exp3 MUL exp3 { Exp.Multiply Exp.{lhs=$1; rhs=$3} }
| exp3 NOTEQ exp3 { Exp.Not_eq Exp.{lhs=$1; rhs=$3} }
| exp3 OR exp3 { Exp.Or Exp.{lhs=$1; rhs=$3} }
| exp3 PLUS exp3 { Exp.Plus Exp.{lhs=$1; rhs=$3} }
| exp3 { $1 }

exp3:
| UPPER_NAME exp3 { Exp.Type_constr {constr=$1; exp=$2} }
| LPAREN exp COLON type_exp RPAREN { Exp.Typed ($2, $4) }
| LPAREN exp RPAREN { $2 }
| BEGIN exp END { $2 }
| const { Exp.Const $1 }

const:
| INT { Const.Int $1 }
| FLOAT { Const.Float $1 }
| TRUE { Const.Bool true }
| FALSE { Const.Bool false }
| UNIT_VAL { Const.Unit }
| STRING { Const.String $1 }
| ERROR { Const.Error $1 }

name:
| LOWER_NAME { $1 }
| UPPER_NAME { $1 }
