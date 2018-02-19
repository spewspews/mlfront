%{
open Ast
%}

%token <Ast.sym> LOWER_NAME UPPER_NAME
%token <int> INT
%token <float> FLOAT
%token <string> STRING ERROR
%token EOF LET EQ REC AND PLUS TRUE FALSE COMMA LPAREN RPAREN EMPTY UNIT_VAL
%token AS COLON SINGLEQ UNDERSCORE ARROW COLONCOLON MUL BEGIN END
%token MATCH WITH IN FUN FUNCTION SEMICOLON IF PLUS LESS GREATER
%token LESSEQ GREATEREQ MINUS NOTEQ DIV NOT LNOT LAND LOR LSL LSR
%token ASR LBRACE RBRACE LBRACK RBRACK LBRACKARR RBRACKARR TYPE OF

%type <Ast.prog> prog

%nonassoc AS
%left SEMICOLON
%left ALT
%right THEN ELSE
%right ASSIGN
%left COMMA
%right OR
%right AND
%left ARROW
%nonassoc LESS GREATER LESSEQ GREATEREQ NOTEQ EQ
%right COLONCOLON
%left PLUS MINUS
%left MUL DIV MOD LAND LOR LXOR LNOT
%right LSL LSR ASR
%nonassoc UNARY

%start prog

%%

prog:
  | prog1 { List.rev $1 }

prog1:
  | { [] }
  | top_binding prog1 { Exp $1 :: $2 }
  | top_type_binding prog1 { Type $1 :: $2 }

top_binding:
  | binding_head binding_tail
    {
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
  | pattern EQ exp { Exp.(Value_def {binds = $1; body = $3}) }
  | LOWER_NAME parameters EQ exp { Exp.(Function_def ($1, {binds = $2; body = $4})) }

pattern:
  | pattern AS LOWER_NAME { Pattern.As {pattern=$1; bound_var=$3} }
  | pattern_tuple { Pattern.Tuple (List.rev $1) }
  | pattern_alt { Pattern.Alt (List.rev $1) }
  | pattern COLONCOLON pattern { Pattern.Cons ($1, $3) }
  | pattern1 { $1 }

pattern_alt:
  | pattern_alt ALT pattern1 { $3 :: $1 }
  | pattern1 ALT pattern1 { [$3; $1] }

pattern_tuple:
  | pattern_tuple COMMA pattern1 { $3 :: $1 }
  | pattern1 COMMA pattern1 { [$3; $1] }

pattern1:
  | UPPER_NAME pattern1 { Pattern.Type_constr {constr=$1; body=$2} }
  | const { Pattern.Const $1 }
  | LOWER_NAME { Pattern.Name $1 }
  | LPAREN pattern COLON type_exp RPAREN { Pattern.Typed {pattern=$2; typ=$4} }
  | LPAREN pattern RPAREN { $2 }

type_exp:
  | type_exp_fun { Type_exp.Fun (List.rev $1) }
  | type_exp1 { $1 }

type_exp_fun:
  | type_exp_fun ARROW type_exp1 { $3 :: $1 }
  | type_exp1 ARROW type_exp1 { [$3; $1] }

type_exp1:
  | type_exp_tuple { Type_exp.Tuple (List.rev $1) }
  | type_exp2 { $1 }

type_exp_tuple:
  | type_exp_tuple MUL type_exp2 { $3 :: $1 }
  | type_exp2 MUL type_exp2 { [$3; $1] }

type_exp2:
  | SINGLEQ name { Type_exp.Var $2 }
  | UNDERSCORE { Type_exp.Anon }
  | LPAREN type_exp RPAREN { $2 }
  | type_constr { $1 }

type_constr:
  | LOWER_NAME { Type_exp.Constr {constr=$1; exps=[]} }
  | type_exp2 LOWER_NAME { Type_exp.Constr {constr=$2; exps=[$1]} }
  | LPAREN type_exps RPAREN LOWER_NAME { Type_exp.Constr {constr=$4; exps=(List.rev $2)} }

type_exps:
  | type_exps COMMA type_exp { $3 :: $1 }
  | type_exp COMMA type_exp { [$3; $1] }

parameters: parameters1 { List.rev $1 }

parameters1:
  | parameter { [$1] }
  | parameters1 parameter { $2 :: $1 }

parameter:
  | LOWER_NAME { Type_exp.Untyped $1 }
  | LPAREN LOWER_NAME COLON type_exp RPAREN { Type_exp.Typed ($2, $4) }

exp:
  | binding_head binding_tail IN exp
    {
      let (b, recur) = $1 in
      let binds = b::$2 in
      let binds = if recur then Exp.Rec_binding binds else Exp.Let_binding binds in
      Exp.(Let {binds; body=$4})
    }
  | MATCH exp WITH pattern_matching { Exp.Match {exp=$2; pattern_matches=$4} }
  | FUN parameters ARROW exp { Exp.(Fun {binds=$2; body=$4}) }
  | FUNCTION pattern_matching { Exp.Pattern_fun $2 }
  | exp_sequence { Exp.Sequence (List.rev $1) }
  | exp1 { $1 }

exp_sequence:
  | exp1 SEMICOLON { [$1] }
  | exp_sequence SEMICOLON exp1 { $3 :: $1 }
  | exp_sequence SEMICOLON exp1 SEMICOLON { $3 :: $1 }

pattern_matching:
  | ALT pattern_matching1 { List.rev $2 }
  | pattern_matching1 { List.rev $1 }

pattern_matching1:
  | pattern_match { [$1] }
  | pattern_matching1 ALT pattern_match { $3 :: $1 }

pattern_match: pattern ARROW exp1 { Exp.{binds=$1; body=$3} }

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
  | LNOT exp2 %prec UNARY { Exp.Lnot $2 }
  | MINUS exp2 %prec UNARY { Exp.Neg $2 }
  | NOT exp2 %prec UNARY { Exp.Not $2 }
  | exp2 ASR exp2 { Exp.(Asr {lhs=$1; rhs=$3}) }
  | exp2 COLONCOLON exp2 { Exp.(Cons {lhs=$1; rhs=$3}) }
  | exp2 DIV exp2 { Exp.(Divide {lhs=$1; rhs=$3}) }
  | exp2 EQ exp2 { Exp.(Equals {lhs=$1; rhs=$3}) }
  | exp2 GREATER exp2 { Exp.(Greater {lhs=$1; rhs=$3}) }
  | exp2 GREATEREQ exp2 { Exp.(Greater_eq {lhs=$1; rhs=$3}) }
  | exp2 LAND exp2 { Exp.(Land {lhs=$1; rhs=$3}) }
  | exp2 LESS exp2 { Exp.(Less {lhs=$1; rhs=$3}) }
  | exp2 LESSEQ exp2 { Exp.(Less_eq {lhs=$1; rhs=$3}) }
  | exp2 LOR exp2 { Exp.(Lor {lhs=$1; rhs=$3}) }
  | exp2 LSL exp2 { Exp.(Lsl {lhs=$1; rhs=$3}) }
  | exp2 LSR exp2 { Exp.(Lsr {lhs=$1; rhs=$3}) }
  | exp2 LXOR exp2 { Exp.(Lxor {lhs=$1; rhs=$3}) }
  | exp2 MINUS exp2 { Exp.(Minus {lhs=$1; rhs=$3}) }
  | exp2 MOD exp2 { Exp.(Mod {lhs=$1; rhs=$3}) }
  | exp2 MUL exp2 { Exp.(Multiply {lhs=$1; rhs=$3}) }
  | exp2 NOTEQ exp2 { Exp.(Not_eq {lhs=$1; rhs=$3}) }
  | exp2 OR exp2 { Exp.(Or {lhs=$1; rhs=$3}) }
  | exp2 PLUS exp2 { Exp.(Plus {lhs=$1; rhs=$3}) }
  | exp3 { $1 }
  | exp_app { Exp.App $1 }
  | LBRACK exp_sequence RBRACK { Exp.List (List.rev $2) }
  | LBRACKARR exp_sequence RBRACKARR { Exp.Arr (List.rev $2) }

exp_app: exp_app1 { Exp.(let {fn; args} = $1 in {fn; args=List.rev args}) }

exp_app1:
  | exp3 exp3 { Exp.{fn=$1; args=[$2]} }
  | exp_app1 exp3 { Exp.(let {fn; args} = $1 in {fn; args=$2::args}) }

exp3:
  | UPPER_NAME exp3 { Exp.Type_constr {constr=$1; exp=$2} }
  | LPAREN exp COLON type_exp RPAREN { Exp.Typed ($2, $4) }
  | LPAREN exp RPAREN { $2 }
  | BEGIN exp END { $2 }
  | const { Exp.Const $1 }
  | LOWER_NAME { Exp.Var $1 }
  | LBRACE fields RBRACE { Exp.Record $2 }

fields:
  | fields1 { List.rev $1 }
  | fields1 SEMICOLON { List.rev $1 }

fields1:
  | field { [$1] }
  | fields1 SEMICOLON field { $3 :: $1 }

field:
  | LOWER_NAME field_type { Exp.{field=$1; typ=$2; exp=Exp.Var $1} }
  | LOWER_NAME field_type EQ exp1 { Exp.{field=$1; typ=$2; exp=$4} }

field_type:
  | { None }
  | COLON type_exp { Some $2 }

const:
  | INT { Const.Int $1 }
  | FLOAT { Const.Float $1 }
  | TRUE { Const.Bool true }
  | FALSE { Const.Bool false }
  | UNIT_VAL { Const.Unit }
  | STRING { Const.String $1 }
  | ERROR { Const.Error $1 }

top_type_binding:
  | TYPE type_bindings { List.rev $2 }

type_bindings:
  | type_binding { [$1] }
  | type_bindings AND type_binding { $3 :: $1 }

type_binding:
  | type_param LOWER_NAME EQ type_exp { Type_exp.{sym=$2; params=$1; body=$4} }
  | type_param LOWER_NAME EQ LBRACE type_fields RBRACE { Type_exp.{sym=$2; params=$1; body=Record $5} }
  | type_param LOWER_NAME EQ type_variants { Type_exp.{sym=$2; params=$1; body=Variant $4} }

type_param:
  | { [] }
  | SINGLEQ LOWER_NAME { [$2] }
  | LPAREN type_params RPAREN { $2 }

type_params:
  | SINGLEQ LOWER_NAME { [$2] }
  | type_params COMMA SINGLEQ LOWER_NAME { $4 :: $1 }

type_fields:
  | type_fields1 { $1 }
  | type_fields1 SEMICOLON { $1 }

type_fields1:
  | type_field { [$1] }
  | type_fields1 SEMICOLON type_field { $3 :: $1 }

type_field: LOWER_NAME COLON type_exp { $1, $3 }

type_variants:
  | type_variant { [$1] }
  | ALT type_variant { [$2] }
  | type_variants ALT type_variant { $3 :: $1 }

type_variant:
  | UPPER_NAME { Type_exp.Untyped $1 }
  | UPPER_NAME OF type_exp { Type_exp.Typed ($1, $3) }

name:
  | LOWER_NAME { $1 }
  | UPPER_NAME { $1 }
