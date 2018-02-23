type sym = {n : string; lnum : int}

module Type_exp = struct
  type t =
    | Anon
    | Constr of {exps:t list; constr:sym}
    | Var of sym
    | Fun of t list
    | Tuple of t list
    | Record of (sym * t) list
    | Variant of name list
  and binding = {sym:sym; params:sym list; body:t}
  and name =
    | Untyped of sym
    | Typed of sym * t
end

module Const = struct
  type t =
    | Bool of bool
    | Char of char
    | Error of string
    | Float of float
    | Int of int
    | String of string
    | Unit
end

module Pattern = struct
  type t =
    | Alt of t list
    | As of {pattern:t; bound_var:sym}
    | Cons of t * t
    | Const of Const.t
    | Name of sym
    | Tuple of t list
    | Type_constr of {constr:sym; body:t}
    | Typed of {pattern:t; typ:Type_exp.t}
end

module Exp = struct
  type t =
    | App of app
    | Arr of t list
    | Asr of binary_op
    | Assign of binary_op
    | Cons of binary_op
    | Const of Const.t
    | Divide of binary_op
    | Equals of binary_op
    | Fun of Type_exp.name list binding
    | Greater of binary_op
    | Greater_eq of binary_op
    | If of {ante:t; cons:t; alt:t}
    | Land of binary_op
    | Less of binary_op
    | Less_eq of binary_op
    | Let of definitions binding
    | List of t list
    | Lnot of t
    | Lor of binary_op
    | Lsl of binary_op
    | Lsr of binary_op
    | Lxor of binary_op
    | Match of {exp:t; pattern_matches:Pattern.t binding list}
    | Minus of binary_op
    | Mod of binary_op
    | Multiply of binary_op
    | Neg of t
    | Not of t
    | Not_eq of binary_op
    | Or of binary_op
    | Pattern_fun of Pattern.t binding list
    | Plus of binary_op
    | Record of field list
    | Sequence of t list
    | Tuple of t list
    | Type_constr of {constr:sym; exp:t}
    | Typed of t * Type_exp.t
    | Unit
    | Var of sym
  and definition =
    | Function_def of sym * Type_exp.name list binding
    | Value_def of Pattern.t binding
  and definitions =
    | Let_binding of definition list
    | Rec_binding of definition list
  and binary_op = {lhs:t; rhs:t}
  and app = {fn:t; args:t list}
  and field = {field:sym; typ:Type_exp.t option; exp:t}
  and 'a binding = {binds:'a; body:t}
end

type top =
  | Exp of Exp.definitions
  | Type of Type_exp.binding list

type prog = top list

let dump_e e = ()
let dump_t t = ()

let dump prog =
  List.iter (function Exp e -> dump_e e | Type t -> dump_t t) prog
