type sym = {n : string; lnum : int}

module Type_exp = struct
  type t =
    | Anon
    | Constr of {exps:t list; constr:sym}
    | Var of sym
    | Fun of t list
    | Tuple of t list
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

module Parameter = struct
  type t =
    | Untyped of sym
    | Typed of {parameter:sym; typ:Type_exp.t}
end

module Exp = struct
  type t =
    | App of app
    | Asr of binary_op
    | Assign of binary_op
    | Cons of binary_op
    | Const of Const.t
    | Divide of binary_op
    | Equals of binary_op
    | Fun of {parameters:Parameter.t list; body:t}
    | Greater of binary_op
    | Greater_eq of binary_op
    | If of {ante:t; cons:t; alt:t}
    | Land of binary_op
    | Less of binary_op
    | Less_eq of binary_op
    | Let of {binding:bindings; body:t}
    | Lnot of t
    | Lor of binary_op
    | Lsl of binary_op
    | Lsr of binary_op
    | Lxor of binary_op
    | Match of {exp:t; pattern_matches:pattern_match list}
    | Minus of binary_op
    | Mod of binary_op
    | Multiply of binary_op
    | Neg of t
    | Not of t
    | Not_eq of binary_op
    | Or of binary_op
    | Pattern_fun of pattern_match list
    | Plus of binary_op
    | Sequence of t list
    | Tuple of t list
    | Type_constr of {constr:sym; exp:t}
    | Typed of t * Type_exp.t
    | Unit
    | Var of sym
    | Record of field list
  and binding =
    | Value of {bound:Pattern.t; exp:t}
    | Function of {sym:sym; parameters:Parameter.t list; body:t}
  and bindings =
    | Let_binding of binding list
    | Rec_binding of binding list
  and pattern_match = {pattern:Pattern.t; body:t}
  and binary_op = {lhs:t; rhs:t}
  and app = {fn:t; args:t list}
  and field = {field:sym; typ:Type_exp.t option; exp:t}
end

type prog = Exp.bindings list
