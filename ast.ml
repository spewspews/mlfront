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
  | Let of {binding:bindings; body:t}
  | Const of Const.t
  | Fun of {parameters:Parameter.t list; body:t}
  | Match of {exp:t; pattern_matches:pattern_match list}
  | Pattern_fun of pattern_match list
  | Plus of t * t
  | Sequence of t list
  | Typed of t * Type_exp.t
  | Var of sym
  | If of {ante:t; cons:t; alt:t}
  | Unit
  | Assign of {lhs:t; rhs:t}
  | Tuple of t list
  and pattern_match = {pattern:Pattern.t; body:t}
  and binding =
  | Value of {bound:Pattern.t; exp:t}
  | Function of {sym:sym; parameters:Parameter.t list; body:t}
  and bindings =
  | Let_binding of binding list
  | Rec_binding of binding list
end

type prog = Exp.bindings list
