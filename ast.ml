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
  | Const of Const.t
  | Fun of {parameters:Parameter.t list; body:t}
  | Function of pattern_match list
  | Let of {binding:let_bindings; body:t}
  | Match of {exp:t; pattern_matches:pattern_match list}
  | Plus of t * t
  | Typed of t * Type_exp.t
  | Var of sym
  and pattern_match = {pattern:Pattern.t; body:t}
  and binding =
  | Value_binding of {bound:Pattern.t; exp:t}
  | Function_binding of {sym:sym; parameters:Parameter.t list; body:t}
  and let_bindings =
  | Let_bind of binding list
  | Rec_bind of binding list
end

type prog = Exp.let_bindings list
