module Type :
  sig
    type exp =
        Anon
      | Ctor of { ctor : Util.sym; parameters : exp list; }
      | Var of Util.sym
      | Fun of exp list
      | Product of exp list
    type variant = { var_ctor : Util.sym; param : exp option; }
    type field = { field_name : Util.sym; typ : exp; }
    type body = Sum of variant list | Record of field list | Exp of exp
    type decl = { ctor : Util.sym; params : Util.sym list; body : body; }
  end
module Constant :
  sig
    type t =
        Bool of bool
      | Char of char
      | Error of string
      | Float of float
      | Int of int
      | String of string
      | Unit
  end
module Pattern :
  sig
    type t =
        Alt of t list
      | As of { pattern : t; bound_var : Util.sym; }
      | Cons of t * t
      | Constant of Constant.t
      | Name of Util.sym
      | Tuple of t list
      | Variant of { var_ctor : Util.sym; body : t; }
      | Typed of { pattern : t; typ : Type.exp; }
  end
module Exp :
  sig
    type t =
        App of { fun_name : t; parameters : t list; }
      | Array of t list
      | Asr of binary_op
      | Assign of binary_op
      | Cons of binary_op
      | Constant of Constant.t
      | Divide of binary_op
      | Equals of binary_op
      | Fun of fun_def
      | Function of fun_def list
      | Greater of binary_op
      | Greater_eq of binary_op
      | If of { ante : t; cons : t; alt : t; }
      | Land of binary_op
      | Less of binary_op
      | Less_eq of binary_op
      | Let of { decls : decls; scope : t; }
      | List of t list
      | Lnot of t
      | Lor of binary_op
      | Lsl of binary_op
      | Lsr of binary_op
      | Lxor of binary_op
      | Match of { exp : t; matches : fun_def list; }
      | Minus of binary_op
      | Mod of binary_op
      | Multiply of binary_op
      | Neg of t
      | Not of t
      | Not_eq of binary_op
      | Or of binary_op
      | Plus of binary_op
      | Record of field list
      | Sequence of t list
      | Tuple of t list
      | Type_constr of { constr : Util.sym; exp : t; }
      | Typed of t * Type.exp
      | Unit
      | Var of Util.sym
    and decls = Non_rec of decl list | Rec of decl list
    and binary_op = { lhs : t; rhs : t; }
    and field =
        Typed_field of { field_name : Util.sym; typ : Type.exp; exp : t; }
      | Untyped_field of { field_name : Util.sym; exp : t; }
    and fun_def = { parameters : Pattern.t list; body : t; }
    and decl =
        Value_decl of { pattern : Pattern.t; value : t; }
      | Function_decl of { name : Util.sym; def : fun_def; }
  end
type top = Exp of Exp.decl list | Type of Type.decl list
type prog = top list
