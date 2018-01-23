type sym = {n : string; lnum : int}

type const =
| Bool of bool
| Char of char
| Error of string
| Float of float
| Int of int
| String of string

module Type_exp = struct
  type t =
  | Anon
  | Constr of {exps : t list; constr : sym}
  | Var of sym
  | Fun of t list
  | Tuple of t list
end

module Pattern = struct
  type t =
  | Alt of t list
  | As of {pattern : t; bound_var : sym}
  | Cons of t * t
  | Const of const
  | Name of sym
  | Tuple of t list
  | Type_constr of {constr : sym; body : t}
  | Typed of {pattern : t; typ : Type_exp.t}
end

type exp =
| Const of const
| Plus of exp * exp
| Var of sym

type bind =
| Value of {bound : Pattern.t; exp : exp}
| Function of {sym : sym; args : sym list; exp : exp}

type mutual_bind =
| Let_bind of bind list
| Rec_bind of bind list

type prog = mutual_bind list

let last_sym = ref {n="No Sym"; lnum=0}
