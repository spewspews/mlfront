module U = Util

module Type_exp = struct
  type t =
    | Anon
    | Constr of {exps:t list; constr:U.sym}
    | Var of U.sym
    | Fun of t list
    | Tuple of t list
    | Record of typed_sym list
    | Variant of name list
  and binding = {ctor:U.sym; params:U.sym list; body:t}
  and name =
    | Untyped of U.sym
    | Typed of typed_sym
  and typed_sym = {sym:U.sym; typ:t}

  let rec dump =
    let module P = Printf in
    function
    | Anon -> P.printf "(\"Anon type\")"
    | Constr {exps; constr} ->
      P.printf "(\"Type constructor\" (%s " (U.quote constr);
      List.iter (fun t -> dump t; print_char ' ') exps;
      print_string ")";
      print_string ")";
    | Var s -> P.printf "(\"Type variable\" %s)" (U.quote s)
    | Fun l ->
      print_string "(\"Function type\" ";
      List.iter (fun t -> dump t; print_char ' ') l;
      print_char ')'
    | Tuple l ->
      print_string "(\"Tuple type\" ";
      List.iter (fun t -> dump t; print_char ' ') l;
      print_char ')'
    | Record l ->
      let f {sym; typ} =
        P.printf "((Field %s) (Type " (U.quote sym);
        dump typ;
        print_string ")) ";
      in
      print_string "(\"Record type\" ";
      List.iter f l;
      print_string ") ";
    | Variant l ->
      let f = function
        | Untyped sym -> P.printf "(Ctor %s)" (U.quote sym)
        | Typed {sym; typ} ->
          P.printf "((Ctor %s) (Typ " (U.quote sym);
          dump typ;
          print_string "))";
      in
      print_string "(\"Variant Type\" ";
      List.iter f l;
      print_string ")\n";
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
    | As of {pattern:t; bound_var:U.sym}
    | Cons of t * t
    | Const of Const.t
    | Name of U.sym
    | Tuple of t list
    | Type_constr of {constr:U.sym; body:t}
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
    | Type_constr of {constr:U.sym; exp:t}
    | Typed of t * Type_exp.t
    | Unit
    | Var of U.sym
  and definition =
    | Function_def of U.sym * Type_exp.name list binding
    | Value_def of Pattern.t binding
  and definitions =
    | Let_binding of definition list
    | Rec_binding of definition list
  and binary_op = {lhs:t; rhs:t}
  and app = {fn:t; args:t list}
  and field = {field:U.sym; typ:Type_exp.t option; exp:t}
  and 'a binding = {binds:'a; body:t}
end

type top =
  | Exp of Exp.definitions
  | Type of Type_exp.binding list

type prog = top list

let dump_e e = ()
let dump_t t =
  let f Type_exp.{ctor=U.{n}; params; body} =
    Printf.printf "(\"Type binding\" (\"%s\" " n;
    List.iter (fun U.{n} -> Printf.printf "\"%s\" " n) params;
    print_string ")";
    Type_exp.dump body;
    print_string ")\n";
  in
  List.iter f t

let dump prog =
  print_endline "(";
  List.iter (function Exp e -> dump_e e | Type t -> dump_t t) prog;
  print_endline ")";
