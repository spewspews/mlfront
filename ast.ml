module U = Util

(*
	TODO: need to break this up into type expressions
	and type definitions I think.
*)
module Type_exp = struct
	type t =
		| Anon
		| Constr of {exps:t list; constr:U.sym}
		| Var of U.sym
		| Fun of t list
		| Tuple of t list
		| Record of field list
		| Variant of variant list
	and declaration = {ctor:U.sym; params:U.sym list; body:t}
	and variant = {ctor:U.sym; param:t option}
	and field = {field_name:U.sym; typ:t}

	let rec dump =
		let module P = Printf in
		function
		| Anon -> P.printf "(\"Anon type\")"
		| Constr {exps; constr} ->
			P.printf "(\"Type constructor\" (%s " (U.quote constr);
			List.iter (fun t -> dump t; print_char ' ') exps;
			print_string "))\n";
		| Var s -> P.printf "(\"Type variable\" %s)" (U.quote s)
		| Fun l ->
			print_string "(\"Function type\" ";
			List.iter (fun t -> dump t; print_char ' ') l;
			print_string ")\n"
		| Tuple l ->
			print_string "(\"Tuple type\" ";
			List.iter (fun t -> dump t; print_char ' ') l;
			print_string ")\n"
		| Record l ->
			let f {field_name; typ} =
				P.printf "((Field %s) (Type " (U.quote field_name);
				dump typ;
				print_string ")) ";
			in
			print_string "(\"Record type\" ";
			List.iter f l;
			print_string ")\n";
		| Variant {ctor; param} ->
			P.printf "((Ctor %s) (" (U.quote sym);
			match param with
				| Some t -> dump t
				| None -> ();
			print_string ")\n"
			let f = function
				| Untyped sym -> P.printf "((Ctor %s) ())" (U.quote sym)
				| Typed {sym; typ} ->
					P.printf "((Ctor %s) " (U.quote sym);
					dump typ;
					print_string ")";
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
		| App of {fun_name:t; parameters:t list}
		| Arr of t list
		| Asr of binary_op
		| Assign of binary_op
		| Cons of binary_op
		| Const of Const.t
		| Divide of binary_op
		| Equals of binary_op
		| Fun of function_def
		| Function of fun_match list
		| Greater of binary_op
		| Greater_eq of binary_op
		| If of {ante:t; cons:t; alt:t}
		| Land of binary_op
		| Less of binary_op
		| Less_eq of binary_op
		| Let of {decls:declaration list; scope:t}
		| List of t list
		| Lnot of t
		| Lor of binary_op
		| Lsl of binary_op
		| Lsr of binary_op
		| Lxor of binary_op
		| Match of {exp:t; matches:fun_match list}
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
		| Type_constr of {constr:U.sym; exp:t}
		| Typed of t * Type_exp.t
		| Unit
		| Var of U.sym
	and bindings =
		| Let_binding of binding list
		| Rec_binding of binding list
	and binary_op = {lhs:t; rhs:t}
	and field =
		| Typed_field of {field_name:U.sym; typ:Type_exp.t; exp:t}
		| Untyped_field of {field_name:U.sym; exp:t}
	and binding = {pattern:Pattern.t; value:t}
	and function_def = {parameters:Pattern.t list; body:t}
	and fun_match = {pattern:Pattern.t; body:t}
	type declaration =
		| Value_decl of binding
		| Function_decl of {name:U.sym; def:function_def}
end

type top =
	| Exp of Exp.declaration list
	| Type of Type_exp.binding list

type prog = top list

let dump_e e =
	let f = function
		| Exp.Function_def (sym, _) -> Printf.printf "(Fun %s)\n" (U.quote sym)
		| Exp.Value_def _ -> print_string "(Value)\n"
	in
	let l = match e with
		| Exp.Let_binding l -> print_string "(\"Let binding\" "; l
		| Exp.Rec_binding l -> print_string "(\"Letrec binding\" "; l
	in
	List.iter f l;
	print_string ")\n"

let dump_t t =
	let f Type_exp.{ctor=U.{n}; params; body} =
		Printf.printf "(\"%s\" (" n;
		List.iter (fun U.{n} -> Printf.printf "\"%s\" " n) params;
		print_string ")";
		Type_exp.dump body;
		print_string ")\n";
	in
	print_string "(\"Type binding\" ";
	List.iter f t;
	print_string ")\n"

let dump prog =
	print_endline "(";
	List.iter (function Exp e -> dump_e e | Type t -> dump_t t) prog;
	print_endline ")";
