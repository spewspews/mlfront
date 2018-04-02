module U = Util

module Type = struct
	type exp =
		| Anon
		| Ctor of {ctor:U.sym; params:exp list}
		| Var of U.sym
		| Fun of exp list
		| Product of exp list
	type variant = {variant:U.sym; typ:exp option}
	type field = {field:U.sym; typ:exp}
	type body =
		| Sum of variant list
		| Record of field list
		| Exp of exp
	type decl = {ctor:U.sym; params:U.sym list; body:body}

(*
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
*)
end

module Constant = struct
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
		| Constant of Constant.t
		| Name of U.sym
		| Tuple of t list
		| Variant of {variant:U.sym; body:t}
		| Typed of {pattern:t; typ:Type.exp}
end

module Exp = struct
	type t =
		| Apply of {fun_name:t; params:t list}
		| Array of t list
		| Asr of binary_op
		| Assign of binary_op
		| Cons of binary_op
		| Constant of Constant.t
		| Divide of binary_op
		| Equals of binary_op
		| Fun of fun_def
		| Function of matching list
		| Greater of binary_op
		| Greater_eq of binary_op
		| If of {ante:t; cons:t; alt:t}
		| Land of binary_op
		| Less of binary_op
		| Less_eq of binary_op
		| Let of {decls:decls; scope:t}
		| List of t list
		| Lnot of t
		| Lor of binary_op
		| Lsl of binary_op
		| Lsr of binary_op
		| Lxor of binary_op
		| Match of {exp:t; matches:matching list}
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
		| Typed of t * Type.exp
		| Unit
		| Var of U.sym
	and decls =
		| Non_rec of decl list
		| Rec of decl list
	and binary_op = {lhs:t; rhs:t}
	and field = {field:U.sym; exp:t}
	and fun_def = {params:Pattern.t list; body:t}
	and matching = {pattern:Pattern.t; result:t}
	and decl =
		| Value_decl of {pattern:Pattern.t; value:t}
		| Function_decl of {name:U.sym; def:fun_def}
end

type top =
	| Exp of Exp.decls
	| Type of Type.decl list

type prog = top list
(*
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
*)
