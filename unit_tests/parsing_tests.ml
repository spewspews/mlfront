open OUnit2

module P = Parser
module L = Lexer
module A = Ast

let string_of_token = function
| P.SYM A.{n; lnum} -> Printf.sprintf "SYM {n=\"%s\"; lnum=%d}" n lnum
| P.LET -> "LET"
| P.EQ -> "EQ"
| P.INT i -> Printf.sprintf "INT %d" i
| P.EOF -> "EOF"
| _ -> "DUNNO"

let assert_token = assert_equal ~printer:string_of_token

let lexing_tests =
  let buf = Lexing.from_string "let foo = 3 + 4" in
  [
    "returns LET" >:: (fun ctxt ->
      assert_token P.LET (L.token buf)
    );
    "returns SYM foo" >:: (fun ctxt ->
      assert_token ~ctxt (P.SYM A.{n="foo"; lnum=1}) (L.token buf)
    );
    "returns EQ" >:: (fun ctxt ->
      assert_token ~ctxt P.EQ (L.token buf)
    );
    "returns INT 3" >:: (fun ctxt ->
      assert_token ~ctxt (P.INT 3) (L.token buf)
    );
    "returns PLUS" >:: (fun ctxt ->
      assert_token ~ctxt P.PLUS (L.token buf)
    );
    "returns INT 4" >:: (fun ctxt ->
      assert_token ~ctxt (P.INT 4) (L.token buf)
    );
    "returns EOF" >:: (fun ctxt ->
      assert_token ~ctxt P.EOF (L.token buf)
    );
  ]

let () = run_test_tt_main ("Lexer tests" >::: lexing_tests)

let basic_arithmetic =
  let buf = Lexing.from_string "let x = 3 + 4" in
  fun ctxt ->
    match P.ml L.token buf with
    | [A.{binds=b; is_rec=false}] ->
      begin match b with
      | [A.{sym; args=[]; exp}] ->
        assert_equal ~ctxt A.{n="x"; lnum=1} sym;
        assert_equal ~ctxt (A.Plus (A.Int 3, A.Int 4)) exp
      | _ -> assert_failure "foobar"
      end
    | _ -> assert_failure "foobar"

let () = run_test_tt_main ("Parser tests" >::: ["arithmetic" >:: basic_arithmetic])
