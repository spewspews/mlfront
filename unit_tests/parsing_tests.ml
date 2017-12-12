open OUnit2

module G = Grammar

let one_sym =
  let lex = Parser.lexer "in" (Stream.of_string "bar") in
  [
    "Returns sym bar" >:: (fun ctxt ->
      let (t,spos,epos) = lex () in
      assert_equal ~ctxt (G.SYM Ast.{name="bar"; lnum=0}) t
    );
    "Returns eof" >:: (fun ctxt ->
      let (t,spos,epos) = lex () in
      assert_equal ~ctxt G.EOF t
    );
  ]

let two_syms =
  let lex = Parser.lexer "in" (Stream.of_string "foo bar") in
  [
    "Returns sym foo" >:: (fun ctxt ->
      let (t,spos,epos) = lex () in
      assert_equal ~ctxt (G.SYM Ast.{name="foo"; lnum=0}) t
    );
    "Returns sym bar" >:: (fun ctxt ->
      let (t,spos,epos) = lex () in
      assert_equal ~ctxt (G.SYM Ast.{name="bar"; lnum=0}) t
    );
    "Returns eof" >:: (fun ctxt ->
      let (t,spos,epos) = lex () in
      assert_equal ~ctxt G.EOF t
    );
  ]

let lexer_tests = one_sym @ two_syms

let () = run_test_tt_main ("Lexer tests" >::: lexer_tests)
