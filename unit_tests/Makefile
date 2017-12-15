test: parsing_tests
	./parsing_tests

parsing_tests: parsing_tests.ml
	ocamlopt -o $@ -I .. -I +oUnit unix.cmxa oUnitAdvanced.cmxa oUnit.cmxa ast.cmx parser.cmx lexer.cmx $<

clean:
	rm -f *.cm[ix] *.o oUnit* parsing_tests
