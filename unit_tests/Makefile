test: parsing_tests
	./parsing_tests

parsing_tests: parsing_tests.ml
	ocamlopt -o $@ -I .. -I +oUnit -I +menhirLib unix.cmxa menhirLib.cmx oUnitAdvanced.cmxa oUnit.cmxa grammar.cmx parser.cmx $<

clean:
	rm -f *.cm[ix] *.o oUnit* parsing_tests

