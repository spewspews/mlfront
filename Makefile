parser.cmx: parser.ml parser.cmi grammar.cmx
	ocamlopt -c -I +menhirLib parser.ml

parser.cmi: parser.mli ast.cmi grammar.cmi
	ocamlopt parser.mli

ast.cmi: ast.ml
	ocamlopt -c ast.ml

grammar.mli: grammar.mly
	menhir grammar.mly

grammar.cmx: grammar.ml grammar.cmi
	ocamlopt -c grammar.ml

%.cmi: %.mli
	ocamlopt $<

test: parser.cmx
	$(MAKE) -C unit_tests

clean:
	rm -f *.cm[ix] *.o grammar.mli grammar.ml
	$(MAKE) -C unit_tests clean
