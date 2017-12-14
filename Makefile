OBJ = grammar.cmx

build: $(OBJ)

#parser.cmx: parser.ml parser.cmi grammar.cmx
#	ocamlopt -c -I +menhirLib parser.ml

#parser.cmi: parser.mli ast.cmi grammar.cmi
#	ocamlopt parser.mli

grammar.cmx: grammar.ml grammar.cmi ast.cmi
	ocamlopt -c grammar.ml

grammar.cmi: grammar.mli ast.cmi
	ocamlopt $<

grammar.ml: grammar.mly
	ocamlyacc grammar.mly

grammar.mli: grammar.mly
	ocamlyacc grammar.mly

%.cmi: %.ml
	ocamlopt -c $<

test: parser.cmx
	$(MAKE) -C unit_tests

clean:
	rm -f *.cm[ix] *.o grammar.mli grammar.ml
	$(MAKE) -C unit_tests clean

.PHONY: clean build
