OBJ = parser.cmx lexer.cmx

build: $(OBJ)

parser.cmx: parser.ml parser.cmi ast.cmi
	ocamlopt -c parser.ml

parser.cmi: parser.mli ast.cmi
	ocamlopt $<

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

lexer.cmx: lexer.ml parser.cmi ast.cmi
	ocamlopt -c $<

lexer.ml: lexer.mll
	ocamllex $<

%.cmi: %.ml
	ocamlopt -c $<

test: $(OBJ)
	$(MAKE) -C unit_tests

clean:
	rm -f *.cm[ix] *.o parser.mli parser.ml lexer.ml
	$(MAKE) -C unit_tests clean

.PHONY: clean build
