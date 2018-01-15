OBJ = parser.cmx lexer.cmx

build: $(OBJ)

parser.cmx: parser.ml parser.cmi ast.cmi
	ocamlopt -c parser.ml

parser.cmi: parser.mli ast.cmi
	ocamlopt $<

parser.ml: parser.mly
	ocamlyacc $<

parser.mli: parser.mly
	ocamlyacc $<

lexer.cmx: lexer.ml parser.cmi ast.cmi util.cmi
	ocamlopt -c $<

lexer.ml: lexer.mll
	ocamllex $<

%.cmi: %.ml
	ocamlopt -c $<

parser.output: parser.mly
	ocamlyacc -v $<

test: $(OBJ)
	$(MAKE) -C unit_tests

clean:
	rm -f *.cm[ix] *.o parser.mli parser.ml lexer.ml parser.output
	$(MAKE) -C unit_tests clean

.PHONY: clean build
