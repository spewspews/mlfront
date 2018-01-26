OBJ = ast.cmx util.cmx parser.cmx lexer.cmx mc.cmx
TARG = mc

$(TARG): $(OBJ)
	ocamlopt -o $(TARG) $^

mc.cmx: mc.cmi

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

util.cmx: util.cmi ast.cmi

ast.cmx: ast.cmi

%.cmi: %.ml
	ocamlopt -c $<

parser.output: parser.mly
	ocamlyacc -v $<

test: $(OBJ)
	$(MAKE) -C unit_tests

clean:
	rm -f *.cm[ix] *.o parser.mli parser.ml lexer.ml parser.output a.out mc
	$(MAKE) -C unit_tests clean

.PHONY: clean build
