OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
INCLUDES =
OCAMLFLAGS = $(INCLUDES)
OCAMLOPTFLAGS = $(INCLUDES)
OBJ = util.cmx ast.cmx parser.cmx lexer.cmx mc.cmx
TARG = mc

$(TARG): $(OBJ)
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) $(OBJ)

parser.mli: parser.mly
	$(OCAMLYACC) $<

parser.ml: parser.mli

lexer.ml: lexer.mll
	$(OCAMLLEX) $<

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f *.cm[iox] *.o mc parser.ml parser.mli lexer.ml

depend: parser.ml lexer.ml
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.PHONY: clean depend

include .depend
