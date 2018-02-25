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

$(TARG): $(OBJ) depend
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) $(OBJ)

%.ml: %.mly
	$(OCAMLYACC) $<

%.ml: %.mll
	$(OCAMLLEX) $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f *.cm[iox] *.o mc parser.ml parser.mli lexer.ml depend

depend: parser.ml lexer.ml
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > depend

include depend
