SOURCES = types.ml parser.mly lexer.mll fcparsing.ml common.ml debruijn.ml typing.ml environment.ml debug.ml betareduce.ml indistinguish.ml bind.ml tactic.ml testtactics.ml funcrypt.ml

EXEC = funcrypt

TOP = fctop

LIBS =

#######################################################################################

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLMKTOP = ocamlmktop
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

#######################################################################################

all: depend $(EXEC) $(TOP)

opt : $(EXEC).opt

SOURCES1 = $(SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
OBJS = $(SOURCES2:.ml=.cmo)
OPTOBJS = $(SOURCES2:.ml=.cmx)

$(EXEC): $(OBJS)
	$(CAMLC) -o $(EXEC) $(LIBS) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

$(TOP): $(OBJS)
	$(CAMLMKTOP) -o $(TOP) $(LIBS) $(OBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm lexer.ml parser.mli parser.ml
	rm -f $(EXEC)
	rm -f $(EXEC).opt
	rm -f $(TOP)

.depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > .depend

depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
