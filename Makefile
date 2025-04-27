COQC=coqc -Q . Main -Q MenhirLib MenhirLib
COQDEP=coqdep -Q . Main -Q MenhirLib MenhirLib
MENHIR=menhir

# Menhir source and output
PARSER_SOURCE = Parser.vy
PARSER_OUTPUT = Parser.v

VFILES = Ast.v $(PARSER_OUTPUT) extraction.v
VOFILES = $(VFILES:.v=.vo)

all: $(VOFILES)

# Rule: first generate Parser.v from Parser.vy
$(PARSER_OUTPUT): $(PARSER_SOURCE)
	$(MENHIR) --coq --coq-no-version-check $<

# Rule: compile .v to .vo
%.vo: %.v
	$(COQC) $<

depend:
	$(COQDEP) $(VFILES) > .depend

clean:
	rm -f *.vo *.glob *.vok *.vos .*.aux .depend MenhirLib/.*.aux MenhirLib/*.vok \
	MenhirLib/*.glob MenhirLib/*.vo MenhirLib/*.vos MenhirLib/.depend Parser.v Parser.mli Parser.ml

-include .depend
