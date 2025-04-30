# Compiler commands
COQC = coqc -Q . Main -Q MenhirLib MenhirLib
COQDEP = coqdep -Q . Main -Q MenhirLib MenhirLib
MENHIR = menhir
OCAMLLEX = ocamllex
OCAMLC = ocamlc

# Menhir source and output
PARSER_SOURCE = Parser.vy
PARSER_OUTPUT = Parser.v

# Coq files
VFILES = Ast.v $(PARSER_OUTPUT) extraction.v
VOFILES = $(VFILES:.v=.vo)

# OCaml files
MLFILES = Ast.mli Ast.ml Parser.mli Parser.ml lexer.ml main.ml

# Default target
all: $(VOFILES) ocaml-build

# Rule: generate Parser.v from Parser.vy
Parser.v: Parser.vy
	$(MENHIR) --coq --coq-no-version-check $<


# Rule: compile .v to .vo
%.vo: %.v
	$(COQC) $<

# Rule: generate lexer.ml from lexer.mll
lexer.ml: lexer.mll
	$(OCAMLLEX) $<

# Rule: build OCaml executable
ocaml-build: lexer.ml
	$(OCAMLC) -o main.byte $(MLFILES)

depend:
	$(COQDEP) $(VFILES) > .depend

clean:
	rm -f *.vo *.glob *.vok *.vos .*.aux .depend MenhirLib/.*.aux MenhirLib/*.vok \
	MenhirLib/*.glob MenhirLib/*.vo MenhirLib/*.vos MenhirLib/.depend Pre_parser.ml Pre_parser.mli Parser.v Parser.mli Parser.ml *.out *.cmo *.cmi lexer.ml  \
	Ascii.ml Ascii.mli Ast.mli Ast.ml Datatypes.ml Datatypes.mli String.mli String.ml main.byte

-include .depend

