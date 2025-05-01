# Compiler commands
COQC = coqc -Q . Main -Q MenhirLib MenhirLib
COQDEP = coqdep -Q . Main -Q MenhirLib MenhirLib
MENHIR = menhir
OCAMLLEX = ocamllex
OCAMLC = ocamlc

# Menhir source and output
PARSER_SOURCE = Parser.vy
PARSER_OUTPUT = Parser.v

# Paths
EXTRACTED_DIR = extraction

# Source files
VFILES = Cabs.v $(PARSER_OUTPUT) extraction.v
VOFILES := $(VFILES:.v=.vo)

EXTRACTED_ML := $(wildcard $(EXTRACTED_DIR)/*.ml)
EXTRACTED_MLI := $(wildcard $(EXTRACTED_DIR)/*.mli)
EXTRACTED_CMI := $(EXTRACTED_MLI:.mli=.cmi)

MLFILES := main.ml

# Default target
all: $(VOFILES) ocaml-build

# Rule: generate Parser.v from Parser.vy
$(PARSER_OUTPUT): $(PARSER_SOURCE)
	$(MENHIR) --coq --coq-no-version-check $<

# Compile Coq files to .vo
%.vo: %.v
	$(COQC) $<

# Generate lexer.ml from lexer.mll
$(EXTRACTED_DIR)/lexer.ml: $(EXTRACTED_DIR)/lexer.mll
	$(OCAMLLEX) $< -o $@

# Build .cmi files from .mli
$(EXTRACTED_DIR)/%.cmi: $(EXTRACTED_DIR)/%.mli
	$(OCAMLC) -c -I $(EXTRACTED_DIR) $< -o $@

$(EXTRACTED_DIR)/%.cmo: $(EXTRACTED_DIR)/%.ml
	$(OCAMLC) -c -I $(EXTRACTED_DIR) $< -o $@

lexer.cmo: extraction/lexer.ml
	$(OCAMLC) -c -I $(EXTRACTED_DIR) $<

main.cmo: main.ml
	$(OCAMLC) -c -I $(EXTRACTED_DIR) $<
# Build OCaml executable
EXTRACTED_CMO := $(EXTRACTED_ML:.ml=.cmo)

ocaml-build: $(EXTRACTED_CMI) $(EXTRACTED_CMO)  main.cmo
	$(OCAMLC) -I $(EXTRACTED_DIR) -o main.byte $(EXTRACTED_CMO) lexer.cmo main.cmo

# Dependency generation
depend:
	$(COQDEP) $(VFILES) > .depend

# Cleanup
clean:
	rm -f *.vo *.glob *.vok *.vos .*.aux .depend \
	      MenhirLib/*.vo MenhirLib/*.vos MenhirLib/*.glob MenhirLib/*.vok \
	      Parser.v Parser.mli Parser.ml lexer.ml main.byte \
	      $(EXTRACTED_DIR)/*

-include .depend
