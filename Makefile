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

# Coq files
VFILES = Cabs.v $(PARSER_OUTPUT) extraction.v
VOFILES := $(VFILES:.v=.vo)

# Default target
all: $(VOFILES) ocaml-build

# Rule: generate Parser.v from Parser.vy
$(PARSER_OUTPUT): $(PARSER_SOURCE)
	$(MENHIR) --coq --coq-no-version-check $<

# Compile Coq files to .vo
%.vo: %.v
	$(COQC) $<

# Rule: generate lexer.ml from lexer.mll inside extraction/
$(EXTRACTED_DIR)/lexer.ml: lexer.mll | $(EXTRACTED_DIR)
	$(OCAMLLEX) $< -o $@

# Rule: copy main.ml into extraction/
$(EXTRACTED_DIR)/main.ml: main.ml | $(EXTRACTED_DIR)
	cp $< $@

# Rule: make sure extraction/ exists
$(EXTRACTED_DIR):
	mkdir -p $@

# Compile .mli to .cmi
$(EXTRACTED_DIR)/%.cmi: $(EXTRACTED_DIR)/%.mli
	$(OCAMLC) -c -I $(EXTRACTED_DIR) $< -o $@

# Compile .ml to .cmo
$(EXTRACTED_DIR)/%.cmo: $(EXTRACTED_DIR)/%.ml
	$(OCAMLC) -c -I $(EXTRACTED_DIR) $< -o $@

# Build OCaml executable
ocaml-build: $(EXTRACTED_DIR)/lexer.ml $(EXTRACTED_DIR)/main.ml
	$(MAKE) $(EXTRACTED_DIR)/*.cmo
	$(OCAMLC) -o main.byte $(EXTRACTED_DIR)/*.cmo

# Dependency generation
depend:
	$(COQDEP) $(VFILES) > .depend

# Cleanup
clean:
	rm -f *.vo *.glob *.vok *.vos .*.aux .depend \
	      MenhirLib/*.vo MenhirLib/*.vos MenhirLib/*.glob MenhirLib/*.vok \
	      Parser.v Parser.mli Parser.ml lexer.ml main.byte \
	      $(EXTRACTED_DIR)/* Cabs.ml Cabs.mli

-include .depend
