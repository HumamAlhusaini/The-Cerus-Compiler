COQC = coqc -Q . Proj -Q MenhirLib MenhirLib
COQDEP = coqdep -Q . Proj -Q MenhirLib MenhirLib
MENHIR = menhir
SEDLEX = ocamlfind ocamlc -c -package sedlex.ppx
OCAMLBUILD = ocamlbuild -use-ocamlfind

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

# Generate Parser.v from Parser.vy
$(PARSER_OUTPUT): $(PARSER_SOURCE)
	$(MENHIR) --coq --coq-no-version-check $<

# Compile Coq files
%.vo: %.v
	$(COQC) $<

# Rule: make sure extraction/ exists
$(EXTRACTED_DIR):
	mkdir -p $@

# Copy enter.ml into extraction/
$(EXTRACTED_DIR)/enter.ml: enter.ml | $(EXTRACTED_DIR)
	cp $< $@

# Copy lexer.ml into extraction/
$(EXTRACTED_DIR)/lexer.ml: lexer.ml | $(EXTRACTED_DIR)
	cp $< $@

# Copy lexer.ml into extraction/
$(EXTRACTED_DIR)/pprint.ml: pprint.ml | $(EXTRACTED_DIR)
	cp $< $@

# Use ocamlbuild to build the OCaml program
ocaml-build: $(EXTRACTED_DIR)/enter.ml $(EXTRACTED_DIR)/lexer.ml
	$(OCAMLBUILD) -I extraction extraction/enter.native

# Dependency generation
depend:
	$(COQDEP) $(VFILES) > .depend

# Clean
clean:
	rm -f *.vo *.glob *.vok *.vos .*.aux .depend \
	      MenhirLib/*.vo MenhirLib/*.vos MenhirLib/*.glob MenhirLib/*.vok \
	      Parser.v Parser.mli Parser.ml enter.byte \
	      *.native *.byte *.o *.cm* *.d.byte *.d.native *.ml.d
	find $(EXTRACTED_DIR) -type f ! -name "_tags" -exec rm -f {} +

test_lexer:
	@echo "Creating test input file..."
	@printf "'a'\n'\\n'\n'\\t'\n'\\\\'\n'\\''\n'🔥'\n'\\x41'\n'\\x7F'\n'\\0'\n'\\u{1F525}'\n''\n'ab'\n'\\xZZ'\n'\\u{110000}'\n'🔥bar'\n\"Hello, world\"\n\"String with newline\\n\"\n\"Tab:\\tEnd\"\n\"Quote: \\\" and Backslash: \\\\\"\n\"Emoji: 🔥\"\n\"Hex: \\x41\\x42\\x43\"\n\"Unicode: \\u{1F525}\\u{1F600}\"\n" > test_input.txt

	@echo "Running lexer..."
	@./enter.native test_input.txt || echo "Lexer failed or returned error"

	@echo "Cleaning up..."
	@rm -f test_input.txt

-include .depend
