CMO=tokens.cmo lexer.cmo parser.cmo main.cmo
COMPILER = ocamlc
LEXER = ocamllex
PARSER = menhir
PARSER-OPTS = 
GENERATED = tokens.ml tokens.mli lexer.ml parser.ml parser.mli
BIN=minic++
FLAGS=

all: $(BIN)
	@echo ''Compilation réussie.''

$(BIN):$(CMO)
	$(COMPILER) $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly  

.mli.cmi:
	$(COMPILER) -annot $(FLAGS) -c  $<

.ml.cmo:
	$(COMPILER) -annot $(FLAGS) -c  $<

.mll.ml:
	$(LEXER) $<

tokens.ml tokens.mli: tokens.mly
	$(PARSER) --only-tokens tokens.mly


parser.ml parser.mli: parser.mly
	$(PARSER) $(PARSER-OPTS) --base parser --external-tokens Tokens -v tokens.mly parser.mly

clean:
	@echo "Suppression des fichiers générés par make."
	@rm -f *.cm[io] *.o *~ *.annot *.automaton .depend sortie_test $(BIN) $(GENERATED) parser.output

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
