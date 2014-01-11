CMO=tokens.cmo lexer.cmo parser.cmo typer.cmo mips.cmo compile.cmo main.cmo 
COMPILER = ocamlc -g
LEXER = ocamllex
PARSER = menhir
PARSER-OPTS = 
GENERATED = tokens.ml tokens.mli lexer.ml parser.ml parser.mli 
BIN=minic++
FLAGS=
MARS=java -jar docs/tests/Mars_4_4.jar

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
	@rm -f *.cm[io] *.s *.o *~ *.annot *.automaton .depend sortie_test $(BIN) $(GENERATED) parser.output

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

test: $(BIN) test.cpp
	@echo "########## Sortie pour le fichier compilé par minic++ :"	
	@./$(BIN) test.cpp
	@$(MARS) test.s | tail -n +3

g++: test.cpp
	@echo "########## Sortie pour le fichier compilé par g++ :"
	@g++ test.cpp -o /tmp/test -fpermissive
	@/tmp/test

compare: test g++

include .depend
