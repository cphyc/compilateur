CMO=tokens.cmo lexer.cmo parser.cmo hack.cmo main.cmo
GENERATED = tokens.ml tokens.mli lexer.ml parser.ml parser.mli
BIN=minic++
FLAGS=

all: $(BIN)
	echo ''Youpi !''

$(BIN):$(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly  

.mli.cmi:
	ocamlc -annot $(FLAGS) -c  $<

.ml.cmo:
	ocamlc -annot $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

tokens.ml tokens.mli: tokens.mly
	menhir --only-tokens tokens.mly


parser.ml parser.mli: parser.mly
	menhir --base parser --external-tokens Tokens -v tokens.mly parser.mly

clean:
	rm -f *.cm[io] *.o *~ *.annot *.automaton .depend sortie_test $(BIN) $(GENERATED) parser.output

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend