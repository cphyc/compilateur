all:
	ocamlbuild -use-menhir -menhir "menhir -v" main.native -j 4
	@echo
	@echo "Moving binary to" `pwd`
	mv _build/main.native minic++
	rm main.native

clean:
	ocamlbuild -clean
	@echo "Removing binary : rm minic++"
	if [ -e minic++ ]; then rm minic++; fi
