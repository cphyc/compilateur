all:
	ocamlbuild -use-menhir main.native -j 4
	mv _build/main.native minic++
	rm main.native

clean:
	ocamlbuild -clean
	rm minic++
