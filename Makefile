.PHONY: build
build:
	cd src && ocamlbuild -use-ocamlfind  main.byte
	ln -fs src/main.byte verve

.PHONY: test
test: build
	lit -v tests

.PHONY: clean
clean:
	cd src && ocamlbuild -clean
