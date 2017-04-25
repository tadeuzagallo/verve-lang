.PHONY: build
build:
	ocamlbuild -use-ocamlfind  src/main.byte
	ln -fs src/main.byte verve

.PHONY: test
test:
	lit -v tests
