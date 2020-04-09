.PHONY: dump clean $(INCLUDES)
INCLUDES= checkers

TOLINT = test.ml destructtest.ml

# Builds the linter and the harness for the linter
linter:
	ocamlbuild -Is $(INCLUDES) -package compiler-libs.common linter.native && ocamlbuild -package compiler-libs.common main.native

lint:
	./main.native ./linter.native $(TOLINT)
# Clears the current build
clean:
	rm -rf _build *.native



