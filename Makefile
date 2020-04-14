.PHONY: dump clean $(INCLUDES)
INCLUDES= checkers

TOLINT = test/

# Builds the linter and the harness for the linter
linter:
	ocamlbuild -Is $(INCLUDES) -package compiler-libs.common -package ANSITerminal main.native && mv ./main.native camelot

lint:
	./camelot -d $(TOLINT)

# Clears the current build
clean:
	rm -rf _build *.native camelot



