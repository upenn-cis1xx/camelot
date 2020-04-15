.PHONY: all clean test
INCLUDES := checkers
TESTS := $(wildcard test/*)

all: camelot

# Builds the linter and the harness for the linter
camelot:
	ocamlbuild -Is $(INCLUDES) -package compiler-libs.common -package ANSITerminal main.native && mv ./main.native camelot

test:
	./camelot -d $(TESTS)

# Clears the current build
clean:
	rm -rf _build *.native camelot



