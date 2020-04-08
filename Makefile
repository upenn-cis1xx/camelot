.PHONY: dump clean $(INCLUDES)
INCLUDES= checkers

all: build test

# Builds the ppx rewriter names sample
build:
	ocamlbuild -Is $(INCLUDES) -package compiler-libs.common linter.native


# Runs the ppx rewriter on test code
test:
	ocamlfind ppx_tools/rewriter ./linter.native test.ml


# Lets you check the AST for an individual expression
# e.g. make dump "1 + 2"
dump:
	ocamlfind ppx_tools/dumpast -e

# Clears the current build
clean:
	rm -rf _build *.native



