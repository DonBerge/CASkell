STACK = stack

main: build test docs

docs:
	cabal haddock --haddock-internal

open-docs:
	cabal haddock --haddock-internal --open

test:
	cabal repl --with-compiler=doctest

build:
	$(STACK) build

repl: 
	$(STACK) repl