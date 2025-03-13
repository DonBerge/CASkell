STACK = stack

all: build test docs

docs:
	cabal haddock --haddock-internal

open-docs:
	cabal haddock --haddock-internal --open

setup:
	$(STACK) setup
	$(STACK) install
	$(STACK) install doctest

test:
	cabal repl --with-compiler=doctest

build:
	$(STACK) build

repl: 
	$(STACK) exec -- ghci