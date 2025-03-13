STACK = stack

all: build test docs

docs:
	cabal haddock --haddock-internal

open-docs:
	cabal haddock --haddock-internal --open

setup:
	stack setup
	stack install

test:
	cabal repl --with-compiler=doctest

build:
	$(STACK) build

repl: 
	$(STACK) exec -- ghci