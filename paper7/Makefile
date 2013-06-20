all: build

.PHONY: init build doc test CI sync paper

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build: init
	cabal build

test: build
	cabal test

install: init build test
	cabal install

doc:
	cabal haddock --hyperlink-source

sync:
	git pull

paper:
	./dist/build/make-paper/make-paper

CI: sync test paper
