all: paper

.PHONY: init build clean doc test CI sync paper presentation

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build: init
	cabal build  --ghc-options=-dsuppress-module-prefixes


clean:
	cabal clean

test: build
	cabal test

install: init build test
	cabal install

doc:
	cabal haddock --hyperlink-source

sync:
	git pull


paper: build
# ./dist/build/make-figures/make-figures
	./link-figure.sh
	./dist/build/make-paper/make-paper

presentation: build
	./dist/build/make-presentation/make-presentation

CI: sync test paper
