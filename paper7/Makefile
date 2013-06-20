all: build

.PHONY: init build doc test CI sync

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

test: build
	cabal test

install: init build test
	cabal install

doc:
	cabal haddock --hyperlink-source

sync:
	git pull

CI: sync init test
