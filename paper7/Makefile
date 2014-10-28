all: paper

.PHONY: init build clean doc test CI sync paper presentation

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build: init
	cabal build -j1


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
#gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS='/screen' -sOutputFile=output/paper-lowres.pdf output/paper.pdf


presentation: build
	./dist/build/make-presentation/make-presentation

CI: sync test paper
