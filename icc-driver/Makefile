
.PHONY: build

build: src/Main.hs src/ICC.hs
	cabal build icc-parser

src/ICC.hs: specs/ICC.ddl src/Generator.hs
	cabal run icc-generator
