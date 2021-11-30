RUNTEST=cabal v2-run -- run-testset

quick:  build
	${RUNTEST} --tool validatePDF --corpora misc
	${RUNTEST} --tool validatePDF --corpora generated

build:
	cd ../..; cabal v2-build exe:run-testset

all:	quick
	${RUNTEST} --tool validatePDF --corpora 2020-03-eval






