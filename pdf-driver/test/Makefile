SHAKEOPTS=-j8
  # works like Make's -j
RUNTESTSET=cabal v2-run -- run-testset ${SHAKEOPTS}

QUICKTESTS=test_validatePDF_generated/test-summary \
           test_validatePDF_misc/test-summary
LONGTESTS=test_validatePDF_2020-03-eval/test-summary

# copy corpora files into appropriate directories:
corpora/%:
	../scripts/get-pdf-corpora.sh $*

# Note:
#  - dependency on corpora/name not encoded
#  - dependency on executable not encoded

test_%/test-summary: build
	${RUNTESTSET} --tool $(firstword $(subst _, ,$*)) \
                      --corpora $(lastword $(subst _, ,$*))

  # RUNTESTSET does make-like dependencies using Shake library
  #  - for details, see source here src/RunTestSet.hs

quick: ${QUICKTESTS}

long:  quick ${LONGTESTS}

build:
	cd ../..; cabal v2-build exe:run-testset

# turn 'all tests succeed' into single standard error code

quick-status: ${QUICKTESTS}
	test `egrep '^0 problems' $^ | wc -l` \
         -eq `echo $^ | wc -w`

long-status: ${QUICKTESTS} ${LONGTESTS}
	test `egrep '^0 problems' $^ | wc -l` \
         -eq `echo $^ | wc -w`
