RUNTESTCMD=cabal v2-run -- run-testset -j8

  # note the '-j8' this is Shake option that works like Make's -j

QUICKTESTS=test_validatePDF_generated/test-summary \
           test_validatePDF_misc/test-summary
LONGTESTS=test_validatePDF_2020-03-eval/test-summary

test_%/test-summary: build
	${RUNTESTCMD} --tool $(firstword $(subst _, ,$*)) \
                      --corpora $(lastword $(subst _, ,$*))

quick: ${QUICKTESTS}

long:  quick ${LONGTESTS}

build:
	cd ../..; cabal v2-build exe:run-testset


# turn 'all tests succeed' into single standard error code

quick-status: ${QUICKTESTS}
	test `egrep '^0 problem.s.:' $^ | wc -l` \
         -eq `echo $^ | wc -w`

long-status: ${QUICKTESTS} ${LONGTESTS}
	test `egrep '^0 problem.s.:' $^ | wc -l` \
         -eq `echo $^ | wc -w`
