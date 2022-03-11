SHAKEOPTS?=-j16
  # works like Make's -j
RUNTESTSET_TIMEOUT?=-t 240
RUNTESTSET?=cabal v2-run -- run-testset ${SHAKEOPTS} ${RUNTESTSET_TIMEOUT}

nodefaulttarget:
	@echo no default target, did you want ...?
	@echo " " make quick "         # 5 min check of local pdfs"
	@echo " " make remotecorpora " # rsync remote corporas to here"
	@echo " " make long "          # 2 hour check of larger corporas"


QUICKTESTS=test_validatePDF_generated/test-summary     \
           test_validatePDF_misc/test-summary          \
           test_validatePDF_pdf20examples/test-summary \
           test_validatePDF_govdocs-subset-a/test-summary    

LONGTESTS=test_validatePDF_2020-03-eval/test-summary

# copy corpora files into appropriate directories:
#  - the rule action is a no-op if accidentally invoked on existing directory.

REMOTECORPORA= corpora/pdf20examples \
               corpora/2020-03-eval  \
               corpora/govdocs-subset-a

remotecorpora: ${REMOTECORPORA}

corpora/%:
	../scripts/get-pdf-corpora.sh $*


test_%/test-summary: build
	${RUNTESTSET} --tool $(firstword $(subst _, ,$*)) \
                      --corpora $(lastword $(subst _, ,$*))

  # RUNTESTSET does make-like dependencies using Shake library
  #  - for details, see source here src/RunTestSet.hs

test_%/clean: build
	${RUNTESTSET} --tool $(firstword $(subst _, ,$*)) \
                      --corpora $(lastword $(subst _, ,$*)) \
                      clean

CMAPTESTS=test_cmap-cid_cmaps-cid/test-summary \
          test_cmap-sf_cmaps-simple/test-summary

cmaps: ${CMAPTESTS}

quick: ${QUICKTESTS}

long:  ${LONGTESTS}

quick-clean: $(QUICKTESTS:test-summary=clean)

long-clean:  $(LONGTESTS:test-summary=clean)


build:
	cd ../..; cabal v2-build exe:run-testset

# turn 'all tests succeed' into an error code:

quick-status: ${QUICKTESTS}
	test `egrep '^0 unexpected variances' $^ | wc -l` \
         -eq `echo $^ | wc -w`

long-status: ${LONGTESTS}
	test `egrep '^0 unexpected variances' $^ | wc -l` \
         -eq `echo $^ | wc -w`

