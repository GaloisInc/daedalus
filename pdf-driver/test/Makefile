TOP=../..
  # top of daedalus repo
SHAKEOPTS?=-j16
  # works like Make's -j
RUNTESTSET_TIMEOUT_IN_SECS?=240
  # timeout : corp         : no. file timeouts
  # 240     : 2020-03-eval : ~8
  # 600     : 2020-03-eval : 2

RUNTESTSET?=cabal v2-run -- run-testset \
            ${SHAKEOPTS} -t ${RUNTESTSET_TIMEOUT_IN_SECS}

nodefaulttarget:
	@echo no default target, did you want ...?
	@echo " " make quick "         # 1 min check of local pdfs"
	@echo " " make remotecorpora " # rsync remote corporas to here"
	@echo " " make long "          # 20 min check of larger corporas"


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
	${TOP}/pdf-driver/scripts/get-pdf-corpora.sh $*


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

build:  ${TOP}/pdf-driver/test/src/run-testset/*.hs
	cd ${TOP}; cabal v2-build exe:run-testset

# turn 'all tests succeed' into an error code:

quick-status: ${QUICKTESTS}
	test `egrep '^0 unexpected variances' $^ | wc -l` \
         -eq `echo $^ | wc -w`

long-status: ${LONGTESTS}
	test `egrep '^0 unexpected variances' $^ | wc -l` \
         -eq `echo $^ | wc -w`

