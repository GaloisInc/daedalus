# maybe one day the cabal build will morph into this ... but just to add a few helpful targets

all:
	echo there is no default target, cabal is used to build


# The moving of generated Haskell files from place to place can play havoc with
# the build, let's be safe and delete old or generated files that might get in the way:
#   CAVEAT: this may delete some files not committed to the repo!!
clean-hs:
	rm `git ls-files --other pdf-cos/src/*.hs`
	rm pdf-cos/src/spec/*.hs
	rm pdf-driver/src/spec/*.hs
