# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
CABAL         = $(shell which cabal)

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: github help Makefile

github:
	make html
	mkdir -p ../docs
	cp -r build/html/* ../docs/

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
#
# After extracting the examples and building the docs, we run daedalus
# on the extracted example files to ensure that they're valid. This assumes daedalus
%: Makefile
	rm -rf build/downloads
	rm -f build/examples.tar.gz
	python3 build_examples.py
	tar czf build/examples.tar.gz -C build/downloads/source examples
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

	cd .. && $(CABAL) build exe:daedalus
	cd .. && for f in $$(find docs-src/build -name \*.ddl); do \
		echo "Testing $$f with daedalus:" ; \
		$(CABAL) exec daedalus -- --path=lib/ $$f; \
		done
