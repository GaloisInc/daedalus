.PHONY: clean list-cavities test

IGNORED=cavity-PRU-1-core.ddl cavity-IUU-3.ddl
CAVITIES=$(filter-out ${IGNORED}, $(wildcard cavity-*.ddl))

list-cavities:
	@echo $(CAVITIES)

# Evaluate each Daedalus cavity specification using its corresponding .txt
# input.  Fails if the input doesn't parse.
test: $(CAVITIES:.ddl=.test)
%.test: %.ddl %.txt
	@echo --- $@
	daedalus run --input $(word 2, $^) $< 2>&1 > $@

clean:
	rm *.test
