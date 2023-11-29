.PHONY: clean run bulid

build: parser

./parser: Main.ddl SExp.ddl
	cabal exec daedalus -- compile-c++ --determinize $< --out-dir=out
	make -C out
	cp out/parser .

run: parser
	./parser ../data.dat

clean:
	rm -rf parser out
