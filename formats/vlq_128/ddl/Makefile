.PHONY: clean run


./parser: vlq_128.ddl
	cabal exec daedalus -- compile-c++ $< --out-dir=out
	make -C out
	cp out/parser .

run: parser
	./parser ../data.dat
clean:
	rm -rf parser out
