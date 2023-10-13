.PHONY: clean run


./parser: Main.ddl BSON.ddl
	cabal exec daedalus -- compile-c++ $< --out-dir=out
	make -C out
	cp out/parser .

run: parser
	./parser ../data.dat

clean:
	rm -rf parser out
