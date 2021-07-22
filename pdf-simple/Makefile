.PHONY: build

DIR=c++

build:
	cabal exec daedalus -- \
	  --compile-c++ \
	  --out-dir="$(DIR)" \
	  Pdf.ddl
	make -C $(DIR)
