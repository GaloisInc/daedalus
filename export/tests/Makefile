.PHONY: run clean

DDL=../../rts-c
 
CFLAGS=-std=c++20 -I $(DDL) `pkg-config --cflags gmpxx`
LDFLAGS=`pkg-config --libs gmpxx`

simple_app: simple_app.cpp parser/main_parser.h parser/main_parser.o
	clang++ $(CFLAGS) simple_app.cpp $(LDFLAGS) parser/main_parser.o -o $@

parser/main_parser.h parser/main_parser.o: simple_parser.ddl
	cabal run exe:daedalus -- compile-c++ $< --out-dir=parser --entry=Main
	clang++ $(CFLAGS) -c parser/main_parser.cpp -o parser/main_parser.o

run:
	cabal run exe:export -- simple.export --ddl simple_parser.ddl

clean:
	rm -rf parser simple_app