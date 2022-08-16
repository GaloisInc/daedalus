.PHONY: run clean

run: parser
	@./parser input > output
	@diff output expected

CC=g++
CPPFLAGS=-I.
WARNS=-Wall -Wno-uninitialized
CXXFLAGS=--std=c++17 $(WARNS) -O3 -o parser
LIBS=-lgmpxx -lgmp

FILES=ddl/*.h main_parser.h main_parser.cpp main.cpp prim.cpp

main_parser.cpp main_parser.h: test.ddl
	cabal run exe:daedalus --\
	  compile-c++ test.ddl --entry=Main --user-state=int

parser: $(FILES)
	$(CC) $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp prim.cpp $(LIBS)

clean:
	-rm parser output main_parser.cpp main_parser.h
