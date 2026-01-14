.PHONY: run clean

DDL=../../rts-c
 
CXXFLAGS=-std=c++20 -Wall -I $(DDL) `pkg-config --cflags gmpxx` -g -fsanitize=address
LDFLAGS=`pkg-config --libs gmpxx`

simple_app: simple_app.cpp parser/main_parser.h parser/main_parser.o
	clang++ $(CXXFLAGS) simple_app.cpp $(LDFLAGS) parser/main_parser.o -o $@ -fsanitize=address

parser/main_parser.h parser/main_parser.o: json.ddl utf8.ddl Daedalus.ddl
	cabal run exe:daedalus -- compile-c++ $< --out-dir=parser --entry=JSON_value_strict
	clang++ $(CXXFLAGS) -c parser/main_parser.cpp -o parser/main_parser.o -fsanitize=address

run:
	cabal run exe:export -- json.export --ddl json.ddl

clean:
	rm -rf parser simple_app