.PHONY: run clean

DDL=../../../rts-c
DEX=../../lib
 
CXXFLAGS=-std=c++20 -Wall -I $(DDL) -I. `pkg-config --cflags gmpxx` -g -fsanitize=address
LDFLAGS=`pkg-config --libs gmpxx`

simple_app: simple_app.cpp parser/main_parser.h parser/main_parser.o parser/exporter.o
	clang++ $(CXXFLAGS) simple_app.cpp $(LDFLAGS) parser/main_parser.o parser/exporter.o -o $@ -fsanitize=address

parser/exporter.o: json.dex parser/main_parser.h
	cabal run exe:dex json.dex -- --dex-path="$(DEX)" --output=parser/exporter.cpp
	clang++ $(CXXFLAGS) -c parser/exporter.cpp -o $@ -fsanitize=address

parser/main_parser.h parser/main_parser.o: json.ddl utf8.ddl Daedalus.ddl
	cabal run exe:daedalus -- compile-c++ $< --out-dir=parser --entry=JSON_value_strict
	clang++ $(CXXFLAGS) -c parser/main_parser.cpp -o parser/main_parser.o -fsanitize=address

clean:
	rm -rf parser simple_app simple_app.dSYM