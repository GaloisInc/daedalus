.PHONY: all

all: parser docs

CPPFLAGS=-I.
CXXFLAGS=--std=c++17 -Wall -O3 -o parser
LIBS=-lgmpxx -lgmp

FILES=ddl/*.h main_parser.h main_parser.cpp main.cpp

parser: $(FILES)
	g++ $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp $(LIBS)

docs: $(FILES)
	doxygen Doxyfile

