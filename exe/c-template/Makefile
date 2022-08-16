.PHONY: all

all: parser

CC=g++
CPPFLAGS=-I.
WARNS=-Wall -Wno-uninitialized
CXXFLAGS=--std=c++17 $(WARNS) -O3 -o parser
LIBS=-lgmpxx -lgmp

FILES=ddl/*.h main_parser.h main_parser.cpp main.cpp

parser: $(FILES)
	$(CC) $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp $(LIBS)

docs: $(FILES)
	doxygen Doxyfile

