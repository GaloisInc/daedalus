.PHONY: all

all: parser

CC=g++
CPPFLAGS=-I. $(shell pkg-config --cflags gmpxx)
WARNS=-Wall -Wno-uninitialized
CXXFLAGS=--std=c++17 $(WARNS) -O3 -o parser
LIBS=$(shell pkg-config --libs gmpxx)

FILES=ddl/*.h main_parser.h main_parser.cpp main.cpp

parser: $(FILES)
	$(CC) $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp $(LIBS)

docs: $(FILES)
	doxygen Doxyfile

