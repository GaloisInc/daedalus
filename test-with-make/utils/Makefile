CC=g++
CPPFLAGS=-I. -Iutils $(shell pkg-config -cflags gmpxx)
WARNS=-Wall -Wno-uninitialized
CXXFLAGS=--std=c++17 $(WARNS) -O3 -o parser
LIBS=$(shell pkg-config --libs gmpxx)
