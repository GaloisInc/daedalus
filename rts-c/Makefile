.PHONY: all

CPPFLAGS=-I.
CXXFLAGS=-std=c++17 -O2 -Wall -fsanitize=address
LIBS=-lgmpxx -lgmp -static-libsan
CC=clang++
FILES=test.cpp

all:
	$(CC) $(CPPFLAGS) $(CXXFLAGS) $(FILES) $(LIBS)
