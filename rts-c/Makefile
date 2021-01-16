.PHONY: all

CPPFLAGS=-I.
CXXFLAGS=-std=c++17 -O2 -Wall
LIBS=-lgmpxx -lgmp
CC=clang++
FILES=rb_tests.cpp cast_tests.cpp test.cpp

all:
	$(CC) $(CPPFLAGS) $(CXXFLAGS) $(FILES) $(LIBS)
