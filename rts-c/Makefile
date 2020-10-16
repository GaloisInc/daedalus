.PHONY: all

CPPFLAGS=-I.
CXXFLAGS=-std=c++17 -O2 -Wall
LIBS=-lgmpxx -lgmp

all:
	g++ $(CPPFLAGS) $(CXXFLAGS) test.cpp user_data.cpp $(LIBS)
