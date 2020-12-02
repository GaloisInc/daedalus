CPPFLAGS=-I include
CXXFLAGS=--std=c++17 -Wall -O3 -pg
LIBS=-lgmpxx -lgmp
a.out: include/ddl/*.h main_parser.h main_parser.cpp main.cpp
	g++ $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp $(LIBS) 2> build-errors; cat build-errors
