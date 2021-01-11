CPPFLAGS=-I.
CXXFLAGS=--std=c++17 -Wall -O3 -pg -o parser
LIBS=-lgmpxx -lgmp
parser: ddl/*.h main_parser.h main_parser.cpp main.cpp
	g++ $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp $(LIBS)

