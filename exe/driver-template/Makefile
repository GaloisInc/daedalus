CPPFLAGS=-I.
CXXFLAGS=--std=c++17 -Wall -g -O0 -o parser
LIBS=-lgmpxx -lgmp
parser: ddl/*.h main_parser.h main_parser.cpp main.cpp
	clang++ $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp $(LIBS)

