.PHONY: run clean

include ./utils/Makefile

run: parser
	@./parser > output
	@diff output expected

FILES=./utils/ddl/*.h main_parser.h main_parser.cpp main.cpp ./utils/mainWrapper.cpp

main_parser.cpp main_parser.h: test.ddl
	./utils/daedalus compile-c++ test.ddl \
	  --entry=Main \
	  --user-namespace=Custom

CPPFLAGS:=-Iinclude $(CPPFLAGS)

parser: $(FILES)
	$(CC) $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp $(LIBS)

clean:
	-rm parser output main_parser.cpp main_parser.h
