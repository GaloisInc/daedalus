.PHONY: run clean

include ../../utils/Makefile

run: parser
	@./parser > output
	@diff output expected

FILES=ddl/*.h include/custom_parser.h custom_parser.cpp main.cpp \
          ../../utils/mainWrapper.cpp

custom_parser.cpp include/custom_parser.h: test.ddl
	cabal run exe:daedalus --\
	  compile-c++ test.ddl \
	    --entry=Main \
	    --out-dir-headers=include \
	    --file-root=custom_parser

CPPFLAGS:=-Iinclude $(CPPFLAGS)

parser: $(FILES)
	$(CC) $(CPPFLAGS) $(CXXFLAGS) custom_parser.cpp main.cpp $(LIBS)

clean:
	-rm parser output custom_parser.cpp include/custom_parser.h
