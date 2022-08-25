.PHONY: run clean

include ./utils/Makefile

run: parser
	@./parser > output
	@diff output expected

FILES=./utils/ddl/*.h ./utils/mainWrapper.cpp \
      main_parser.h main_parser.cpp main.cpp prim.cpp userState.hpp \

main_parser.cpp main_parser.h: test.ddl
	./utils/daedalus compile-c++ test.ddl \
	    --entry=Main \
	    --user-state=UserState \
	    --add-include='"userState.hpp"'

parser: $(FILES)
	$(CC) $(CPPFLAGS) $(CXXFLAGS) main_parser.cpp main.cpp prim.cpp $(LIBS)

clean:
	-rm parser output main_parser.cpp main_parser.h
