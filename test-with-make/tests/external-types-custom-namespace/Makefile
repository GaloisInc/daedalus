.PHONY: run clean

include ./utils/Makefile

run: parser
	@./parser > output
	@diff output expected

FILES=./utils/ddl/*.h \
      P1/main_parser.h P1/main_parser.cpp \
      P2/main_parser.h P2/main_parser.cpp \
      ../../utils/mainWrapper.cpp

P1/main_parser.cpp P1/main_parser.h: P1.ddl
	mkdir -p P1
	./utils/daedalus compile-c++ P1.ddl \
	    --user-namespace=Custom \
	    --entry=P1 \
	    --out-dir=P1

P2/main_parser.cpp P2/main_parser.h: P2.ddl
	mkdir -p P2
	./utils/daedalus compile-c++ P2.ddl \
	    --entry=P2 \
	    --out-dir=P2 \
	    --extern=P1:Custom \
	    --add-include='"../P1/main_parser.h"'


parser: $(FILES)
	$(CC) $(CPPFLAGS) $(CXXFLAGS) \
	    main.cpp P2/main_parser.cpp \
	    $(LIBS)

clean:
	-rm -rf parser output P1 P2
