DAEDALUS=../../bin/daedalus
PARSER_DIR=parser
PARSER_FILES=$(PARSER_DIR)/format.cpp $(PARSER_DIR)/format.h

CXXFLAGS=--std=c++20 -I . -I $(PARSER_DIR) $(shell pkg-config --cflags gmpxx)
LDFLAGS=$(shell pkg-config --libs gmpxx)  

example: main.cpp $(PARSER_FILES)
	g++ -o $@ $(CXXFLAGS) $< $(PARSER_DIR)/format.cpp $(LD_FLAGS)

$(PARSER_FILES): format.ddl
	$(DAEDALUS) compile-c++ format.ddl \
	    --save-rts \
		--out-dir=$(PARSER_DIR) --entry=Packets --file-root=format \
		--user-state=State --add-include='"state.h"'

PHONY: clean

clean:
	rm -rf $(PARSER_DIR)