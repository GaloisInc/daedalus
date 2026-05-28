.PHONY: clean

BUILD:=build
FILES:=main_parser.h main_parser.cpp
GENERATED:=$(patsubst %,$(BUILD)/%,$(FILES))

BOOST_CFLAGS ?= $(shell pkg-config --cflags boost_context 2>/dev/null || brew --prefix boost >/dev/null 2>&1 && echo "-I$$(brew --prefix boost)/include")
BOOST_LIBS   ?= $(shell pkg-config --libs boost_context 2>/dev/null || brew --prefix boost >/dev/null 2>&1 && echo "-L$$(brew --prefix boost)/lib -lboost_context" || echo "-lboost_context")
GMP_CFLAGS   ?= $(shell pkg-config --cflags gmpxx 2>/dev/null)
GMP_LIBS     ?= $(shell pkg-config --libs gmpxx 2>/dev/null || echo "-lgmpxx -lgmp")

$(BUILD)/parser: main.cpp $(GENERATED)
	g++ -std=c++20 -I../../rts-c -I. -I$(BUILD) $(BOOST_CFLAGS) $(GMP_CFLAGS) \
		-o $@ \
		$< $(BUILD)/main_parser.cpp \
		$(BOOST_LIBS) $(GMP_LIBS)

$(GENERATED): Example.ddl
	mkdir -p $(BUILD)
	../../bin/daedalus compile-c++ $< --out-dir=$(BUILD) --lazy-streams

clean:
	rm -rf $(BUILD)
