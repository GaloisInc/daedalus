.PHONY: clean

BUILD:=build
FILES:=main_parser.h main_parser.cpp
GENERATED:=$(patsubst %,$(BUILD)/%,$(FILES))

$(BUILD)/parser: main.cpp $(GENERATED)
	g++ -I. -I$(BUILD) \
		-o $@ \
		$< $(BUILD)/main_parser.cpp \
		-lboost_context

$(GENERATED): Example.ddl
	mkdir -p $(BUILD)
	cabal exec daedalus -- compile-c++ $< --out-dir=$(BUILD) --lazy-streams

clean:
	rm -rf $(BUILD)
