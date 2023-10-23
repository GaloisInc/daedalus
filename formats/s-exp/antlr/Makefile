.PHONY: clean run

run: parser
	./parser ../data.dat

build: parser

clean:
	rm -rf build parser

BUILD=build
GEN_MOD=sexprLexer sexprParser sexprVisitor sexprBaseVisitor
GEN_CPP=$(GEN_MOD:%=$(BUILD)/%.cpp)
GEN=$(GEN_CPP) $(GEN_CPP:.cpp=.h)

ANTLR=antlr4
ANTLR_RTS_INC=/usr/include/antlr4-runtime
ANTLR_RTS_LIB=/usr/lib/x86_64-linux-gnu/

parser: $(GEN) main.cpp
	g++ -std=c++17 -static -O3 \
		-I $(BUILD) -I $(ANTLR_RTS_INC) \
		main.cpp $(GEN_CPP) \
		-L $(ANTLR_RTS_LIB) -lantlr4-runtime \
		-o $@

$(GEN): sexpr.g4
	$(ANTLR) $< -Dlanguage=Cpp -visitor -no-listener -o $(BUILD)


