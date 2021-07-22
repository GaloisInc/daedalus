SPEC_DIR:=../spec
SPEC:=$(SPEC_DIR)/PdfCos.ddl
ENTRIES:=PdfXRef.PdfEnd PdfXRef.CrossRef PdfDecl.TopDecl

RTS_DIR:=../../rts-c
OUT_DIR:=generated
GEN:=$(OUT_DIR)/main_parser.cpp $(OUT_DIR)/main_parser.h
CC:=g++
CPPFLAGS:=-I $(OUT_DIR) -I $(RTS_DIR)
WARNS:=-Wall -Wno-uninitialized
CXXFLAGS:=--std=c++17 $(WARNS) -g -O1
# CXXFLAGS:=--std=c++17 $(WARNS) -O1 -g -fsanitize=address
# LIBS:=-lgmpxx -lgmp -fsanitize=address -static-libasan
LIBS:=-lgmpxx -lgmp


ENTRY_FLAGS:=$(foreach e,$(ENTRIES),--entry=$(e))

.PHONY: clean

parser-test: src/main.cpp src/primitives.cpp main_parser.o
	$(CC) $(CPPFLAGS) $(CXXFLAGS) \
              src/main.cpp \
              src/primitives.cpp \
              main_parser.o -o $@ $(LIBS)


main_parser.o: $(GEN) $(wildcard $(RTS_DIR)/*.h)
	$(CC) -c $(CPPFLAGS) $(CXXFLAGS) $(OUT_DIR)/main_parser.cpp

$(GEN): $(SPEC) $(wildcard $(SPEC_DIR)/*.ddl)
	mkdir -p $(OUT_DIR)
	cabal exec daedalus -- \
	  --compile-c++ \
	  --out-dir=$(OUT_DIR) \
	  $(ENTRY_FLAGS) \
	  $<

out.core: $(SPEC)
	cabal exec daedalus -- --dump-core $< $(ENTRY_FLAGS) > $@

out.vm : $(SPEC)
	cabal exec daedalus -- --dump-vm $< $(ENTRY_FLAGS) > $@



clean:
	rm -rf $(OUT_DIR)
