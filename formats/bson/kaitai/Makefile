.PHONY: run build clean


build: parser

GEN=bson.cpp bson.h

REPO=https://github.com/kaitai-io/kaitai_struct_cpp_stl_runtime.git
RTS=rts
RUNTIME=kaitai_struct_cpp_stl_runtime
RUNTIME_FILE=$(RTS)/build/lib$(RUNTIME).so

parser: main.cpp $(GEN) $(RUNTIME_FILE)
	g++ -I$(RTS) -L$(RTS)/build -o parser \
	  -std=c++11 -O3 \
	  main.cpp bson.cpp \
	  -l$(RUNTIME)

run: parser
	LD_LIBRARY_PATH=$(RTS)/build ./parser ../data.dat

$(GEN): bson.ksy
	ksc --target=cpp_stl $<

$(RUNTIME_FILE):
	git clone $(REPO) $(RTS)
	patch -d $(RTS) -p1 < rts_patch
	cd $(RTS) && cmake -B build && cmake --build build

clean:
	rm -rf parser $(GEN) $(RTS)

