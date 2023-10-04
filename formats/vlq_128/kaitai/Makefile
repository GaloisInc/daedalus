.PHONY: run clean

GEN=vlq_base128_le.cpp vlq_base128_le.h

REPO=git@github.com:kaitai-io/kaitai_struct_cpp_stl_runtime.git
RTS=rts
RUNTIME=kaitai_struct_cpp_stl_runtime
RUNTIME_FILE=$(RTS)/build/lib$(RUNTIME).so

parser: main.cpp $(GEN) $(RUNTIME_FILE)
	g++ -I$(RTS) -L$(RTS)/build -o parser \
	  main.cpp vlq_base128_le.cpp \
	  -l$(RUNTIME)

run: parser
	LD_LIBRARY_PATH=$(RTS)/build ./parser ../data.dat

$(GEN): vlq_128.ksy
	ksc --target=cpp_stl $<

$(RUNTIME_FILE):
	git clone $(REPO) $(RTS)
	cd $(RTS) && cmake -B build && cmake --build build

clean:
	rm -rf parser $(GEN) $(RTS)

