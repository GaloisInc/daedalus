.PHONY: all clean kaitai/parser

all: data.dat ddl/parser kaitai/parser

data.dat: make_vlq.hs
	./make_vlq.hs

ddl/parser:
	make -C ddl

kaitai/parser:
	make -C kaitai

clean:
	rm -f data.dat
	make clean -C ddl
	make clean -C kaitai
