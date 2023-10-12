.PHONY: clean

all: data.dat

data.dat: make_vlq.hs
	./make_vlq.hs

clean:
	rm -f data.dat
	make clean -C ddl
	make clean -C ddl-hs
	make clean -C atto
	make clean -C kaitai
