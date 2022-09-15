.PHONY: build run clean example0 example1 example2

# IMPORTANT:
# Assumes that `ddl` directory is in the ecurrent directory

run: build
	###################
	# Running Example 0
	###################
	./build/example0 inputs/joined
	###################
	# Running Example 1
	###################
	./build/example1 inputs/packet1 inputs/packet1 inputs/packet2
	###################
	# Running Example 2
	###################
	./build/example2 inputs/packet1 inputs/packet1 inputs/packet2
	###################
	# Running Example 3
	###################
	./build/example3 inputs/packet1 inputs/packet1 inputs/packet2

build: example0 example1 example2 example3

example0:
	###########
	# Example 0
	###########
	# We use Daedalus to generate the C++ parser, and a sample driver
	cabal exec daedalus -- \
	  compile-c++ \
	  Example0 \
	  --out-dir=build/ddl/example0
	#
	# Then we build the executable
	make -C build/ddl/example0
	cp build/ddl/example0/parser build/example0

example1:
	###########
	# Example 1
	###########
	# First we use Daedalus to generate the C++ for the parser
	cabal exec daedalus -- \
	  compile-c++ \
	  Example1 \
	  --out-dir=build/ddl/example1 \
	  --entry=Packet
	#
	# Then we build the executable
	g++ -I. \
	    src/example1-driver.cpp \
	    build/ddl/example1/main_parser.cpp \
	    -o build/example1

example2:
	###########
	# Example 2
	###########
	# First we use Daedalus to generate the C++ for the parser
	cabal exec daedalus -- \
	  compile-c++ \
	  Example2 \
	  --out-dir=build/ddl/example2 \
	  --entry=Protocol
	#
	# Then we build the executable
	g++ -I. \
	    src/example2-driver.cpp \
	    build/ddl/example2/main_parser.cpp \
	    -o build/example2

example3:
	###########
	# Example 3
	###########
	# First we use Daedalus to generate the C++ for the parser
	cabal exec daedalus -- \
	  compile-c++ \
	  Example2 \
	  --out-dir=build/ddl/example3 \
	  --entry=Protocol \
	  --add-include='"example3-driver.h"' \
	  --user-state=CustomState
	#
	# Then we build the executable
	g++ -I. -Isrc \
	    src/example3-driver.cpp \
	    build/ddl/example3/main_parser.cpp \
	    -o build/example3




clean:
	rm -rf build

