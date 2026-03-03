# Dex regression test suite.
#
# Each subdirectory is one test: spec.ddl + spec.dex + driver.cpp → binary
# that prints to stdout, diffed against expected.txt.
#
# Targets:
#   make           - build all tests
#   make clean     - remove build artifacts
#   make <name>    - build one test (e.g. make basic-struct)

# Auto-discover tests: any directory containing spec.ddl
TESTS := $(patsubst %/spec.ddl,%,$(wildcard */spec.ddl))

# Default target: build all tests
.DEFAULT_GOAL := all

# Paths relative to this directory
DAEDALUS_DIR := ../../..
DEX_DIR      := ../..
RTS_INCLUDE  := $(DAEDALUS_DIR)/rts-c
DEX_LIB      := $(DEX_DIR)/lib
DDL_LIB      := $(DAEDALUS_DIR)/lib
BUILDROOT    := build

CXX      := c++
CXXFLAGS := -std=c++20 -Wno-psabi -fsanitize=address,undefined -g -Wall
GMP_CFLAGS := $(shell pkg-config --cflags gmp 2>/dev/null)
GMP_LIBS   := $(shell pkg-config --libs gmpxx gmp 2>/dev/null || echo "-lgmpxx -lgmp")
LDFLAGS  := -fsanitize=address,undefined

# Absolute paths needed for daedalus/dex tools (they cd into daedalus/)
ABS_ROOT     := $(abspath .)
ABS_DDL_LIB  := $(abspath $(DDL_LIB))
ABS_DEX_LIB  := $(abspath $(DEX_LIB))
ABS_DAEDALUS := $(abspath $(DAEDALUS_DIR))

# Pattern rule: generate parser C++ from DDL
$(BUILDROOT)/%/main_parser.stamp: %/spec.ddl
	@mkdir -p $(BUILDROOT)/$*
	cd $(ABS_DAEDALUS) && cabal exec daedalus -- \
		--path=lib compile-c++ \
		--out-dir=$(ABS_ROOT)/$(BUILDROOT)/$* \
		--file-root=main_parser \
		--entry=Main \
		$(ABS_ROOT)/$*/spec.ddl
	@touch $@
$(BUILDROOT)/%/main_parser.cpp $(BUILDROOT)/%/main_parser.h: $(BUILDROOT)/%/main_parser.stamp ;

# Pattern rule: generate exporter C++ from dex
$(BUILDROOT)/%/exporter.stamp: %/spec.dex $(BUILDROOT)/%/main_parser.h
	cd $(ABS_DAEDALUS) && cabal exec dex -- \
		$(ABS_ROOT)/$*/spec.dex \
		--dex-path=$(ABS_DEX_LIB) \
		--ddl-path=$(ABS_ROOT)/$* \
		--ddl-path=lib \
		--output=$(ABS_ROOT)/$(BUILDROOT)/$*/exporter.cpp
	@touch $@
$(BUILDROOT)/%/exporter.cpp $(BUILDROOT)/%/exporter.h: $(BUILDROOT)/%/exporter.stamp ;

# Pattern rule: compile test binary
$(BUILDROOT)/%/driver: %/driver.cpp $(BUILDROOT)/%/main_parser.cpp $(BUILDROOT)/%/main_parser.h \
                       $(BUILDROOT)/%/exporter.cpp $(BUILDROOT)/%/exporter.h
	$(CXX) $(CXXFLAGS) $(GMP_CFLAGS) \
		-I$(RTS_INCLUDE) -I$(BUILDROOT)/$* \
		$(BUILDROOT)/$*/main_parser.cpp \
		$(BUILDROOT)/$*/exporter.cpp \
		$*/driver.cpp \
		$(LDFLAGS) $(GMP_LIBS) -o $@

# Per-test build targets
$(foreach t,$(TESTS),$(eval $(t): $(BUILDROOT)/$(t)/driver))

all: $(foreach t,$(TESTS),$(BUILDROOT)/$(t)/driver)

clean:
	rm -rf $(BUILDROOT)

# Keep all intermediate generated files (don't let Make auto-delete them)
.SECONDARY:

.PHONY: all test clean $(TESTS)
