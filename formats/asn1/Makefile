# Build and test the ASN.1 example.
#
# By default this uses `daedalus` from the PATH; from a source checkout
# you can use `make DAEDALUS="cabal exec daedalus --"` instead.

DAEDALUS ?= daedalus

# Written verbatim into gen/Cargo.toml, so this is relative to gen/.
RTS_PATH := ../../../rts-rust

.PHONY: all gen test interp smoke clean

all: test

gen:
	$(DAEDALUS) compile-rust --determinize --out-dir=gen --rts-path=$(RTS_PATH) asn1.ddl

test: gen
	cd rust && cargo test

# AlgorithmIdentifier for sha256WithRSAEncryption (an X.509 fragment).
# Bytes 30 0d 06 09 2a 86 48 86 f7 0d 01 01 0b 05 00, as octal escapes
# because POSIX printf has no \xHH.
sample.der:
	printf '\060\015\006\011\052\206\110\206\367\015\001\001\013\005\000' > $@

# Quick check with the Daedalus interpreter (no code generation needed).
interp: sample.der
	$(DAEDALUS) run asn1.ddl -i sample.der --json

# Run the CLI driver that compile-rust generates alongside the library.
smoke: gen sample.der
	cd gen && cargo run --quiet -- ../sample.der

clean:
	rm -rf gen sample.der rust/target rust/Cargo.lock
