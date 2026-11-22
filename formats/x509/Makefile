# Build and test the X.509 example.
#
# By default this uses `daedalus` from the PATH; from a source checkout
# you can use `make DAEDALUS="cabal exec daedalus --"` instead.

DAEDALUS ?= daedalus

# Written verbatim into gen/Cargo.toml, so this is relative to gen/.
RTS_PATH := ../../../rts-rust

SAMPLE := tests/certs/rsa2048.der

.PHONY: all gen test interp smoke regen-certs clean

all: test

gen:
	$(DAEDALUS) compile-rust --determinize --out-dir=gen --rts-path=$(RTS_PATH) x509.ddl

test: gen
	cd rust && cargo test

# Quick check with the Daedalus interpreter (no code generation needed).
interp:
	$(DAEDALUS) run x509.ddl -i $(SAMPLE) --json

# Run the CLI driver that compile-rust generates alongside the library.
smoke: gen
	cd gen && cargo run --quiet -- ../$(SAMPLE)

# Regenerate the committed certificate corpus (requires openssl; the
# pinned assertions in rust/tests/cert_tests.rs must be re-pinned after).
regen-certs:
	cd tests/certs && ./generate.sh

clean:
	rm -rf gen rust/target rust/Cargo.lock
