.PHONY: run clean

DAEDALUS = ../../utils/daedalus

run: P2/src/generated.rs
	@cargo run --quiet --manifest-path P2/Cargo.toml > output
	@diff output expected

P1/src/lib.rs: P1.ddl P1/Cargo.toml
	mkdir -p P1/src
	$(DAEDALUS) compile-rust P1.ddl \
	    --determinize \
	    --output-file=P1/src/lib.rs \
	    --entry=P1

P2/src/generated.rs: P2.ddl P1/src/lib.rs P2/Cargo.toml P2/src/main.rs
	$(DAEDALUS) compile-rust P2.ddl \
	    --determinize \
	    --output-file=P2/src/generated.rs \
	    --entry=P2 \
	    --extern=P1:p1

clean:
	-rm -rf output P1/src/lib.rs P2/src/generated.rs P2/Cargo.lock P2/target
