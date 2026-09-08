DAEDALUS ?= ../../bin/daedalus

FORMAT = format.ddl
PARSER = src/format.rs
SAMPLE_INPUT = ../stateful-parser-example/sample-input.txt

.PHONY: example run clean

example: $(PARSER)
	cargo build

run: $(PARSER)
	cargo run -- $(SAMPLE_INPUT)

$(PARSER): $(FORMAT) src/native.rs
	$(DAEDALUS) compile-rust $(FORMAT) \
		--determinize \
		--output-file=$(PARSER) \
		--entry=Packets \
		--user-state=crate::native::State \
		--user-fun=crate::native

clean:
	rm -rf target $(PARSER) parser
