.PHONY: all generate clean

all: generate
	cargo build --workspace

generate:
	$(MAKE) -C pdfcos generate

clean:
	cargo clean
	rm -f pdfcos/src/pdfcos_parsers.rs
