.PHONY: all generate clean

all: generate
	cargo build --workspace

generate:
	$(MAKE) -C pdfcos generate
	$(MAKE) -C text-extract generate

clean:
	cargo clean
	rm -f pdfcos/src/pdfcos_parsers.rs
	rm -f text-extract/src/text_extract_parsers.rs
