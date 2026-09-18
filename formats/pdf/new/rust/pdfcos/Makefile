DAEDALUS ?= ../../../../../bin/daedalus

SPEC_DIR = ../../pdf-cos-spec
SPEC = $(SPEC_DIR)/PdfCos.ddl
SPEC_SOURCES = $(wildcard $(SPEC_DIR)/*.ddl)
PARSER = src/pdfcos_parsers.rs

.PHONY: all generate clean

all: $(PARSER)
	cargo build

generate: $(PARSER)

$(PARSER): $(SPEC_SOURCES) src/native.rs src/pdfcos.rs
	$(DAEDALUS) compile-rust $(SPEC) \
		--determinize \
		--output-file=$(PARSER) \
		--entry=PdfXRef.PdfEnd \
		--entry=PdfXRef.CrossRef \
		--entry=PdfDecl.TopDecl \
		--entry=PdfDecl.ObjStream \
		--entry=PdfDecl.ObjStreamEntry \
		--entry=PdfCrypto.EncryptionDict \
		--user-state=crate::pdfcos::Pdf \
		--user-fun=crate::native

clean:
	cargo clean
	rm -f $(PARSER)
