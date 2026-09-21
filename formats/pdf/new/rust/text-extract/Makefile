DAEDALUS ?= ../../../../../bin/daedalus

COS_SPEC_DIR = ../../pdf-cos-spec
TEXT_SPEC_DIR = ../../pdf-text-extract-spec
SPEC = $(TEXT_SPEC_DIR)/TextExtract.ddl
SPEC_SOURCES = $(wildcard $(TEXT_SPEC_DIR)/*.ddl)
PARSER = src/text_extract_parsers.rs

.PHONY: all generate clean

all: $(PARSER)
	cargo build

generate: $(PARSER)

$(PARSER): $(SPEC_SOURCES)
	$(DAEDALUS) \
		--path=$(COS_SPEC_DIR) \
		--path=$(TEXT_SPEC_DIR) \
		compile-rust $(SPEC) \
		--determinize \
		--inline-this=StandardEncodings.glyph \
		--output-file=$(PARSER) \
		--entry=StandardEncodings.StdEncodings \
		--entry=Catalog.PdfPageCount \
		--entry=Catalog.PdfCatalog \
		--entry=TextExtract.TextInCatalogPage \
		--user-state=crate::TextExtractState \
		--user-fun=crate::native \
		--extern=PdfASCII:daedalus_pdf_cos::pdfcos_parsers \
		--extern=PdfValue:daedalus_pdf_cos::pdfcos_parsers \
		--extern=PdfDecl:daedalus_pdf_cos::pdfcos_parsers \
		--extern=PdfXRef:daedalus_pdf_cos::pdfcos_parsers \
		--extern=PdfCrypto:daedalus_pdf_cos::pdfcos_parsers

clean:
	cargo clean
	rm -f $(PARSER)
