# Rust PDF COS example

This crate is the Rust counterpart of the adjacent C++ PDF COS example. It
uses the Daedalus specification in `../../pdf-cos-spec` and implements its native
functions and lazy object cache in Rust.

## Building

Generate the Rust parser and build the crate with:

```text
make
```

The generated parser is written to `src/pdfcos_parsers.rs` and is not checked
into the repository. It may be regenerated separately with:

```text
make generate
```

## Command-line example

List the references recorded in a PDF's cross-reference information:

```text
cargo run -- list FILE.pdf
```

Resolve and print a specific indirect object:

```text
cargo run -- resolve FILE.pdf OBJECT GENERATION
```

## Implemented functionality

The current implementation provides a COS-oriented API that can:

- Process traditional cross-reference tables and cross-reference streams.
- Follow incremental-update `Prev` links and hybrid-reference `XRefStm` links.
- Resolve uncompressed and object-stream entries lazily.
- Cache successfully parsed objects and failed resolutions.
- Detect recursive object resolution and limit reference depth.
- Expose the newest trailer, root reference, known references, and resolved
  COS objects.

The supported stream filters are:

- FlateDecode.
- LZWDecode, including `EarlyChange` values 0 and 1.
- ASCIIHexDecode.
- ASCII85Decode.
- PNG predictors 10 through 15, including all five PNG row algorithms.

ASCIIHexDecode and ASCII85Decode are implemented in the shared Daedalus
specification. FlateDecode, LZWDecode, and PNG predictor reversal are
implemented in Rust.

## Current limitations

- Encrypted PDFs are detected but are not supported. Streams are passed
  through unchanged only for unencrypted PDFs.
- TIFF Predictor 2 is not implemented.
- DCTDecode is passed through without validating or decoding JPEG data.
- Other stream filters are reported as unsupported.
- Parsed object-stream contents are not cached independently, so resolving
  several objects from one object stream reparses its index and body.
- The PDF validation and text-extraction layers are outside this crate's
  current scope.
- PDF-specific tests and corpus comparisons with the C++ implementation still
  need to be added.
