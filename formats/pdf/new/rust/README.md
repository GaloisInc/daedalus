# Rust PDF COS example

This crate is the Rust counterpart of the adjacent C++ PDF COS example. It
uses the Daedalus specification in `../pdf-cos-spec` and implements its native
functions and lazy object cache in Rust.

The crate is currently a skeleton. Generate the parser with:

```text
make generate
```

Once the native primitives have been implemented, build the example with:

```text
make
```
