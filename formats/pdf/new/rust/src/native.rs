//! Native functions used by the generated Daedalus parser.

use crate::pdfcos::{ReferenceTable, resolve_reference};
use crate::pdfcos_parsers::{Ref, TopDecl};
use daedalus_rts_rust as ddl;

/// Implements `ResolveRef` from `pdf-cos-spec/PdfDecl.ddl:37`.
///
/// Looks up and parses an indirect PDF object through the reference table,
/// returning `nothing` when the cross-reference entry is absent.
pub fn resolve_ref(
    state: &mut ddl::ParserStateWith<ReferenceTable>,
    input: ddl::Input,
    reference: Ref,
) -> ddl::ParserResult<ddl::Maybe<TopDecl>> {
    resolve_reference(state, input, reference)
}

/// Implements `Decrypt` from `pdf-cos-spec/PdfDecl.ddl:142`.
///
/// Decrypts a stream using the document encryption context and current object
/// number/generation, or returns the stream unchanged for an unencrypted file.
pub fn decrypt(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

/// Implements `FlateDecode` from `pdf-cos-spec/PdfDecl.ddl:219`.
///
/// Inflates a zlib-compressed stream and reverses its optional TIFF or PNG
/// predictor using the supplied image parameters.
pub fn flate_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _predictor: ddl::Int,
    _colors: ddl::Int,
    _bits_per_component: ddl::Int,
    _columns: ddl::Int,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

/// Implements `LZWDecode` from `pdf-cos-spec/PdfDecl.ddl:248`.
///
/// Decompresses a PDF LZW stream, honoring `EarlyChange`, and reverses its
/// optional TIFF or PNG predictor using the supplied image parameters.
pub fn lzw_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _predictor: ddl::Int,
    _colors: ddl::Int,
    _bits_per_component: ddl::Int,
    _columns: ddl::Int,
    _early_change: ddl::Int,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

/// Implements `ASCIIHexDecode` from `pdf-cos-spec/PdfDecl.ddl:257`.
///
/// Converts the stream's hexadecimal text representation back into bytes.
pub fn ascii_hex_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

/// Implements `ASCII85Decode` from `pdf-cos-spec/PdfDecl.ddl:260`.
///
/// Converts the stream's base-85 text representation back into bytes.
pub fn ascii85_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}
