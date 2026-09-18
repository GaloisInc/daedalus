//! Native functions used by the generated Daedalus parser.

use crate::filters::apply_predictor;
use crate::lzw;
use crate::pdfcos::Pdf;
use crate::pdfcos_parsers::{Ref, TopDecl};
use crate::resolve::resolve_reference_parser;
use daedalus_rts_rust as ddl;
use ddl::Type;
use flate2::read::ZlibDecoder;
use std::io::Read;

/// Implements `ResolveRef` from `pdf-cos-spec/PdfDecl.ddl:37`.
///
/// Looks up and parses an indirect PDF object through the reference table,
/// returning `nothing` when the cross-reference entry is absent.
pub fn resolve_ref(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: ddl::Input,
    reference: Ref,
) -> ddl::ParserResult<ddl::Maybe<TopDecl>> {
    resolve_reference_parser(state, input, reference)
}

/// Implements `Decrypt` from `pdf-cos-spec/PdfDecl.ddl:142`.
///
/// Decrypts a stream using the document encryption context and current object
/// number/generation, or returns the stream unchanged for an unencrypted file.
pub fn decrypt(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: ddl::Input,
    body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    let encrypted = state
        .user_state
        .trailer
        .as_ref()
        .is_some_and(|trailer| trailer.encrypt.is_just());

    if encrypted {
        native_failure(
            state,
            &input,
            "Decrypt",
            "encrypted PDF streams are not supported",
        )
    } else {
        ddl::ParserResult::Ok(body, input)
    }
}

/// Implements `FlateDecode` from `pdf-cos-spec/PdfDecl.ddl:219`.
///
/// Inflates a zlib-compressed stream and reverses its optional PNG predictor
/// using the supplied image parameters.
pub fn flate_decode(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: ddl::Input,
    predictor: ddl::Int,
    colors: ddl::Int,
    bits_per_component: ddl::Int,
    columns: ddl::Int,
    body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    let result: Result<ddl::Input, String> = (|| {
        let predictor = unsigned_parameter(&predictor, "Predictor")?;
        let colors = usize::try_from(unsigned_parameter(&colors, "Colors")?)
            .map_err(|_| "parameter Colors does not fit in usize".to_owned())?;
        let bits_per_component =
            usize::try_from(unsigned_parameter(&bits_per_component, "BitsPerComponent")?)
                .map_err(|_| "parameter BitsPerComponent does not fit in usize".to_owned())?;
        let columns = usize::try_from(unsigned_parameter(&columns, "Columns")?)
            .map_err(|_| "parameter Columns does not fit in usize".to_owned())?;

        let mut decoder = ZlibDecoder::new(body.as_bytes());
        let mut decoded = Vec::new();
        decoder
            .read_to_end(&mut decoded)
            .map_err(|error| format!("invalid Flate stream: {error}"))?;

        let decoded =
            apply_predictor(decoded, predictor, colors, bits_per_component, columns)?;

        Ok(ddl::new_input(
            ddl::new_byte_array(b"FlateDecode"),
            ddl::new_byte_array_vec(decoded),
        ))
    })();

    match result {
        Ok(decoded) => ddl::ParserResult::Ok(decoded, input),
        Err(error) => native_failure(state, &input, "FlateDecode", &error),
    }
}

/// Implements `LZWDecode` from `pdf-cos-spec/PdfDecl.ddl:248`.
///
/// Decompresses a PDF LZW stream, honoring `EarlyChange`, and reverses its
/// optional PNG predictor using the supplied image parameters.
pub fn lzw_decode(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: ddl::Input,
    predictor: ddl::Int,
    colors: ddl::Int,
    bits_per_component: ddl::Int,
    columns: ddl::Int,
    early_change: ddl::Int,
    body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {

    // Wrap in lambda so we can use ? at Result locally.
    let result: Result<ddl::Input, String> = (|| {
        let predictor = unsigned_parameter(&predictor, "Predictor")?;
        let colors = usize::try_from(unsigned_parameter(&colors, "Colors")?)
            .map_err(|_| "parameter Colors does not fit in usize".to_owned())?;
        let bits_per_component =
            usize::try_from(unsigned_parameter(&bits_per_component, "BitsPerComponent")?)
                .map_err(|_| "parameter BitsPerComponent does not fit in usize".to_owned())?;
        let columns = usize::try_from(unsigned_parameter(&columns, "Columns")?)
            .map_err(|_| "parameter Columns does not fit in usize".to_owned())?;
        let early_change = u8::try_from(unsigned_parameter(&early_change, "EarlyChange")?)
            .map_err(|_| "parameter EarlyChange does not fit in u8".to_owned())?;

        let decoded = lzw::decode(body.as_bytes(), early_change)?;
        let decoded =
            apply_predictor(decoded, predictor, colors, bits_per_component, columns)?;

        Ok(ddl::new_input(
            ddl::new_byte_array(b"LZWDecode"),
            ddl::new_byte_array_vec(decoded),
        ))
    })();

    match result {
        Ok(decoded) => ddl::ParserResult::Ok(decoded, input),
        Err(error) => native_failure(state, &input, "LZWDecode", &error),
    }
}

fn unsigned_parameter(value: &ddl::Int, name: &str) -> Result<u64, String> {
    value
        .try_to_unsigned()
        .ok_or_else(|| format!("parameter {name} is not an unsigned integer"))
}

fn native_failure<T>(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    primitive: &str,
    message: &str,
) -> ddl::ParserResult<T> {
    let message = ddl::new_byte_array(message.as_bytes());
    state.note_fail(true, primitive, input, message.bor());
    ddl::ParserResult::Failure
}
