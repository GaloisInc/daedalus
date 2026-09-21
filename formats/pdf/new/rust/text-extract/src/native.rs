//! Native functions used by the generated text-extraction parser.

#![allow(non_snake_case)]

use crate::TextExtractState;
use crate::text_extract_parsers::CMap::cmap;
use daedalus_pdf_cos::{Ref, TopDecl};
use daedalus_rts_rust as ddl;

/// Resolve an indirect object using the PDF COS parser state.
pub fn ResolveRef(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    reference: Ref,
) -> ddl::ParserResult<ddl::Maybe<TopDecl>> {
    let result = daedalus_pdf_cos::native::ResolveRef(&mut state.user_state.pdf, input, reference);

    if matches!(
        result,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception
    ) {
        state.error = state.user_state.pdf.error.clone();
    }

    result
}

/// Read a character code whose fixed-width value is in a CMap codespace range.
pub fn GetCharCode(
    _state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    cmap: cmap,
) -> ddl::ParserResult<ddl::I<32>> {
    if input.is_empty() || cmap.ranges.is_empty() {
        return ddl::ParserResult::Failure;
    }

    let mut input = input;
    let mut value = 0_u32;

    // ISO 32000-2:2017, 9.7.6.2 requires trying successively longer
    // character codes, beginning with one byte. CMap codes are at most
    // four bytes long.
    for byte_index in 0..4 {
        if input.is_empty() {
            return ddl::ParserResult::Ok(ddl::I::from(-1_i32), input);
        }

        // Character codes are big-endian, so append each byte to the
        // accumulated value before checking ranges of the new width.
        let byte = u8::from(input.head());
        value = (value << 8) | u32::from(byte);
        input = input.advance(1);

        let width = (byte_index + 1) as u64;
        // Width is part of a character code's identity: for example,
        // <01> and <0001> belong to different codespaces despite having
        // the same numeric value.
        let matched = cmap.ranges.iter().any(|range| {
            u64::from(range.start.width) == width
                && u32::from(range.start.value) <= value
                && value <= u32::from(range.end.value)
        });

        if matched {
            return ddl::ParserResult::Ok(ddl::I::from(value as i32), input);
        }
    }

    ddl::ParserResult::Ok(ddl::I::from(-1_i32), input)
}

/// Append a Unicode code point to the extraction output.
pub fn EmitChar(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    character: ddl::U<32>,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.emitted.push(u32::from(character));
    ddl::ParserResult::Ok(ddl::Unit, input)
}
