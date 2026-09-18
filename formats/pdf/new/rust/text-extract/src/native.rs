//! Native functions used by the generated text-extraction parser.

use crate::state::TextExtractState;
use crate::text_extract_parsers::Cmap;
use daedalus_pdf_cos::{Ref, TopDecl};
use daedalus_rts_rust as ddl;

/// Resolve an indirect object using the PDF COS parser state.
pub fn resolve_ref(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    reference: Ref,
) -> ddl::ParserResult<ddl::Maybe<TopDecl>> {
    let result = daedalus_pdf_cos::native::resolve_ref(&mut state.user_state.pdf, input, reference);

    if matches!(
        result,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception
    ) {
        state.error = state.user_state.pdf.error.clone();
    }

    result
}

/// Read a character code whose width is selected by the CMap codespace ranges.
pub fn get_char_code(
    _state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    cmap: Cmap,
) -> ddl::ParserResult<ddl::I<32>> {
    if input.is_empty() || cmap.ranges.is_empty() {
        return ddl::ParserResult::Failure;
    }

    let mut input = input;
    let mut value = 0_u32;
    let mut possible = vec![true; cmap.ranges.len()];

    for byte_index in 0..4 {
        if input.is_empty() {
            return ddl::ParserResult::Ok(ddl::I::from(-1_i32), input);
        }

        let byte = u8::from(input.head());
        value = (value << 8) | u32::from(byte);
        input = input.advance(1);

        for (range_index, range) in cmap.ranges.iter().enumerate() {
            if !possible[range_index] {
                continue;
            }

            let Some((&lower, &upper)) = range.start.get(byte_index).zip(range.end.get(byte_index))
            else {
                possible[range_index] = false;
                continue;
            };

            if byte < u8::from(lower) || byte > u8::from(upper) {
                possible[range_index] = false;
                continue;
            }

            if byte_index + 1 == range.start.len() {
                return ddl::ParserResult::Ok(ddl::I::from(value as i32), input);
            }
        }
    }

    ddl::ParserResult::Ok(ddl::I::from(-1_i32), input)
}

/// Append a Unicode code point to the extraction output.
pub fn emit_char(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    character: ddl::U<32>,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.emitted.push(u32::from(character));
    ddl::ParserResult::Ok(ddl::Unit, input)
}
