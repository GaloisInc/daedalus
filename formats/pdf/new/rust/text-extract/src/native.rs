//! Native functions used by the generated text-extraction parser.

#![allow(non_snake_case)]

use crate::TextExtractState;
use crate::text_extract_parsers::CMap::{self, cmap};
use daedalus_pdf_cos::{Ref, TopDecl};
use daedalus_rts_rust as ddl;
use ddl::Type;

/// Print a diagnostic message emitted by the Daedalus specification.
pub fn Trace(
    _state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    message: ddl::Array<ddl::U<8>>,
) -> ddl::ParserResult<ddl::Unit> {
    let bytes: Vec<u8> = message.iter().map(|byte| u8::from(*byte)).collect();
    eprintln!("{}", String::from_utf8_lossy(&bytes));
    ddl::ParserResult::Ok(ddl::Unit, input)
}

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

/// Load and cache a CMap stored in an indirect PDF stream.
pub fn LoadCMapByRef(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    reference: Ref,
) -> ddl::ParserResult<cmap> {
    if let Some(cmap) = state.user_state.cmap_cache.get(&reference) {
        return ddl::ParserResult::Ok(cmap.clone(), input);
    }

    if !state.user_state.loading_cmaps.insert(reference.clone()) {
        return native_failure(state, &input, "cyclic CMap UseCMap reference");
    }

    let result = CMap::CMap(state, input.clone(), reference.clone());
    state.user_state.loading_cmaps.remove(&reference);

    match result {
        ddl::ParserResult::Ok(cmap, _) => {
            state
                .user_state
                .cmap_cache
                .insert(reference, cmap.clone());
            ddl::ParserResult::Ok(cmap, input)
        }
        ddl::ParserResult::Failure => ddl::ParserResult::Failure,
        ddl::ParserResult::Exception => ddl::ParserResult::Exception,
    }
}

fn native_failure(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: &ddl::Input,
    message: &str,
) -> ddl::ParserResult<cmap> {
    state.note_fail(
        false,
        "LoadCMapByRef",
        input.bor(),
        ddl::new_byte_array(message.as_bytes()).bor(),
    );
    ddl::ParserResult::Failure
}
