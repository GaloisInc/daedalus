//! Native functions used by the generated text-extraction parser.

#![allow(non_snake_case)]

use crate::TextExtractState;
use crate::layout_state::Matrix;
use crate::text_extract_parsers::{
    CMap::{self, cmap},
    Fonts,
};
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

/// Reset page-local extraction state while preserving caches and output.
pub fn ResetPage(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.reset_for_page();
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Begin a text object and initialize its text matrices.
pub fn BeginText(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    let extraction = &mut state.user_state.extraction;
    extraction.in_text = true;
    extraction.text_matrix = Matrix::IDENTITY;
    extraction.text_line_matrix = Matrix::IDENTITY;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// End the current text object.
pub fn EndText(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.in_text = false;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Return the currently selected font.
pub fn CurrentFont(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Maybe<Fonts::Font>> {
    let font = match &state.user_state.extraction.graphics.font {
        Some(font) => ddl::Maybe::Just(font.clone()),
        None => ddl::Maybe::Nothing,
    };
    ddl::ParserResult::Ok(font, input)
}

/// Load a referenced font, using the application cache when possible.
pub fn LoadFontByRef(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    encodings: crate::text_extract_parsers::StandardEncodings::StdEncodings,
    reference: Ref,
) -> ddl::ParserResult<Fonts::Font> {
    if let Some(font) = state.user_state.extraction.font_cache.get(&reference) {
        return ddl::ParserResult::Ok(font.clone(), input);
    }

    match Fonts::FontByRef(
        state,
        input.clone(),
        encodings,
        reference.clone(),
    ) {
        ddl::ParserResult::Ok(font, _) => {
            state
                .user_state
                .extraction
                .font_cache
                .insert(reference, font.clone());
            ddl::ParserResult::Ok(font, input)
        }
        ddl::ParserResult::Failure => ddl::ParserResult::Failure,
        ddl::ParserResult::Exception => ddl::ParserResult::Exception,
    }
}

/// Select the font used to decode subsequent text.
pub fn SetFont(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    font: ddl::Maybe<Fonts::Font>,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.graphics.font = match font {
        ddl::Maybe::Just(font) => Some(font),
        ddl::Maybe::Nothing => None,
    };
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Append decoded UTF-16 code units to the extraction output.
pub fn EmitUtf16(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    text: ddl::Array<ddl::U<16>>,
) -> ddl::ParserResult<ddl::Unit> {
    state
        .user_state
        .extraction
        .output
        .extend(text.iter().map(|unit| u16::from(*unit)));
    ddl::ParserResult::Ok(ddl::Unit, input)
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
