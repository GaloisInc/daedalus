//! Native functions used by the generated text-extraction parser.

#![allow(non_snake_case)]

use crate::TextExtractState;
use crate::layout_state::Matrix;
use crate::text_extract_parsers::{
    CMap::{self, cmap},
    Fonts,
};
use daedalus_pdf_cos::pdfcos_parsers::PdfValue::Number;
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

/// Save the current graphics state.
pub fn SaveGraphicsState(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    let graphics = state.user_state.extraction.graphics.clone();
    state
        .user_state
        .extraction
        .graphics_stack
        .push(graphics);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Restore the most recently saved graphics state.
pub fn RestoreGraphicsState(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    let extraction = &mut state.user_state.extraction;
    match extraction.graphics_stack.pop() {
        Some(graphics) => extraction.graphics = graphics,
        None => extraction.malformed_operators += 1,
    }
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Concatenate a matrix with the current transformation matrix.
pub fn ConcatMatrix(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    a: Number,
    b: Number,
    c: Number,
    d: Number,
    e: Number,
    f: Number,
) -> ddl::ParserResult<ddl::Unit> {
    let Some(matrix) = matrix_from_numbers(&a, &b, &c, &d, &e, &f) else {
        note_malformed(state);
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };
    let graphics = &mut state.user_state.extraction.graphics;
    graphics.ctm = matrix.multiply(graphics.ctm);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Begin a text object and initialize its text matrices.
pub fn BeginText(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    let extraction = &mut state.user_state.extraction;
    extraction.text_matrix = Matrix::IDENTITY;
    extraction.text_line_matrix = Matrix::IDENTITY;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Record a malformed operator without aborting text extraction.
pub fn NoteMalformedOperator(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    note_malformed(state);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set the spacing added after each character.
pub fn SetCharacterSpacing(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: Number,
) -> ddl::ParserResult<ddl::Unit> {
    set_graphics_number(state, &value, |graphics, value| {
        graphics.character_spacing = value;
    });
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set the additional spacing applied to word spaces.
pub fn SetWordSpacing(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: Number,
) -> ddl::ParserResult<ddl::Unit> {
    set_graphics_number(state, &value, |graphics, value| {
        graphics.word_spacing = value;
    });
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set horizontal text scaling as a percentage.
pub fn SetHorizontalScaling(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: Number,
) -> ddl::ParserResult<ddl::Unit> {
    set_graphics_number(state, &value, |graphics, value| {
        graphics.horizontal_scaling = value;
    });
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set the vertical distance used to move to the next text line.
pub fn SetLeading(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: Number,
) -> ddl::ParserResult<ddl::Unit> {
    set_graphics_number(state, &value, |graphics, value| {
        graphics.leading = value;
    });
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set the text rendering mode.
pub fn SetRenderingMode(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: ddl::U<8>,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.graphics.rendering_mode = u8::from(value);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set the vertical displacement of text from the baseline.
pub fn SetTextRise(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: Number,
) -> ddl::ParserResult<ddl::Unit> {
    set_graphics_number(state, &value, |graphics, value| {
        graphics.text_rise = value;
    });
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Replace the text matrix and text-line matrix.
pub fn SetTextMatrix(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    a: Number,
    b: Number,
    c: Number,
    d: Number,
    e: Number,
    f: Number,
) -> ddl::ParserResult<ddl::Unit> {
    let Some(matrix) = matrix_from_numbers(&a, &b, &c, &d, &e, &f) else {
        note_malformed(state);
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };
    let extraction = &mut state.user_state.extraction;
    extraction.text_matrix = matrix;
    extraction.text_line_matrix = matrix;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Translate the text-line matrix and copy it to the text matrix.
pub fn MoveTextPosition(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    tx: Number,
    ty: Number,
) -> ddl::ParserResult<ddl::Unit> {
    let (Some(tx), Some(ty)) = (number_to_f64(&tx), number_to_f64(&ty)) else {
        note_malformed(state);
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };
    move_text_position(&mut state.user_state.extraction, tx, ty);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Translate the text matrices and set text leading to the negative ty value.
pub fn MoveTextPositionAndSetLeading(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    tx: Number,
    ty: Number,
) -> ddl::ParserResult<ddl::Unit> {
    let (Some(tx), Some(ty)) = (number_to_f64(&tx), number_to_f64(&ty)) else {
        note_malformed(state);
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };
    let extraction = &mut state.user_state.extraction;
    extraction.graphics.leading = -ty;
    move_text_position(extraction, tx, ty);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Move to the start of the next text line using the current leading.
pub fn MoveToNextTextLine(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    let extraction = &mut state.user_state.extraction;
    let leading = extraction.graphics.leading;
    move_text_position(extraction, 0.0, -leading);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Apply a numeric positioning adjustment from a TJ array.
pub fn AdjustTextPosition(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: Number,
) -> ddl::ParserResult<ddl::Unit> {
    let Some(adjustment) = number_to_f64(&value) else {
        note_malformed(state);
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };

    let extraction = &mut state.user_state.extraction;
    let Some(font_size) = extraction.graphics.font_size else {
        extraction.malformed_operators += 1;
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };
    let horizontal_scaling = extraction.graphics.horizontal_scaling / 100.0;
    let translation = Matrix {
        e: -adjustment / 1000.0 * font_size * horizontal_scaling,
        ..Matrix::IDENTITY
    };
    extraction.text_matrix = translation.multiply(extraction.text_matrix);
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
    font_size: Number,
    font: ddl::Maybe<Fonts::Font>,
) -> ddl::ParserResult<ddl::Unit> {
    let Some(font_size) = number_to_f64(&font_size) else {
        note_malformed(state);
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };
    let graphics = &mut state.user_state.extraction.graphics;
    graphics.font = match font {
        ddl::Maybe::Just(font) => Some(font),
        ddl::Maybe::Nothing => None,
    };
    graphics.font_size = Some(font_size);
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

fn number_to_f64(number: &Number) -> Option<f64> {
    let value = number.num.to_f64() * 10.0_f64.powf(number.exp.to_f64());
    value.is_finite().then_some(value)
}

fn matrix_from_numbers(
    a: &Number,
    b: &Number,
    c: &Number,
    d: &Number,
    e: &Number,
    f: &Number,
) -> Option<Matrix> {
    Some(Matrix {
        a: number_to_f64(a)?,
        b: number_to_f64(b)?,
        c: number_to_f64(c)?,
        d: number_to_f64(d)?,
        e: number_to_f64(e)?,
        f: number_to_f64(f)?,
    })
}

fn move_text_position(
    extraction: &mut crate::layout_state::ExtractionState,
    tx: f64,
    ty: f64,
) {
    let translation = Matrix {
        e: tx,
        f: ty,
        ..Matrix::IDENTITY
    };
    extraction.text_line_matrix = translation.multiply(extraction.text_line_matrix);
    extraction.text_matrix = extraction.text_line_matrix;
}

fn set_graphics_number(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    number: &Number,
    update: impl FnOnce(&mut crate::layout_state::GraphicsState, f64),
) {
    match number_to_f64(number) {
        Some(value) => update(&mut state.user_state.extraction.graphics, value),
        None => note_malformed(state),
    }
}

fn note_malformed(state: &mut ddl::ParserStateWith<TextExtractState>) {
    state.user_state.extraction.malformed_operators += 1;
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
