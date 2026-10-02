//! Native functions used by the generated text-extraction parser.

#![allow(non_snake_case)]

use crate::font_metrics::estimate_glyph_dimensions;
use crate::layout_state::{BoundingBox, ExtractionState, Matrix, Point};
use crate::text_extract_parsers::{
    CMap::{self, cmap},
    Fonts,
};
use crate::TextExtractState;
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
            state.user_state.cmap_cache.insert(reference, cmap.clone());
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
    state.user_state.extraction.graphics_stack.push(graphics);
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
    a: f64,
    b: f64,
    c: f64,
    d: f64,
    e: f64,
    f: f64,
) -> ddl::ParserResult<ddl::Unit> {
    let matrix = Matrix { a, b, c, d, e, f };
    let graphics = &mut state.user_state.extraction.graphics;
    graphics.ctm = graphics.ctm.map(|ctm| matrix.multiply(ctm));
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Begin a text object and initialize its text matrices.
pub fn BeginText(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    let extraction = &mut state.user_state.extraction;
    extraction.text_matrix = Some(Matrix::IDENTITY);
    extraction.text_line_matrix = Some(Matrix::IDENTITY);
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
    value: f64,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.graphics.character_spacing = value;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set the additional spacing applied to word spaces.
pub fn SetWordSpacing(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: f64,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.graphics.word_spacing = value;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set horizontal text scaling as a percentage.
pub fn SetHorizontalScaling(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: f64,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.graphics.horizontal_scaling = value;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Set the vertical distance used to move to the next text line.
pub fn SetLeading(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    value: f64,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.graphics.leading = value;
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
    value: f64,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.graphics.text_rise = value;
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Replace the text matrix and text-line matrix.
pub fn SetTextMatrix(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    a: f64,
    b: f64,
    c: f64,
    d: f64,
    e: f64,
    f: f64,
) -> ddl::ParserResult<ddl::Unit> {
    let matrix = Matrix { a, b, c, d, e, f };
    let extraction = &mut state.user_state.extraction;
    extraction.text_matrix = Some(matrix);
    extraction.text_line_matrix = Some(matrix);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Translate the text-line matrix and copy it to the text matrix.
pub fn MoveTextPosition(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    tx: f64,
    ty: f64,
) -> ddl::ParserResult<ddl::Unit> {
    move_text_position(&mut state.user_state.extraction, tx, ty);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Translate the text matrices and set text leading to the negative ty value.
pub fn MoveTextPositionAndSetLeading(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    tx: f64,
    ty: f64,
) -> ddl::ParserResult<ddl::Unit> {
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
    adjustment: f64,
) -> ddl::ParserResult<ddl::Unit> {
    let extraction = &mut state.user_state.extraction;
    let Some(font_size) = extraction.graphics.font_size else {
        extraction.text_matrix = None;
        extraction.malformed_operators += 1;
        return ddl::ParserResult::Ok(ddl::Unit, input);
    };
    let horizontal_scaling = extraction.graphics.horizontal_scaling / 100.0;
    let translation = Matrix {
        e: -adjustment / 1000.0 * font_size * horizontal_scaling,
        ..Matrix::IDENTITY
    };
    extraction.text_matrix = extraction
        .text_matrix
        .map(|text_matrix| translation.multiply(text_matrix));
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

    match Fonts::FontByRef(state, input.clone(), encodings, reference.clone()) {
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
    font_size: f64,
    font: ddl::Maybe<Fonts::Font>,
) -> ddl::ParserResult<ddl::Unit> {
    let graphics = &mut state.user_state.extraction.graphics;
    graphics.font = match font {
        ddl::Maybe::Just(font) => Some(font),
        ddl::Maybe::Nothing => None,
    };
    graphics.font_size = Some(font_size);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Begin collecting one text-showing string as a chunk.
pub fn BeginTextChunk(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.begin_chunk();
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Finish collecting the current text chunk.
pub fn FinishTextChunk(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
) -> ddl::ParserResult<ddl::Unit> {
    state.user_state.extraction.finish_chunk();
    ddl::ParserResult::Ok(ddl::Unit, input)
}

/// Append decoded UTF-16 code units to the extraction output.
pub fn EmitUtf16(
    state: &mut ddl::ParserStateWith<TextExtractState>,
    input: ddl::Input,
    code_width: ddl::U<8>,
    character_code: ddl::U<32>,
    text: ddl::Array<ddl::U<16>>,
) -> ddl::ParserResult<ddl::Unit> {
    let text: Vec<u16> = text.iter().map(|unit| u16::from(*unit)).collect();
    let extraction = &mut state.user_state.extraction;
    let bounds = position_glyph(extraction, u8::from(code_width), u32::from(character_code));
    extraction.append_to_chunk(&text, bounds);
    ddl::ParserResult::Ok(ddl::Unit, input)
}

fn move_text_position(extraction: &mut crate::layout_state::ExtractionState, tx: f64, ty: f64) {
    let translation = Matrix {
        e: tx,
        f: ty,
        ..Matrix::IDENTITY
    };
    extraction.text_line_matrix = extraction
        .text_line_matrix
        .map(|text_line_matrix| translation.multiply(text_line_matrix));
    extraction.text_matrix = extraction.text_line_matrix;
}

fn position_glyph(
    extraction: &mut ExtractionState,
    code_width: u8,
    character_code: u32,
) -> Option<BoundingBox> {
    let Some(font) = extraction.graphics.font.as_ref() else {
        extraction.text_matrix = None;
        return None;
    };
    let (metric_code, is_simple_font) = match &font.cidFont {
        // Identity-H maps each two-byte source code directly to a CID.
        ddl::Maybe::Just(cid_font)
            if code_width == 2 && matches!(&cid_font.encoding, ddl::Maybe::Just(_)) =>
        {
            (character_code, false)
        }
        // Other CID encodings and source-code widths are not yet supported.
        ddl::Maybe::Just(_) => {
            extraction.text_matrix = None;
            return None;
        }
        // Simple fonts use a single-byte character code for metric lookup.
        ddl::Maybe::Nothing if code_width == 1 => {
            let Some(character_code) = u8::try_from(character_code).ok() else {
                extraction.text_matrix = None;
                return None;
            };
            (character_code.into(), true)
        }
        // Multi-byte source codes require a supported CID font.
        ddl::Maybe::Nothing => {
            extraction.text_matrix = None;
            return None;
        }
    };
    let Some(font_size) = extraction.graphics.font_size else {
        extraction.text_matrix = None;
        return None;
    };
    let Some(dimensions) = estimate_glyph_dimensions(font, metric_code) else {
        extraction.text_matrix = None;
        return None;
    };
    let horizontal_scaling = extraction.graphics.horizontal_scaling / 100.0;
    let word_spacing = if is_simple_font && metric_code == u32::from(b' ') {
        extraction.graphics.word_spacing
    } else {
        0.0
    };
    let advance = (dimensions.advance_width * font_size
        + extraction.graphics.character_spacing
        + word_spacing)
        * horizontal_scaling;
    if !advance.is_finite() {
        extraction.text_matrix = None;
        return None;
    }

    let bounds = match (
        dimensions.vertical_bounds,
        extraction.text_matrix,
        extraction.graphics.ctm,
    ) {
        (Some(vertical), Some(text_matrix), Some(ctm)) => BoundingBox {
            min: Point {
                x: 0.0,
                y: vertical.bottom * font_size + extraction.graphics.text_rise,
            },
            max: Point {
                x: advance,
                y: vertical.top * font_size + extraction.graphics.text_rise,
            },
        }
        .transform(text_matrix.multiply(ctm)),
        _ => None,
    };

    let translation = Matrix {
        e: advance,
        ..Matrix::IDENTITY
    };
    extraction.text_matrix = extraction
        .text_matrix
        .map(|text_matrix| translation.multiply(text_matrix));
    bounds
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
