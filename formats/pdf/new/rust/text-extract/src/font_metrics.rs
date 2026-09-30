use crate::text_extract_parsers::Fonts;
use daedalus_pdf_cos::pdfcos_parsers::PdfValue::Number;
use daedalus_rts_rust as ddl;

const SIMPLE_FONT_SCALE: f64 = 0.001;

pub(crate) struct GlyphDimensions {
    pub(crate) advance_width: f64,
    pub(crate) vertical_bounds: Option<VerticalBounds>,
}

pub(crate) struct VerticalBounds {
    pub(crate) bottom: f64,
    pub(crate) top: f64,
}

/// Estimate simple-font glyph dimensions in unscaled text space.
///
/// The advance comes from `Widths`, falling back to `MissingWidth`. Vertical
/// bounds prefer `Descent` and `Ascent`, then fall back to the y coordinates
/// in `FontBBox`.
pub(crate) fn estimate_glyph_dimensions(
    font: &Fonts::Font,
    character_code: u8,
) -> Option<GlyphDimensions> {
    let advance_width = explicit_width(font, character_code)
        .or_else(|| maybe_number(&font.missingWidth))
        .filter(|width| *width >= 0.0)?
        * SIMPLE_FONT_SCALE;

    let vertical_bounds = descriptor_vertical_bounds(font)
        .or_else(|| font_bbox_vertical_bounds(font))
        .map(|(bottom, top)| VerticalBounds {
            bottom: bottom.min(top) * SIMPLE_FONT_SCALE,
            top: bottom.max(top) * SIMPLE_FONT_SCALE,
        });

    Some(GlyphDimensions {
        advance_width,
        vertical_bounds,
    })
}

fn explicit_width(font: &Fonts::Font, character_code: u8) -> Option<f64> {
    let ddl::Maybe::Just(first_character) = &font.firstChar else {
        return None;
    };
    let ddl::Maybe::Just(widths) = &font.widths else {
        return None;
    };

    let index = character_code.checked_sub(u8::from(*first_character))? as usize;
    number_to_f64(widths.get(index)?)
}

fn descriptor_vertical_bounds(font: &Fonts::Font) -> Option<(f64, f64)> {
    Some((
        maybe_number(&font.descent)?,
        maybe_number(&font.ascent)?,
    ))
}

fn font_bbox_vertical_bounds(font: &Fonts::Font) -> Option<(f64, f64)> {
    let ddl::Maybe::Just(bbox) = &font.fontBBox else {
        return None;
    };
    Some((number_to_f64(bbox.get(1)?)?, number_to_f64(bbox.get(3)?)?))
}

fn maybe_number(number: &ddl::Maybe<Number>) -> Option<f64> {
    match number {
        ddl::Maybe::Just(number) => number_to_f64(number),
        ddl::Maybe::Nothing => None,
    }
}

fn number_to_f64(number: &Number) -> Option<f64> {
    let value = number.num.to_f64() * 10.0_f64.powf(number.exp.to_f64());
    value.is_finite().then_some(value)
}
