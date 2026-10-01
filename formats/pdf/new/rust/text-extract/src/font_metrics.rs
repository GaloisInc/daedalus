use crate::text_extract_parsers::Fonts;
use daedalus_pdf_cos::pdfcos_parsers::PdfValue::Number;
use daedalus_rts_rust as ddl;
use ddl::Type;

const SIMPLE_FONT_SCALE: f64 = 0.001;

#[derive(Clone, Copy)]
pub(crate) struct GlyphDimensions {
    pub(crate) advance_width: f64,
    pub(crate) vertical_bounds: Option<VerticalBounds>,
}

#[derive(Clone, Copy)]
pub(crate) struct VerticalBounds {
    pub(crate) bottom: f64,
    pub(crate) top: f64,
}

/// Estimate glyph dimensions in unscaled text space.
///
/// Simple-font advances come from `Widths`, falling back to `MissingWidth`.
/// CIDFont advances come from `W`, falling back to `DW`.
/// Vertical bounds prefer `Descent` and `Ascent`, then fall back to `FontBBox`.
pub(crate) fn estimate_glyph_dimensions(
    font: &Fonts::Font,
    character_code: u32,
) -> Option<GlyphDimensions> {
    match &font.cidFont {
        ddl::Maybe::Just(cid_font) => estimate_cid_glyph_dimensions(cid_font, character_code),
        ddl::Maybe::Nothing => estimate_simple_glyph_dimensions(font, character_code),
    }
}

fn estimate_simple_glyph_dimensions(
    font: &Fonts::Font,
    character_code: u32,
) -> Option<GlyphDimensions> {
    let character_code = u8::try_from(character_code).ok()?;
    let advance_width = simple_explicit_width(font, character_code)
        .or_else(|| maybe_number(&font.missingWidth))
        .filter(|width| *width >= 0.0)?
        * SIMPLE_FONT_SCALE;

    let vertical_bounds =
        vertical_bounds(&font.descent, &font.ascent, &font.fontBBox).map(|(bottom, top)| {
            VerticalBounds {
                bottom: bottom.min(top) * SIMPLE_FONT_SCALE,
                top: bottom.max(top) * SIMPLE_FONT_SCALE,
            }
        });

    Some(GlyphDimensions {
        advance_width,
        vertical_bounds,
    })
}

fn estimate_cid_glyph_dimensions(font: &Fonts::GetCIDFont, cid: u32) -> Option<GlyphDimensions> {
    let advance_width = cid_explicit_width(font, cid)
        .or_else(|| number_to_f64(&font.defaultWidth))
        .filter(|width| *width >= 0.0)?
        * SIMPLE_FONT_SCALE;

    let vertical_bounds =
        vertical_bounds(&font.descent, &font.ascent, &font.fontBBox).map(|(bottom, top)| {
            VerticalBounds {
                bottom: bottom.min(top) * SIMPLE_FONT_SCALE,
                top: bottom.max(top) * SIMPLE_FONT_SCALE,
            }
        });

    Some(GlyphDimensions {
        advance_width,
        vertical_bounds,
    })
}

fn cid_explicit_width(font: &Fonts::GetCIDFont, cid: u32) -> Option<f64> {
    let cid_key = ddl::U::from(cid);
    let ddl::Maybe::Just((first, entry)) = font.widths.bor().lookup_le(cid_key.bor()) else {
        return None;
    };
    let first = u32::from(first);

    match entry {
        Fonts::cidWidth::Consecutive((last, widths)) => {
            if cid > u32::from(last) {
                return None;
            }
            let index = cid.checked_sub(first)? as usize;
            number_to_f64(widths.get(index)?)
        }
        Fonts::cidWidth::Range((last, width)) => (cid <= u32::from(last))
            .then(|| number_to_f64(&width))
            .flatten(),
    }
}

fn simple_explicit_width(font: &Fonts::Font, character_code: u8) -> Option<f64> {
    let ddl::Maybe::Just(first_character) = &font.firstChar else {
        return None;
    };
    let ddl::Maybe::Just(widths) = &font.widths else {
        return None;
    };

    let index = character_code.checked_sub(u8::from(*first_character))? as usize;
    number_to_f64(widths.get(index)?)
}

fn vertical_bounds(
    descent: &ddl::Maybe<Number>,
    ascent: &ddl::Maybe<Number>,
    font_bbox: &ddl::Maybe<ddl::Array<Number>>,
) -> Option<(f64, f64)> {
    match (maybe_number(descent), maybe_number(ascent)) {
        (Some(descent), Some(ascent)) => Some((descent, ascent)),
        _ => {
            let ddl::Maybe::Just(bbox) = font_bbox else {
                return None;
            };
            Some((number_to_f64(bbox.get(1)?)?, number_to_f64(bbox.get(3)?)?))
        }
    }
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
