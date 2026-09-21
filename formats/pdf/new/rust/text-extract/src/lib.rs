mod native;
mod text_extract_parsers;

use crate::text_extract_parsers::{Catalog, StandardEncodings, TextExtract};
use daedalus_pdf_cos::{PdfCos, PdfError};
use daedalus_rts_rust as ddl;
use std::fmt;

const GLYPH_MAP: &[u8] = include_bytes!("../../../pdf-text-extract-spec/glyphmap.txt");

pub(crate) struct TextExtractState {
    pub(crate) pdf: PdfCos,
    pub(crate) emitted: Vec<u32>,
}

/// An error encountered while extracting text from a PDF.
#[derive(Debug)]
pub enum ExtractError {
    Pdf(PdfError),
    MissingRoot,
    GlyphMap(String),
    Catalog(String),
    Text(String),
    InvalidCodePoint(u32),
}

impl fmt::Display for ExtractError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pdf(error) => write!(f, "failed to prepare PDF: {error}"),
            Self::MissingRoot => write!(f, "PDF trailer has no root reference"),
            Self::GlyphMap(error) => write!(f, "failed to parse glyph map: {error}"),
            Self::Catalog(error) => write!(f, "failed to parse PDF catalog: {error}"),
            Self::Text(error) => write!(f, "failed to extract text: {error}"),
            Self::InvalidCodePoint(code) => {
                write!(f, "extracted value U+{code:04X} is not a Unicode code point")
            }
        }
    }
}

impl From<PdfError> for ExtractError {
    fn from(error: PdfError) -> Self {
        Self::Pdf(error)
    }
}

/// Extract UTF-8 text from the complete contents of a PDF.
pub fn extract_text_bytes(name: &str, bytes: &[u8]) -> Result<String, ExtractError> {
    let pdf = daedalus_pdf_cos::prepare_pdf_bytes(name, bytes)?;
    let root = pdf
        .user_state
        .root
        .clone()
        .ok_or(ExtractError::MissingRoot)?;
    let mut state = ddl::new_parser_state_with(TextExtractState {
        pdf,
        emitted: Vec::new(),
    });

    let glyph_input = ddl::new_input(
        ddl::new_byte_array(b"glyphmap.txt"),
        ddl::new_byte_array(GLYPH_MAP),
    );
    let encodings = match StandardEncodings::StdEncodings(&mut state, glyph_input) {
        ddl::ParserResult::Ok(encodings, _) => encodings,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
            return Err(ExtractError::GlyphMap(state.error.to_string()));
        }
    };

    let empty_input = ddl::new_input(ddl::new_byte_array(b""), ddl::new_byte_array(b""));
    let catalog = match Catalog::PdfCatalog(
        &mut state,
        empty_input.clone(),
        true,
        ddl::Maybe::Just(encodings),
        root,
    ) {
        ddl::ParserResult::Ok(catalog, _) => catalog,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
            return Err(ExtractError::Catalog(state.error.to_string()));
        }
    };

    let code_points = match TextExtract::TextInCatalog(&mut state, empty_input, catalog) {
        ddl::ParserResult::Ok(_, _) => state.user_state.emitted,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
            return Err(ExtractError::Text(state.error.to_string()));
        }
    };

    code_points
        .into_iter()
        .map(|code| char::from_u32(code).ok_or(ExtractError::InvalidCodePoint(code)))
        .collect()
}
