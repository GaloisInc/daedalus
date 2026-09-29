mod native;
mod text_extract_parsers;

use crate::text_extract_parsers::{CMap, Catalog, StandardEncodings, TextExtract};
use daedalus_pdf_cos::{PdfCos, PdfError, Ref};
use daedalus_rts_rust as ddl;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::time::Instant;

const GLYPH_MAP: &[u8] = include_bytes!("../../../pdf-text-extract-spec/glyphmap.txt");

pub(crate) struct TextExtractState {
    pub(crate) pdf: PdfCos,
    pub(crate) cmap_cache: BTreeMap<Ref, CMap::cmap>,
    pub(crate) loading_cmaps: BTreeSet<Ref>,
}

/// An error encountered while extracting text from a PDF.
#[derive(Debug)]
pub enum ExtractError {
    Pdf(PdfError),
    MissingRoot,
    GlyphMap(String),
    Catalog(String),
    Text(String),
    InvalidPageNumber(u64),
    InvalidUtf16,
}

impl fmt::Display for ExtractError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pdf(error) => write!(f, "failed to prepare PDF: {error}"),
            Self::MissingRoot => write!(f, "PDF trailer has no root reference"),
            Self::GlyphMap(error) => write!(f, "failed to parse glyph map: {error}"),
            Self::Catalog(error) => write!(f, "failed to parse PDF catalog: {error}"),
            Self::Text(error) => write!(f, "failed to extract text: {error}"),
            Self::InvalidPageNumber(page) => {
                write!(f, "invalid page number {page}; page numbers start at 1")
            }
            Self::InvalidUtf16 => write!(f, "extracted text contains malformed UTF-16"),
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
    extract_text_bytes_from_page(name, bytes, None)
}

/// Extract UTF-8 text from one page of a PDF, using a one-based page number.
pub fn extract_page_text_bytes(
    name: &str,
    bytes: &[u8],
    page: u64,
) -> Result<String, ExtractError> {
    let page_index = page
        .checked_sub(1)
        .ok_or(ExtractError::InvalidPageNumber(page))?;
    extract_text_bytes_from_page(name, bytes, Some(page_index))
}

fn extract_text_bytes_from_page(
    name: &str,
    bytes: &[u8],
    page_index: Option<u64>,
) -> Result<String, ExtractError> {
    let pdf = daedalus_pdf_cos::prepare_pdf_bytes(name, bytes)?;
    let root = pdf
        .user_state
        .root
        .clone()
        .ok_or(ExtractError::MissingRoot)?;
    let mut state = ddl::new_parser_state_with(TextExtractState {
        pdf,
        cmap_cache: BTreeMap::new(),
        loading_cmaps: BTreeSet::new(),
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
    state.reset_parse_error();
    let page_count =
        match Catalog::PdfPageCount(&mut state, empty_input.clone(), root.clone()) {
            ddl::ParserResult::Ok(page_count, _) => u64::from(page_count),
            ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
                return Err(ExtractError::Catalog(state.error.to_string()));
            }
        };

    let first_page = page_index.unwrap_or(0);
    let last_page = page_index.map_or(page_count, |page| page + 1);
    let mut extract_state = TextExtract::ExtractState {
        font: ddl::Maybe::Nothing,
        fontCache: ddl::empty_map(),
        output: ddl::new_builder(),
    };

    for page_index in first_page..last_page {
        let page_number = page_index + 1;
        eprintln!("page {page_number} of {page_count}: parsing content...");
        let page_start = Instant::now();
        state.reset_parse_error();
        let catalog = match Catalog::PdfCatalog(
            &mut state,
            empty_input.clone(),
            true,
            encodings.clone(),
            ddl::Maybe::Just(ddl::U::from(page_index)),
            root.clone(),
        ) {
            ddl::ParserResult::Ok(catalog, _) => catalog,
            ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
                return Err(ExtractError::Catalog(state.error.to_string()));
            }
        };

        eprintln!("page {page_number} of {page_count}: extracting text...");
        state.reset_parse_error();
        extract_state = match TextExtract::TextInCatalogPage(
            &mut state,
            empty_input.clone(),
            extract_state,
            catalog,
        ) {
            ddl::ParserResult::Ok(next_state, _) => next_state,
            ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
                return Err(ExtractError::Text(state.error.to_string()));
            }
        };
        eprintln!(
            "page {page_number} of {page_count}: complete in {:.3?}",
            page_start.elapsed()
        );
    }

    char::decode_utf16(
        extract_state
            .output
            .build()
            .iter()
            .map(|unit| u16::from(*unit)),
    )
    .map(|character| character.map_err(|_| ExtractError::InvalidUtf16))
    .collect()
}
