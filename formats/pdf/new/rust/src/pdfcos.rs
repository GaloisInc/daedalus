//! PDF COS preparation, reference table, and lazy object cache.

use crate::pdfcos_parsers::{
    CrossRef, CrossRefAndTrailer, CrossRefEntry, Ref, TopDecl, TrailerDict, XRefObjEntry,
    XRefObjTable,
};
use daedalus_rts_rust as ddl;
use ddl::Type;
use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt;

/// The current state of an object in the reference table.
#[derive(Clone, PartialEq, Eq)]
pub enum ReferenceState {
    /// The object is currently being parsed.
    Loading,

    /// An indirect object at a byte offset relative to the `%PDF-` header.
    AtOffset(usize),

    /// An object stored at an index within an object stream.
    InObjectStream { container: u64, index: u64 },

    /// An unsupported or unknown cross-reference stream entry type.
    Null,

    /// An object that has already been parsed.
    Parsed(TopDecl),
}

/// A processed cross-reference entry.
#[derive(Clone, PartialEq, Eq)]
pub struct ReferenceEntry {
    pub generation: u64,
    pub state: ReferenceState,
}

/// Reference table and lazy object cache used while processing a PDF.
pub struct ReferenceTable {
    /// The PDF input, normalized to begin at its `%PDF-` header and end at
    pub input: ddl::Input,

    /// The entries collected from the PDF's cross-reference sections.
    pub entries: BTreeMap<u64, ReferenceEntry>,

    /// The root object reference from the newest trailer, if present.
    pub root: Option<Ref>,

    /// The object currently being parsed, for object-specific operations such
    /// as decryption.
    pub current_object: Option<(u64, u64)>,
}

/// An error encountered while locating or processing the PDF cross-reference
/// information.
#[derive(Debug)]
pub enum PreparePdfError {
    PdfStartNotFound,
    StartXrefNotFound,
    StartXrefParse(String),
    XrefOffsetOutOfRange(usize),
    XrefCycle(usize),
    XrefParse { offset: usize, error: String },
    InvalidXrefInteger(&'static str),
    InvalidXrefOffset(&'static str),
}

impl fmt::Display for PreparePdfError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PdfStartNotFound => write!(f, "PDF header not found"),
            Self::StartXrefNotFound => write!(f, "final startxref marker not found"),
            Self::StartXrefParse(error) => write!(f, "failed to parse startxref: {error}"),
            Self::XrefOffsetOutOfRange(offset) => {
                write!(f, "cross-reference offset {offset} is outside the PDF")
            }
            Self::XrefCycle(offset) => {
                write!(f, "cross-reference sections form a cycle at offset {offset}")
            }
            Self::XrefParse { offset, error } => {
                write!(
                    f,
                    "failed to parse cross-reference section at offset {offset}: {error}"
                )
            }
            Self::InvalidXrefInteger(field) => {
                write!(f, "cross-reference field {field} does not fit in a u64")
            }
            Self::InvalidXrefOffset(field) => {
                write!(f, "cross-reference offset {field} does not fit in a usize")
            }
        }
    }
}

impl Error for PreparePdfError {}

/// Locate the PDF header and process its cross-reference sections.
///
/// The returned reference table retains an input normalized to the `%PDF-`
/// header, so all stored byte offsets use the PDF coordinate system.
pub fn prepare_pdf(input: ddl::Input) -> Result<ReferenceTable, PreparePdfError> {
    let input_bytes = input.bytes();
    let pdf_start =
        find_bytes(&input_bytes, b"%PDF-").ok_or(PreparePdfError::PdfStartNotFound)?;
    let input = input.advance(pdf_start);

    let pdf_bytes = input.bytes();
    let pdf_end =
        rfind_bytes(&pdf_bytes, b"startxref").ok_or(PreparePdfError::StartXrefNotFound)?;

    let mut state = ddl::new_parser_state_with(ReferenceTable {
        input: input.clone(),
        entries: BTreeMap::new(),
        root: None,
        current_object: None,
    });
    let startxref =
        match crate::pdfcos_parsers::pdf_end(&mut state, input.clone().advance(pdf_end)) {
        ddl::ParserResult::Ok(offset, _) => usize::try_from(u64::from(offset))
            .map_err(|_| PreparePdfError::InvalidXrefOffset("startxref"))?,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
            return Err(PreparePdfError::StartXrefParse(state.error.to_string()));
        }
    };

    let mut visited = BTreeSet::new();
    process_xref(&mut state, &input, &mut visited, startxref, true)?;
    Ok(state.user_state)
}

fn process_xref(
    state: &mut ddl::ParserStateWith<ReferenceTable>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    offset: usize,
    top: bool,
) -> Result<(), PreparePdfError> {
    if !visited.insert(offset) {
        return Err(PreparePdfError::XrefCycle(offset));
    }

    let xref_input = match input.clone().advance_maybe(offset) {
        ddl::Maybe::Just(input) => input,
        ddl::Maybe::Nothing => return Err(PreparePdfError::XrefOffsetOutOfRange(offset)),
    };

    let xref = match crate::pdfcos_parsers::cross_ref(state, xref_input) {
        ddl::ParserResult::Ok(xref, _) => xref,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
            return Err(PreparePdfError::XrefParse {
                offset,
                error: state.error.to_string(),
            });
        }
    };

    match xref {
        CrossRef::OldXref(xref) => process_old_xref(state, input, visited, xref, top),
        CrossRef::NewXref(xref) => process_new_xref(state, input, visited, xref, top),
    }
}

fn process_old_xref(
    state: &mut ddl::ParserStateWith<ReferenceTable>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    xref: CrossRefAndTrailer,
    top: bool,
) -> Result<(), PreparePdfError> {
    process_trailer_links(state, input, visited, &xref.trailer)?;

    for subsection in xref.xref.iter() {
        let mut object = int_to_u64(&subsection.first_id, "firstId")?;
        for entry in subsection.entries.iter() {
            match entry {
                CrossRefEntry::InUse(entry) => {
                    state.user_state.entries.insert(
                        object,
                        ReferenceEntry {
                            generation: int_to_u64(&entry.r#gen, "generation")?,
                            state: ReferenceState::AtOffset(int_to_usize(
                                &entry.offset,
                                "object",
                            )?),
                        },
                    );
                }
                CrossRefEntry::Free(_) => {
                    state.user_state.entries.remove(&object);
                }
            }
            object = object
                .checked_add(1)
                .ok_or(PreparePdfError::InvalidXrefInteger("object number"))?;
        }
    }

    if top {
        record_top_trailer(&mut state.user_state, &xref.trailer);
    }
    Ok(())
}

fn process_new_xref(
    state: &mut ddl::ParserStateWith<ReferenceTable>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    xref: XRefObjTable,
    top: bool,
) -> Result<(), PreparePdfError> {
    process_trailer_links(state, input, visited, &xref.trailer)?;

    for subsection in xref.xref.iter() {
        let mut object = int_to_u64(&subsection.first_id, "firstId")?;
        for entry in subsection.entries.iter() {
            match entry {
                XRefObjEntry::InUse(entry) => {
                    state.user_state.entries.insert(
                        object,
                        ReferenceEntry {
                            generation: int_to_u64(&entry.r#gen, "generation")?,
                            state: ReferenceState::AtOffset(int_to_usize(
                                &entry.offset,
                                "object",
                            )?),
                        },
                    );
                }
                XRefObjEntry::Compressed(entry) => {
                    state.user_state.entries.insert(
                        object,
                        ReferenceEntry {
                            generation: 0,
                            state: ReferenceState::InObjectStream {
                                container: int_to_u64(
                                    &entry.container_obj,
                                    "container object",
                                )?,
                                index: int_to_u64(&entry.obj_index, "object stream index")?,
                            },
                        },
                    );
                }
                XRefObjEntry::Free(_) => {
                    state.user_state.entries.remove(&object);
                }
                XRefObjEntry::Null => {
                    state.user_state.entries.insert(
                        object,
                        ReferenceEntry {
                            generation: 0,
                            state: ReferenceState::Null,
                        },
                    );
                }
            }
            object = object
                .checked_add(1)
                .ok_or(PreparePdfError::InvalidXrefInteger("object number"))?;
        }
    }

    if top {
        record_top_trailer(&mut state.user_state, &xref.trailer);
    }
    Ok(())
}

fn process_trailer_links(
    state: &mut ddl::ParserStateWith<ReferenceTable>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    trailer: &TrailerDict,
) -> Result<(), PreparePdfError> {
    if let ddl::Maybe::Just(offset) = &trailer.prev {
        process_xref(state, input, visited, int_to_usize(offset, "Prev")?, false)?;
    }

    // XRefStm entries override Prev entries, so process this link second.
    if let ddl::Maybe::Just(offset) = &trailer.xrefstm {
        process_xref(
            state,
            input,
            visited,
            int_to_usize(offset, "XRefStm")?,
            false,
        )?;
    }

    Ok(())
}

fn record_top_trailer(table: &mut ReferenceTable, trailer: &TrailerDict) {
    if let ddl::Maybe::Just(root) = &trailer.root {
        table.root = Some(root.clone());
    }
}

fn int_to_u64(value: &ddl::Int, field: &'static str) -> Result<u64, PreparePdfError> {
    value
        .try_to_unsigned()
        .ok_or(PreparePdfError::InvalidXrefInteger(field))
}

fn int_to_usize(value: &ddl::Int, field: &'static str) -> Result<usize, PreparePdfError> {
    let value = value
        .try_to_unsigned()
        .ok_or(PreparePdfError::InvalidXrefOffset(field))?;
    usize::try_from(value).map_err(|_| PreparePdfError::InvalidXrefOffset(field))
}

pub(crate) fn resolve_reference(
    parser_state: &mut ddl::ParserStateWith<ReferenceTable>,
    input: ddl::Input,
    reference: Ref,
) -> ddl::ParserResult<ddl::Maybe<TopDecl>> {
    let object = match reference.obj.try_to_unsigned() {
        Some(object) => object,
        None => {
            return resolve_failure(
                parser_state,
                &input,
                "PDF reference has an invalid object number",
            );
        }
    };
    let requested_generation = match reference.r#gen.try_to_unsigned() {
        Some(generation) => generation,
        None => {
            return resolve_failure(
                parser_state,
                &input,
                "PDF reference has an invalid generation number",
            );
        }
    };

    let entry = match parser_state.user_state.entries.remove(&object) {
        Some(entry) => entry,
        None => return ddl::ParserResult::Ok(ddl::Maybe::Nothing, input),
    };

    let ReferenceEntry { generation, state } = entry;
    if generation != requested_generation {
        parser_state.user_state.entries.insert(
            object,
            ReferenceEntry { generation, state },
        );
        return ddl::ParserResult::Ok(ddl::Maybe::Nothing, input);
    }

    match state {
        ReferenceState::Parsed(value) => {
            let result = value.clone();
            parser_state.user_state.entries.insert(
                object,
                ReferenceEntry {
                    generation,
                    state: ReferenceState::Parsed(value),
                },
            );
            ddl::ParserResult::Ok(ddl::Maybe::Just(result), input)
        }

        ReferenceState::Null => {
            parser_state.user_state.entries.insert(
                object,
                ReferenceEntry {
                    generation,
                    state: ReferenceState::Null,
                },
            );
            ddl::ParserResult::Ok(ddl::Maybe::Nothing, input)
        }

        ReferenceState::Loading => {
            parser_state.user_state.entries.insert(
                object,
                ReferenceEntry {
                    generation,
                    state: ReferenceState::Loading,
                },
            );
            resolve_failure(
                parser_state,
                &input,
                &format!(
                    "recursive resolution of PDF object {object} {generation}"
                ),
            )
        }

        ReferenceState::InObjectStream { container, index } => {
            parser_state.user_state.entries.insert(
                object,
                ReferenceEntry {
                    generation,
                    state: ReferenceState::InObjectStream { container, index },
                },
            );
            resolve_failure(
                parser_state,
                &input,
                &format!(
                    "resolving PDF object {object} {generation} from an object stream is not implemented"
                ),
            )
        }

        ReferenceState::AtOffset(offset) => {
            parser_state.user_state.entries.insert(
                object,
                ReferenceEntry {
                    generation,
                    state: ReferenceState::Loading,
                },
            );

            let object_input =
                match parser_state.user_state.input.clone().advance_maybe(offset) {
                    ddl::Maybe::Just(input) => input,
                    ddl::Maybe::Nothing => {
                        return resolve_failure(
                            parser_state,
                            &input,
                            &format!(
                                "PDF object {object} {generation} has invalid offset {offset}"
                            ),
                        );
                    }
                };

            let error_input = object_input.clone();
            let previous_object = parser_state
                .user_state
                .current_object
                .replace((object, generation));
            let parsed = crate::pdfcos_parsers::top_decl(parser_state, object_input);
            parser_state.user_state.current_object = previous_object;

            match parsed {
                ddl::ParserResult::Ok(value, _) => {
                    parser_state.user_state.entries.insert(
                        object,
                        ReferenceEntry {
                            generation,
                            state: ReferenceState::Parsed(value.clone()),
                        },
                    );
                    ddl::ParserResult::Ok(ddl::Maybe::Just(value), input)
                }
                ddl::ParserResult::Failure => resolve_failure(
                    parser_state,
                    &error_input,
                    &format!("failed to parse PDF object {object} {generation}"),
                ),
                ddl::ParserResult::Exception => ddl::ParserResult::Exception,
            }
        }
    }
}

fn resolve_failure<T>(
    parser_state: &mut ddl::ParserStateWith<ReferenceTable>,
    input: &ddl::Input,
    message: &str,
) -> ddl::ParserResult<T> {
    let message = ddl::new_byte_array(message.as_bytes());
    parser_state.note_fail(
        true,
        "pdf-cos-spec/PdfDecl.ddl:37:ResolveRef",
        input,
        message.bor(),
    );
    ddl::ParserResult::Failure
}

fn find_bytes(bytes: &[ddl::U<8>], needle: &[u8]) -> Option<usize> {
    bytes
        .windows(needle.len())
        .position(|window| window.iter().copied().map(u8::from).eq(needle.iter().copied()))
}

fn rfind_bytes(bytes: &[ddl::U<8>], needle: &[u8]) -> Option<usize> {
    bytes
        .windows(needle.len())
        .rposition(|window| window.iter().copied().map(u8::from).eq(needle.iter().copied()))
}
