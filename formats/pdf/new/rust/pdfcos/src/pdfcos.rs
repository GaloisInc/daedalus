//! PDF COS preparation, reference table, and lazy object cache.

use crate::pdfcos_parsers::PdfDecl::ObjStream;
use crate::pdfcos_parsers::PdfXRef::{
    CrossRef, CrossRefAndTrailer, CrossRefEntry, TrailerDict, XRefObjEntry, XRefObjTable,
};
pub use crate::pdfcos_parsers::PdfDecl::TopDecl;
pub use crate::pdfcos_parsers::PdfValue::Ref;
pub use crate::resolve::resolve_reference;
use daedalus_rts_rust as ddl;
use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt;

/// The current state of an object in the reference table.
#[derive(Clone, PartialEq, Eq)]
pub(crate) enum ReferenceState {
    /// The object is currently being parsed.
    Loading,

    /// Parsing the object failed on an earlier resolution attempt.
    Failed {
        error: String,
        exception: bool,
    },

    /// An indirect object at a byte offset relative to the `%PDF-` header.
    AtOffset(usize),

    /// An object stored at an index within an object stream.
    InObjectStream { container: u64, index: u64 },

    /// An unsupported or unknown cross-reference stream entry type.
    Null,

    /// An object that has already been parsed.
    Parsed(TopDecl),
}

/// A PDF object and its lazy parsing state.
#[derive(Clone, PartialEq, Eq)]
pub(crate) struct PdfObject {
    pub(crate) generation: u64,
    pub(crate) state: ReferenceState,
}

/// The cached parsing state of an object-stream container.
#[derive(Clone, PartialEq, Eq)]
pub(crate) enum ObjectStreamState {
    /// The object stream is currently being parsed.
    Loading,

    /// Parsing the object stream failed on an earlier attempt.
    Failed {
        error: String,
        exception: bool,
    },

    /// The object-stream index and decoded object data have been parsed.
    Parsed(ObjStream),
}

/// A PDF document being processed.
pub struct Pdf {
    /// The PDF input, normalized to begin at its `%PDF-` header and end at
    pub input: ddl::Input,

    /// The entries collected from the PDF's cross-reference sections.
    /// We only track the current generation of each object.
    pub(crate) entries: BTreeMap<u64, PdfObject>,

    /// Parsed object streams, indexed by their container object number.
    pub(crate) object_streams: BTreeMap<u64, ObjectStreamState>,

    /// The trailer from the newest cross-reference section.
    pub trailer: Option<TrailerDict>,

    /// The root object reference from the newest trailer, if present.
    pub root: Option<Ref>,

    /// The object currently being parsed and its resolution depth, for
    /// object-specific operations such as decryption.
    pub current_object: Option<(u64, u64, usize)>,
}

/// A PDF whose cross-reference information has been prepared for object
/// parsing.
pub type PdfCos = ddl::ParserStateWith<Pdf>;

/// An error encountered while locating or processing the PDF cross-reference
/// information.
#[derive(Debug)]
pub enum PdfError {
    PdfStartNotFound,
    StartXrefNotFound,
    StartXrefParse(String),
    XrefOffsetOutOfRange(usize),
    XrefCycle(usize),
    XrefParse { offset: usize, error: String },
    InvalidXrefInteger(&'static str),
    InvalidXrefOffset(&'static str),
}

impl fmt::Display for PdfError {
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

impl Error for PdfError {}

/// Construct a Daedalus input and prepare a PDF from its complete contents.
pub fn prepare_pdf_bytes(name: &str, bytes: &[u8]) -> Result<PdfCos, PdfError> {
    prepare_pdf(ddl::new_input(
        ddl::new_byte_array(name.as_bytes()),
        ddl::new_byte_array(bytes),
    ))
}

/// Iterate over the current references in object-number order.
pub fn references(pdf: &PdfCos) -> impl Iterator<Item = Ref> + '_ {
    pdf.user_state.entries.iter().map(|(&object, entry)| Ref {
        obj: ddl::Int::from(object),
        r#gen: ddl::Int::from(entry.generation),
    })
}

/// Locate the PDF header and process its cross-reference sections.
///
/// The returned prepared PDF retains an input normalized to the `%PDF-`
/// header, so all stored byte offsets use the PDF coordinate system.
pub fn prepare_pdf(input: ddl::Input) -> Result<PdfCos, PdfError> {
    let input_bytes = input.bytes();
    let pdf_start = find_bytes(&input_bytes, b"%PDF-").ok_or(PdfError::PdfStartNotFound)?;
    let input = input.advance(pdf_start);

    let pdf_bytes = input.bytes();
    let pdf_end =
        rfind_bytes(&pdf_bytes, b"startxref").ok_or(PdfError::StartXrefNotFound)?;

    let mut state = ddl::new_parser_state_with(Pdf {
        input: input.clone(),
        entries: BTreeMap::new(),
        object_streams: BTreeMap::new(),
        trailer: None,
        root: None,
        current_object: None,
    });
    let startxref =
        match crate::pdfcos_parsers::PdfXRef::PdfEnd(
            &mut state,
            input.clone().advance(pdf_end),
        ) {
        ddl::ParserResult::Ok(offset, _) => usize::try_from(u64::from(offset))
            .map_err(|_| PdfError::InvalidXrefOffset("startxref"))?,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
            return Err(PdfError::StartXrefParse(state.error.to_string()));
        }
    };

    let mut visited = BTreeSet::new();
    process_xref(&mut state, &input, &mut visited, startxref, true)?;
    Ok(state)
}

fn process_xref(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    offset: usize,
    top: bool,
) -> Result<(), PdfError> {
    if !visited.insert(offset) {
        return Err(PdfError::XrefCycle(offset));
    }

    let xref_input = match input.clone().advance_maybe(offset) {
        ddl::Maybe::Just(input) => input,
        ddl::Maybe::Nothing => return Err(PdfError::XrefOffsetOutOfRange(offset)),
    };

    let (xref, parse_error) = with_fresh_error(state, |state| {
        crate::pdfcos_parsers::PdfXRef::CrossRef(state, xref_input)
    });
    let xref = match xref {
        ddl::ParserResult::Ok(xref, _) => xref,
        ddl::ParserResult::Failure | ddl::ParserResult::Exception => {
            return Err(PdfError::XrefParse {
                offset,
                error: parse_error.to_string(),
            });
        }
    };

    match xref {
        CrossRef::oldXref(xref) => process_old_xref(state, input, visited, xref, top),
        CrossRef::newXref(xref) => process_new_xref(state, input, visited, xref, top),
    }
}

fn process_old_xref(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    xref: CrossRefAndTrailer,
    top: bool,
) -> Result<(), PdfError> {
    process_trailer_links(state, input, visited, &xref.trailer)?;

    for subsection in xref.xref.iter() {
        let mut object = int_to_u64(&subsection.firstId, "firstId")?;
        for entry in subsection.entries.iter() {
            match entry {
                CrossRefEntry::inUse(entry) => {
                    state.user_state.entries.insert(
                        object,
                        PdfObject {
                            generation: int_to_u64(&entry.r#gen, "generation")?,
                            state: ReferenceState::AtOffset(int_to_usize(
                                &entry.offset,
                                "object",
                            )?),
                        },
                    );
                }
                CrossRefEntry::free(_) => {
                    state.user_state.entries.remove(&object);
                }
            }
            object = object
                .checked_add(1)
                .ok_or(PdfError::InvalidXrefInteger("object number"))?;
        }
    }

    if top {
        record_top_trailer(&mut state.user_state, &xref.trailer);
    }
    Ok(())
}

fn process_new_xref(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    xref: XRefObjTable,
    top: bool,
) -> Result<(), PdfError> {
    process_trailer_links(state, input, visited, &xref.trailer)?;

    for subsection in xref.xref.iter() {
        let mut object = int_to_u64(&subsection.firstId, "firstId")?;
        for entry in subsection.entries.iter() {
            match entry {
                XRefObjEntry::inUse(entry) => {
                    state.user_state.entries.insert(
                        object,
                        PdfObject {
                            generation: int_to_u64(&entry.r#gen, "generation")?,
                            state: ReferenceState::AtOffset(int_to_usize(
                                &entry.offset,
                                "object",
                            )?),
                        },
                    );
                }
                XRefObjEntry::compressed(entry) => {
                    state.user_state.entries.insert(
                        object,
                        PdfObject {
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
                XRefObjEntry::free(_) => {
                    state.user_state.entries.remove(&object);
                }
                XRefObjEntry::null => {
                    state.user_state.entries.insert(
                        object,
                        PdfObject {
                            generation: 0,
                            state: ReferenceState::Null,
                        },
                    );
                }
            }
            object = object
                .checked_add(1)
                .ok_or(PdfError::InvalidXrefInteger("object number"))?;
        }
    }

    if top {
        record_top_trailer(&mut state.user_state, &xref.trailer);
    }
    Ok(())
}

fn process_trailer_links(
    state: &mut ddl::ParserStateWith<Pdf>,
    input: &ddl::Input,
    visited: &mut BTreeSet<usize>,
    trailer: &TrailerDict,
) -> Result<(), PdfError> {
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

fn record_top_trailer(table: &mut Pdf, trailer: &TrailerDict) {
    if let ddl::Maybe::Just(root) = &trailer.root {
        table.root = Some(root.clone());
    }
    table.trailer = Some(trailer.clone());
}

fn int_to_u64(value: &ddl::Int, field: &'static str) -> Result<u64, PdfError> {
    value
        .try_to_unsigned()
        .ok_or(PdfError::InvalidXrefInteger(field))
}

fn int_to_usize(value: &ddl::Int, field: &'static str) -> Result<usize, PdfError> {
    let value = value
        .try_to_unsigned()
        .ok_or(PdfError::InvalidXrefOffset(field))?;
    usize::try_from(value).map_err(|_| PdfError::InvalidXrefOffset(field))
}

/// Run an operation with a fresh parse error, then restore the caller's error.
pub(crate) fn with_fresh_error<T, R>(
    parser_state: &mut ddl::ParserStateWith<T>,
    action: impl FnOnce(&mut ddl::ParserStateWith<T>) -> R,
) -> (R, ddl::ParseError) {
    let previous = std::mem::replace(&mut parser_state.error, ddl::ParseError::new());
    let result = action(parser_state);
    let error = std::mem::replace(&mut parser_state.error, previous);
    (result, error)
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
