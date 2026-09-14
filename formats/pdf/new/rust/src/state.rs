//! Application state shared by the generated parsers and native functions.

/// Reference table and lazy object cache used while processing a PDF.
///
/// Its fields will be added as reference and cross-reference processing are
/// implemented.
#[derive(Default)]
pub struct ReferenceTable;
