//! Native functions used by the generated Daedalus parser.
//!
//! The reference resolver and stream-decoding primitives will be implemented
//! here as the corresponding milestones are reached.

use crate::pdfcos::{Ref, TopDecl};
use crate::state::ReferenceTable;
use daedalus_rts_rust as ddl;

pub fn resolve_ref(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _reference: Ref,
) -> ddl::ParserResult<ddl::Maybe<TopDecl>> {
    todo!()
}

pub fn decrypt(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

pub fn flate_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _predictor: ddl::Int,
    _colors: ddl::Int,
    _bits_per_component: ddl::Int,
    _columns: ddl::Int,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

pub fn lzw_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _predictor: ddl::Int,
    _colors: ddl::Int,
    _bits_per_component: ddl::Int,
    _columns: ddl::Int,
    _early_change: ddl::Int,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

pub fn ascii_hex_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}

pub fn ascii85_decode(
    _state: &mut ddl::ParserStateWith<ReferenceTable>,
    _input: ddl::Input,
    _body: ddl::Input,
) -> ddl::ParserResult<ddl::Input> {
    todo!()
}
