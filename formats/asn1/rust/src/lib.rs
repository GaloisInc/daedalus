use daedalus_rts_rust as ddl;

/// Parse a single DER-encoded ASN.1 value.
///
/// The parsed value tree is returned as JSON (the generated types are
/// `serde::Serialize`): union variants become one-key objects such as
/// `{"$integer": [42]}`, structs keep their Daedalus field names, and
/// byte arrays become arrays of numbers.  On failure the error message
/// produced by the parser is returned.
pub fn parse(bytes: &[u8]) -> Result<serde_json::Value, String> {
    let mut state = ddl::new_parser_state();
    let input = ddl::new_input(ddl::new_byte_array(b"input"), ddl::new_byte_array(bytes));
    match asn1::main(&mut state, input) {
        ddl::ParserResult::Ok(value, _remaining) => {
            serde_json::to_value(&value).map_err(|e| e.to_string())
        }
        ddl::ParserResult::Failure => Err(format!("parse error: {}", state.error)),
        ddl::ParserResult::Exception => Err(format!("exception: {}", state.error)),
    }
}
