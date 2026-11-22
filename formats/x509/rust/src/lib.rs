use daedalus_rts_rust as ddl;
use serde_json::Value;

/// Parse a DER-encoded X.509 certificate.
///
/// The parsed certificate is returned as JSON (the generated types are
/// `serde::Serialize`): structs keep their Daedalus field names, union
/// variants become one-key objects such as `{"$utcTime": [...]}`,
/// optional fields are `null` or `{"$$just": value}`, and byte arrays
/// become arrays of numbers.  On failure the parser's error message is
/// returned.
pub fn parse(bytes: &[u8]) -> Result<Value, String> {
    let mut state = ddl::new_parser_state();
    let input = ddl::new_input(ddl::new_byte_array(b"input"), ddl::new_byte_array(bytes));
    match x509::main(&mut state, input) {
        ddl::ParserResult::Ok(value, _remaining) => {
            serde_json::to_value(&value).map_err(|e| e.to_string())
        }
        ddl::ParserResult::Failure => Err(format!("parse error: {}", state.error)),
        ddl::ParserResult::Exception => Err(format!("exception: {}", state.error)),
    }
}

/// The tbsCertificate of a parsed certificate.
pub fn tbs(cert: &Value) -> &Value {
    &cert["tbsCertificate"]
}

/// Decode a JSON byte array (e.g. a parsed string value) as text.
pub fn text(v: &Value) -> String {
    bytes_of(v).into_iter().map(|b| b as char).collect()
}

/// Decode a JSON byte array into bytes.
pub fn bytes_of(v: &Value) -> Vec<u8> {
    v.as_array()
        .expect("expected a JSON array of bytes")
        .iter()
        .map(|b| b.as_u64().expect("expected a byte") as u8)
        .collect()
}

/// Find the first attribute with the given type OID in a Name (an array
/// of RDNs) and decode its value as text.
pub fn attr(name: &Value, oid: &[u64]) -> Option<String> {
    let oid = Value::from(oid.to_vec());
    for rdn in name.as_array()? {
        for atv in rdn.as_array()? {
            if atv["attrType"] == oid {
                // The value is a one-key union: {"$utf8String": [...]} etc.
                let inner = atv["attrValue"].as_object()?.values().next()?;
                return Some(text(inner));
            }
        }
    }
    None
}

/// Find an extension by OID; returns (critical, extnValue).  The
/// extnValue is a one-key union keyed by the decoded form, e.g.
/// `{"$basicConstraints": {...}}` or `{"$raw": [...]}`.
pub fn ext<'a>(cert: &'a Value, oid: &[u64]) -> Option<(bool, &'a Value)> {
    let oid = Value::from(oid.to_vec());
    let exts = tbs(cert)["extensions"]["$$just"].as_array()?;
    for e in exts {
        if e["extnID"] == oid {
            return Some((e["critical"].as_bool()?, &e["extnValue"]));
        }
    }
    None
}

/// Test a KeyUsage bit by its RFC 5280 position (0 = digitalSignature,
/// 5 = keyCertSign, 6 = cRLSign, ...).  Bit 0 is the most significant
/// bit of the first content byte.
pub fn key_usage_has(bit_string: &Value, bit: usize) -> bool {
    let bytes = bytes_of(&bit_string["bytes"]);
    bytes
        .get(bit / 8)
        .is_some_and(|b| b & (0x80 >> (bit % 8)) != 0)
}
