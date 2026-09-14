//! Structural tests on hand-built synthetic certificates.
//!
//! A tiny DER builder composes a minimal valid certificate; each
//! negative test then swaps one part for a malformed variant and checks
//! that the parser rejects it.  Signatures are structurally valid but
//! cryptographically meaningless -- the parser does not verify them.

use serde_json::{Value, json};
use x509_tests::{attr, parse, tbs};

//----------------------------------------------------------------------
// DER builder helpers

fn tlv(tag: u8, body: &[u8]) -> Vec<u8> {
    let mut out = vec![tag];
    let n = body.len();
    if n < 0x80 {
        out.push(n as u8);
    } else if n < 0x100 {
        out.extend_from_slice(&[0x81, n as u8]);
    } else {
        out.extend_from_slice(&[0x82, (n >> 8) as u8, n as u8]);
    }
    out.extend_from_slice(body);
    out
}

fn seq(parts: &[&[u8]]) -> Vec<u8> {
    tlv(0x30, &parts.concat())
}

/// AlgorithmIdentifier for sha256WithRSAEncryption with NULL parameters.
fn alg_id() -> Vec<u8> {
    seq(&[
        &tlv(0x06, &[0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0B]),
        &tlv(0x05, &[]),
    ])
}

/// Name with a single CN RDN: CN=t (PrintableString).
fn name() -> Vec<u8> {
    let atv = seq(&[&tlv(0x06, &[0x55, 0x04, 0x03]), &tlv(0x13, b"t")]);
    seq(&[&tlv(0x31, &atv)])
}

fn validity() -> Vec<u8> {
    seq(&[&tlv(0x17, b"260101000000Z"), &tlv(0x17, b"360101000000Z")])
}

fn spki() -> Vec<u8> {
    // A structurally valid SPKI; the key bytes are nonsense.
    seq(&[&alg_id(), &tlv(0x03, &[0x00, 0xAB, 0xCD])])
}

/// TBS fields shared by every variant: serial 1, algid, issuer,
/// validity, subject, SPKI.
fn tbs_tail() -> Vec<u8> {
    [
        tlv(0x02, &[0x01]),
        alg_id(),
        name(),
        validity(),
        name(),
        spki(),
    ]
    .concat()
}

/// Explicit [0] version field (0 = v1, 1 = v2, 2 = v3).
fn version_field(v: u8) -> Vec<u8> {
    tlv(0xA0, &tlv(0x02, &[v]))
}

/// [3] extensions wrapper around raw extension TLVs.
fn extensions_field(exts: &[&[u8]]) -> Vec<u8> {
    tlv(0xA3, &seq(exts))
}

/// An extension with the given OID body, criticality, and value bytes.
fn extension(oid_body: &[u8], critical: bool, value: &[u8]) -> Vec<u8> {
    let mut parts = tlv(0x06, oid_body);
    if critical {
        parts.extend_from_slice(&tlv(0x01, &[0xFF]));
    }
    parts.extend_from_slice(&tlv(0x04, value));
    seq(&[&parts])
}

fn cert_from_tbs(tbs: &[u8]) -> Vec<u8> {
    seq(&[tbs, &alg_id(), &tlv(0x03, &[0x00, 0xEE])])
}

/// A minimal valid v1 certificate.
fn minimal_v1() -> Vec<u8> {
    cert_from_tbs(&seq(&[&tbs_tail()]))
}

/// A minimal valid v3 certificate carrying the given extension TLVs.
fn v3_with_extensions(exts: &[&[u8]]) -> Vec<u8> {
    cert_from_tbs(&seq(&[
        &version_field(2),
        &tbs_tail(),
        &extensions_field(exts),
    ]))
}

const BC_OID: &[u8] = &[0x55, 0x1D, 0x13]; // 2.5.29.19
const UNKNOWN_OID: &[u8] = &[0x2B, 0x06, 0x01, 0x04, 0x01, 0x01]; // 1.3.6.1.4.1.1

fn ok(bytes: &[u8]) -> Value {
    match parse(bytes) {
        Ok(v) => v,
        Err(e) => panic!("expected successful parse, got: {e}"),
    }
}

fn fails(bytes: &[u8]) {
    if let Ok(v) = parse(bytes) {
        panic!("expected parse failure, got: {v}");
    }
}

//----------------------------------------------------------------------
// Positive: the builder produces certificates the parser accepts

#[test]
fn minimal_v1_parses() {
    let c = ok(&minimal_v1());
    assert_eq!(tbs(&c)["version"], json!(0));
    assert_eq!(tbs(&c)["serialNumber"], json!([1]));
    assert_eq!(attr(&tbs(&c)["subject"], &[2, 5, 4, 3]).as_deref(), Some("t"));
    assert_eq!(tbs(&c)["extensions"], Value::Null);
}

#[test]
fn explicit_v3_parses() {
    let bc = extension(BC_OID, true, &seq(&[&tlv(0x01, &[0xFF])]));
    let c = ok(&v3_with_extensions(&[&bc]));
    assert_eq!(tbs(&c)["version"], json!(2));
}

#[test]
fn explicit_v2_with_issuer_unique_id_parses() {
    // v2 + issuerUniqueID [1] IMPLICIT BIT STRING (no extensions).
    let tbs_der = seq(&[&version_field(1), &tbs_tail(), &tlv(0x81, &[0x00, 0x5A])]);
    let c = ok(&cert_from_tbs(&tbs_der));
    assert_eq!(tbs(&c)["version"], json!(1));
    assert_eq!(
        tbs(&c)["issuerUniqueID"],
        json!({"$$just": {"unused": 0, "bytes": [0x5A]}})
    );
}

#[test]
fn unknown_extension_kept_raw() {
    let unknown = extension(UNKNOWN_OID, false, &[0xDE, 0xAD, 0xBE, 0xEF]);
    let c = ok(&v3_with_extensions(&[&unknown]));
    let e = &tbs(&c)["extensions"]["$$just"][0];
    assert_eq!(e["critical"], json!(false));
    assert_eq!(e["extnValue"], json!({"$raw": [0xDE, 0xAD, 0xBE, 0xEF]}));
}

#[test]
fn critical_unknown_extension() {
    let unknown = extension(UNKNOWN_OID, true, &[0x00]);
    let c = ok(&v3_with_extensions(&[&unknown]));
    assert_eq!(tbs(&c)["extensions"]["$$just"][0]["critical"], json!(true));
}

#[test]
fn basic_constraints_empty_body_means_not_a_ca() {
    // Both DEFAULT/OPTIONAL fields absent: an empty SEQUENCE.
    let bc = extension(BC_OID, false, &seq(&[]));
    let c = ok(&v3_with_extensions(&[&bc]));
    assert_eq!(
        tbs(&c)["extensions"]["$$just"][0]["extnValue"],
        json!({"$basicConstraints": {"ca": false, "pathLen": null}})
    );
}

#[test]
fn basic_constraints_ca_with_path_len() {
    let bc = extension(
        BC_OID,
        true,
        &seq(&[&tlv(0x01, &[0xFF]), &tlv(0x02, &[0x05])]),
    );
    let c = ok(&v3_with_extensions(&[&bc]));
    assert_eq!(
        tbs(&c)["extensions"]["$$just"][0]["extnValue"],
        json!({"$basicConstraints": {"ca": true, "pathLen": {"$$just": 5}}})
    );
}

#[test]
fn san_directory_name_recurses_into_name() {
    // GeneralName [4] wraps a full Name.
    let san_oid: &[u8] = &[0x55, 0x1D, 0x11];
    let gn = tlv(0xA4, &name());
    let san = extension(san_oid, false, &seq(&[&gn]));
    let c = ok(&v3_with_extensions(&[&san]));
    let names = &tbs(&c)["extensions"]["$$just"][0]["extnValue"]["$subjectAltName"];
    assert_eq!(
        attr(&names[0]["$directoryName"], &[2, 5, 4, 3]).as_deref(),
        Some("t")
    );
}

#[test]
fn san_unknown_general_name_form() {
    // registeredID [8] is not decoded specially: otherForm fallback.
    let san_oid: &[u8] = &[0x55, 0x1D, 0x11];
    let gn = tlv(0x88, &[0x2A]);
    let san = extension(san_oid, false, &seq(&[&gn]));
    let c = ok(&v3_with_extensions(&[&san]));
    let names = &tbs(&c)["extensions"]["$$just"][0]["extnValue"]["$subjectAltName"];
    assert_eq!(
        names[0]["$otherForm"],
        json!({"tagNum": 8, "constructed": false, "content": {"$raw": [0x2A]}})
    );
}

#[test]
fn algorithm_parameters_may_be_absent() {
    // AlgorithmIdentifier with no parameters at all (like Ed25519).
    let alg = seq(&[&tlv(0x06, &[0x2B, 0x65, 0x70])]);
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x01]),
        alg.clone(),
        name(),
        validity(),
        name(),
        seq(&[&alg, &tlv(0x03, &[0x00, 0xAB])]),
    ]
    .concat()]);
    let c = ok(&seq(&[&tbs_der, &alg, &tlv(0x03, &[0x00, 0xEE])]));
    assert_eq!(tbs(&c)["signature"]["parameters"], Value::Null);
}

//----------------------------------------------------------------------
// Negative: framing

#[test]
fn empty_input_fails() {
    fails(&[]);
}

#[test]
fn trailing_garbage_fails() {
    let mut der = minimal_v1();
    der.push(0x00);
    fails(&der);
}

#[test]
fn outer_set_instead_of_sequence_fails() {
    let mut der = minimal_v1();
    der[0] = 0x31;
    fails(&der);
}

#[test]
fn truncated_certificate_fails() {
    let der = minimal_v1();
    fails(&der[..der.len() - 1]);
}

#[test]
fn missing_signature_value_fails() {
    fails(&seq(&[&seq(&[&tbs_tail()]), &alg_id()]));
}

#[test]
fn missing_signature_algorithm_fails() {
    fails(&seq(&[&seq(&[&tbs_tail()]), &tlv(0x03, &[0x00, 0xEE])]));
}

//----------------------------------------------------------------------
// Negative: version

#[test]
fn version_zero_encoded_explicitly_fails() {
    // [0] { INTEGER 0 } encodes the DEFAULT value: a DER violation.
    fails(&cert_from_tbs(&seq(&[&version_field(0), &tbs_tail()])));
}

#[test]
fn version_three_fails() {
    fails(&cert_from_tbs(&seq(&[&version_field(3), &tbs_tail()])));
}

#[test]
fn version_tag_primitive_fails() {
    // [0] must be constructed (EXPLICIT wrapper): 0x80 instead of 0xA0.
    let bad = tlv(0x80, &tlv(0x02, &[0x02]));
    fails(&cert_from_tbs(&seq(&[&bad, &tbs_tail()])));
}

#[test]
fn version_non_minimal_integer_fails() {
    let bad = tlv(0xA0, &tlv(0x02, &[0x00, 0x02]));
    fails(&cert_from_tbs(&seq(&[&bad, &tbs_tail()])));
}

#[test]
fn version_with_trailing_bytes_in_wrapper_fails() {
    let bad = tlv(0xA0, &[tlv(0x02, &[0x02]), vec![0x00]].concat());
    fails(&cert_from_tbs(&seq(&[&bad, &tbs_tail()])));
}

//----------------------------------------------------------------------
// Negative: version gating of optional fields

#[test]
fn v1_with_extensions_fails() {
    let unknown = extension(UNKNOWN_OID, false, &[0x00]);
    // No version field (v1), but [3] extensions present.
    fails(&cert_from_tbs(&seq(&[
        &tbs_tail(),
        &extensions_field(&[&unknown]),
    ])));
}

#[test]
fn v1_with_issuer_unique_id_fails() {
    fails(&cert_from_tbs(&seq(&[&tbs_tail(), &tlv(0x81, &[0x00, 0x5A])])));
}

#[test]
fn v2_with_extensions_fails() {
    let unknown = extension(UNKNOWN_OID, false, &[0x00]);
    fails(&cert_from_tbs(&seq(&[
        &version_field(1),
        &tbs_tail(),
        &extensions_field(&[&unknown]),
    ])));
}

//----------------------------------------------------------------------
// Negative: serial number

#[test]
fn missing_serial_fails() {
    let tbs_der = seq(&[&[alg_id(), name(), validity(), name(), spki()].concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

#[test]
fn constructed_serial_fails() {
    let mut tail = tbs_tail();
    tail[0] = 0x22; // INTEGER with the constructed bit set
    fails(&cert_from_tbs(&seq(&[&tail])));
}

#[test]
fn non_minimal_serial_fails() {
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x00, 0x01]), // redundant leading 0x00
        alg_id(),
        name(),
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

#[test]
fn empty_serial_fails() {
    let tbs_der = seq(&[&[tlv(0x02, &[]), alg_id(), name(), validity(), name(), spki()].concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

//----------------------------------------------------------------------
// Negative: validity

fn cert_with_validity(v: &[u8]) -> Vec<u8> {
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x01]),
        alg_id(),
        name(),
        v.to_vec(),
        name(),
        spki(),
    ]
    .concat()]);
    cert_from_tbs(&tbs_der)
}

#[test]
fn time_with_octet_string_tag_fails() {
    let v = seq(&[&tlv(0x04, b"260101000000Z"), &tlv(0x17, b"360101000000Z")]);
    fails(&cert_with_validity(&v));
}

#[test]
fn utc_time_bad_terminator_fails() {
    let v = seq(&[&tlv(0x17, b"260101000000A"), &tlv(0x17, b"360101000000Z")]);
    fails(&cert_with_validity(&v));
}

#[test]
fn utc_time_too_short_fails() {
    let v = seq(&[&tlv(0x17, b"2601010000Z"), &tlv(0x17, b"360101000000Z")]);
    fails(&cert_with_validity(&v));
}

#[test]
fn generalized_time_with_trailing_zero_fraction_fails() {
    let v = seq(&[
        &tlv(0x17, b"260101000000Z") as &[u8],
        &tlv(0x18, b"20810101000000.50Z"),
    ]);
    fails(&cert_with_validity(&v));
}

#[test]
fn missing_not_after_fails() {
    let v = seq(&[&tlv(0x17, b"260101000000Z")]);
    fails(&cert_with_validity(&v));
}

#[test]
fn constructed_time_fails() {
    let v = seq(&[&tlv(0x37, b"260101000000Z"), &tlv(0x17, b"360101000000Z")]);
    fails(&cert_with_validity(&v));
}

//----------------------------------------------------------------------
// Negative: names

#[test]
fn empty_rdn_set_fails() {
    // RDN ::= SET SIZE (1..MAX): an empty SET is invalid.
    let bad_name = seq(&[&tlv(0x31, &[])]);
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x01]),
        alg_id(),
        bad_name,
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

#[test]
fn atv_missing_value_fails() {
    let bad_atv = seq(&[&tlv(0x06, &[0x55, 0x04, 0x03])]);
    let bad_name = seq(&[&tlv(0x31, &bad_atv)]);
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x01]),
        alg_id(),
        bad_name,
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

#[test]
fn name_as_set_fails() {
    let mut bad_name = name();
    bad_name[0] = 0x31; // Name must be a SEQUENCE of RDNs
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x01]),
        alg_id(),
        bad_name,
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

//----------------------------------------------------------------------
// Negative: algorithm identifier

#[test]
fn algorithm_identifier_trailing_garbage_fails() {
    // OID + NULL + a stray byte inside the AlgorithmIdentifier SEQUENCE.
    let bad_alg = seq(&[
        &tlv(0x06, &[0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0B]) as &[u8],
        &tlv(0x05, &[]),
        &[0x00],
    ]);
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x01]),
        bad_alg,
        name(),
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

#[test]
fn algorithm_identifier_missing_oid_fails() {
    let bad_alg = seq(&[&tlv(0x05, &[])]);
    let tbs_der = seq(&[&[
        tlv(0x02, &[0x01]),
        bad_alg,
        name(),
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

//----------------------------------------------------------------------
// Negative: extensions

#[test]
fn empty_extensions_sequence_fails() {
    // Extensions ::= SEQUENCE SIZE (1..MAX) OF Extension.
    fails(&cert_from_tbs(&seq(&[
        &version_field(2),
        &tbs_tail(),
        &extensions_field(&[]),
    ])));
}

#[test]
fn critical_false_encoded_explicitly_fails() {
    // critical BOOLEAN DEFAULT FALSE: encoding FALSE violates DER.
    let ext_der = seq(&[&[
        tlv(0x06, UNKNOWN_OID),
        tlv(0x01, &[0x00]),
        tlv(0x04, &[0x00]),
    ]
    .concat()]);
    fails(&v3_with_extensions(&[&ext_der]));
}

#[test]
fn critical_non_canonical_boolean_fails() {
    let ext_der = seq(&[&[
        tlv(0x06, UNKNOWN_OID),
        tlv(0x01, &[0x2A]), // BER-true, DER-invalid
        tlv(0x04, &[0x00]),
    ]
    .concat()]);
    fails(&v3_with_extensions(&[&ext_der]));
}

#[test]
fn extn_id_not_an_oid_fails() {
    let ext_der = seq(&[&[tlv(0x02, &[0x01]), tlv(0x04, &[0x00])].concat()]);
    fails(&v3_with_extensions(&[&ext_der]));
}

#[test]
fn basic_constraints_ca_false_encoded_fails() {
    // cA BOOLEAN DEFAULT FALSE: encoding FALSE violates DER.
    let bc = extension(BC_OID, false, &seq(&[&tlv(0x01, &[0x00])]));
    fails(&v3_with_extensions(&[&bc]));
}

#[test]
fn basic_constraints_negative_path_len_fails() {
    let bc = extension(
        BC_OID,
        false,
        &seq(&[&tlv(0x01, &[0xFF]), &tlv(0x02, &[0x80])]),
    );
    fails(&v3_with_extensions(&[&bc]));
}

#[test]
fn basic_constraints_trailing_bytes_fails() {
    // Valid empty BasicConstraints followed by a stray byte inside
    // extnValue.
    let bc = extension(BC_OID, false, &[seq(&[]), vec![0x00]].concat());
    fails(&v3_with_extensions(&[&bc]));
}

#[test]
fn san_empty_fails() {
    // GeneralNames ::= SEQUENCE SIZE (1..MAX).
    let san = extension(&[0x55, 0x1D, 0x11], false, &seq(&[]));
    fails(&v3_with_extensions(&[&san]));
}

#[test]
fn san_dns_name_with_non_ia5_byte_fails() {
    let gn = tlv(0x82, &[b'a', 0x80, b'b']);
    let san = extension(&[0x55, 0x1D, 0x11], false, &seq(&[&gn]));
    fails(&v3_with_extensions(&[&san]));
}

#[test]
fn san_universal_tag_fails() {
    // GeneralName alternatives are context-tagged; a plain IA5String
    // (universal 22) is not acceptable.
    let gn = tlv(0x16, b"a.example.com");
    let san = extension(&[0x55, 0x1D, 0x11], false, &seq(&[&gn]));
    fails(&v3_with_extensions(&[&san]));
}

#[test]
fn key_usage_with_nonzero_padding_fails() {
    // 1 unused bit but that bit is set.
    let ku = extension(&[0x55, 0x1D, 0x0F], true, &tlv(0x03, &[0x01, 0x07]));
    fails(&v3_with_extensions(&[&ku]));
}

//----------------------------------------------------------------------
// Negative: DER encoding violations inside the TBS

#[test]
fn indefinite_length_fails() {
    // Serial with indefinite length marker.
    let tbs_der = seq(&[&[
        vec![0x02, 0x80, 0x01, 0x00, 0x00],
        alg_id(),
        name(),
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

#[test]
fn long_form_length_for_short_value_fails() {
    // Serial of length 1 encoded with a long-form length.
    let tbs_der = seq(&[&[
        vec![0x02, 0x81, 0x01, 0x2A],
        alg_id(),
        name(),
        validity(),
        name(),
        spki(),
    ]
    .concat()]);
    fails(&cert_from_tbs(&tbs_der));
}

#[test]
fn signature_bit_string_unused_over_seven_fails() {
    fails(&seq(&[&seq(&[&tbs_tail()]), &alg_id(), &tlv(0x03, &[0x08, 0xEE])]));
}

#[test]
fn signature_bit_string_nonzero_padding_fails() {
    // 4 unused bits, but the low nibble of the last byte is not zero.
    fails(&seq(&[&seq(&[&tbs_tail()]), &alg_id(), &tlv(0x03, &[0x04, 0xEF])]));
}

#[test]
fn empty_bit_string_with_unused_bits_fails() {
    fails(&seq(&[&seq(&[&tbs_tail()]), &alg_id(), &tlv(0x03, &[0x03])]));
}

#[test]
fn unique_id_bad_bit_string_fails() {
    // v2 + issuerUniqueID whose unused-bits count is out of range.
    let tbs_der = seq(&[&version_field(1), &tbs_tail(), &tlv(0x81, &[0x08, 0x5A])]);
    fails(&cert_from_tbs(&tbs_der));
}
