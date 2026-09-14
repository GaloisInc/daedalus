//! Test suite for the generated ASN.1 DER parser.
//!
//! Positive tests hand-encode DER values and compare the parsed result
//! against the expected JSON shape; negative tests check that invalid
//! (or merely BER-legal but DER-illegal) encodings are rejected.

use asn1_tests::parse;
use serde_json::{Value, json};

fn ok(bytes: &[u8]) -> Value {
    match parse(bytes) {
        Ok(v) => v,
        Err(e) => panic!("expected successful parse of {bytes:02X?}, got: {e}"),
    }
}

fn fails(bytes: &[u8]) {
    if let Ok(v) = parse(bytes) {
        panic!("expected parse failure for {bytes:02X?}, got: {v}");
    }
}

/// JSON shape of a byte string (arrays of numbers).
fn bytes_json(bytes: &[u8]) -> Value {
    json!(bytes.to_vec())
}

//----------------------------------------------------------------------
// BOOLEAN

#[test]
fn bool_true() {
    assert_eq!(ok(&[0x01, 0x01, 0xFF]), json!({"$boolean": true}));
}

#[test]
fn bool_false() {
    assert_eq!(ok(&[0x01, 0x01, 0x00]), json!({"$boolean": false}));
}

#[test]
fn bool_wrong_length_empty() {
    fails(&[0x01, 0x00]);
}

#[test]
fn bool_wrong_length_two_bytes() {
    fails(&[0x01, 0x02, 0xFF, 0xFF]);
}

#[test]
fn bool_noncanonical() {
    // 0x2A means "true" in BER, but DER requires exactly 0xFF.
    fails(&[0x01, 0x01, 0x2A]);
}

#[test]
fn bool_constructed() {
    fails(&[0x21, 0x01, 0xFF]);
}

//----------------------------------------------------------------------
// INTEGER (content is kept as raw big-endian two's-complement bytes)

#[test]
fn int_zero() {
    assert_eq!(ok(&[0x02, 0x01, 0x00]), json!({"$integer": [0]}));
}

#[test]
fn int_42() {
    assert_eq!(ok(&[0x02, 0x01, 0x2A]), json!({"$integer": [42]}));
}

#[test]
fn int_minus_one() {
    assert_eq!(ok(&[0x02, 0x01, 0xFF]), json!({"$integer": [255]}));
}

#[test]
fn int_256() {
    assert_eq!(ok(&[0x02, 0x02, 0x01, 0x00]), json!({"$integer": [1, 0]}));
}

#[test]
fn int_128_needs_leading_zero() {
    // 128 needs a 0x00 pad byte so it is not read as -128: valid DER.
    assert_eq!(ok(&[0x02, 0x02, 0x00, 0x80]), json!({"$integer": [0, 128]}));
}

#[test]
fn int_minus_128() {
    assert_eq!(ok(&[0x02, 0x01, 0x80]), json!({"$integer": [128]}));
}

#[test]
fn int_large_multibyte() {
    // 2^63 = 00 80 00 00 00 00 00 00 00: exercises values beyond i64.
    let der = [0x02, 0x09, 0x00, 0x80, 0, 0, 0, 0, 0, 0, 0];
    assert_eq!(
        ok(&der),
        json!({"$integer": [0, 128, 0, 0, 0, 0, 0, 0, 0]})
    );
}

#[test]
fn int_empty_content() {
    fails(&[0x02, 0x00]);
}

#[test]
fn int_nonminimal_leading_zero() {
    fails(&[0x02, 0x02, 0x00, 0x01]);
}

#[test]
fn int_nonminimal_leading_ff() {
    fails(&[0x02, 0x02, 0xFF, 0xFF]);
}

//----------------------------------------------------------------------
// BIT STRING

#[test]
fn bit_string() {
    assert_eq!(
        ok(&[0x03, 0x02, 0x06, 0xC0]),
        json!({"$bitString": {"unused": 6, "bytes": [0xC0]}})
    );
}

#[test]
fn bit_string_empty() {
    assert_eq!(
        ok(&[0x03, 0x01, 0x00]),
        json!({"$bitString": {"unused": 0, "bytes": []}})
    );
}

#[test]
fn bit_string_no_unused_bits() {
    assert_eq!(
        ok(&[0x03, 0x03, 0x00, 0xA5, 0x5A]),
        json!({"$bitString": {"unused": 0, "bytes": [0xA5, 0x5A]}})
    );
}

#[test]
fn bit_string_missing_unused_octet() {
    fails(&[0x03, 0x00]);
}

#[test]
fn bit_string_unused_too_large() {
    fails(&[0x03, 0x02, 0x08, 0x00]);
}

#[test]
fn bit_string_empty_with_unused_bits() {
    fails(&[0x03, 0x01, 0x03]);
}

#[test]
fn bit_string_nonzero_padding() {
    // Unused bits must be zero in DER: 0xC1 has bit 0 set with unused == 6.
    fails(&[0x03, 0x02, 0x06, 0xC1]);
}

//----------------------------------------------------------------------
// OCTET STRING

#[test]
fn octet_string() {
    assert_eq!(
        ok(&[0x04, 0x03, b'a', b'b', b'c']),
        json!({"$octetString": bytes_json(b"abc")})
    );
}

#[test]
fn octet_string_empty() {
    assert_eq!(ok(&[0x04, 0x00]), json!({"$octetString": []}));
}

#[test]
fn octet_string_constructed() {
    // Constructed strings are legal BER but rejected by DER.
    fails(&[0x24, 0x03, 0x04, 0x01, 0xAA]);
}

//----------------------------------------------------------------------
// NULL

#[test]
fn null() {
    assert_eq!(ok(&[0x05, 0x00]), json!({"$null": {}}));
}

#[test]
fn null_nonempty() {
    fails(&[0x05, 0x01, 0x00]);
}

//----------------------------------------------------------------------
// OBJECT IDENTIFIER

#[test]
fn oid_1_2_3_4() {
    assert_eq!(ok(&[0x06, 0x03, 0x2A, 0x03, 0x04]), json!({"$oid": [1, 2, 3, 4]}));
}

#[test]
fn oid_sha256_with_rsa() {
    // 1.2.840.113549.1.1.11, with multi-byte subidentifiers.
    assert_eq!(
        ok(&[0x06, 0x09, 0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0B]),
        json!({"$oid": [1, 2, 840, 113549, 1, 1, 11]})
    );
}

#[test]
fn oid_arc_two_large_second() {
    // First subidentifier 180 decodes to arcs 2.100.
    assert_eq!(ok(&[0x06, 0x03, 0x81, 0x34, 0x03]), json!({"$oid": [2, 100, 3]}));
}

#[test]
fn oid_single_subidentifier() {
    assert_eq!(ok(&[0x06, 0x01, 0x2A]), json!({"$oid": [1, 2]}));
}

#[test]
fn oid_empty_content() {
    fails(&[0x06, 0x00]);
}

#[test]
fn oid_truncated_subidentifier() {
    // Last content byte still has the continuation bit set.
    fails(&[0x06, 0x02, 0x2A, 0x86]);
}

#[test]
fn oid_nonminimal_subidentifier() {
    // 0x80 as the first byte of a subidentifier is a forbidden leading zero.
    fails(&[0x06, 0x02, 0x80, 0x01]);
}

//----------------------------------------------------------------------
// Character strings

#[test]
fn utf8_string() {
    assert_eq!(
        ok(&[0x0C, 0x05, b'h', b'e', b'l', b'l', b'o']),
        json!({"$utf8String": bytes_json(b"hello")})
    );
}

#[test]
fn utf8_string_non_ascii_bytes() {
    // UTF8String content is kept raw; non-ASCII bytes are accepted.
    assert_eq!(
        ok(&[0x0C, 0x02, 0xC3, 0xA9]),
        json!({"$utf8String": [0xC3, 0xA9]})
    );
}

#[test]
fn printable_string() {
    assert_eq!(
        ok(&[0x13, 0x05, b'A', b'B', b'C', b' ', b'9']),
        json!({"$printableString": bytes_json(b"ABC 9")})
    );
}

#[test]
fn printable_string_bad_character() {
    // '@' is not in the PrintableString character set.
    fails(&[0x13, 0x01, b'@']);
}

#[test]
fn ia5_string() {
    // '@' is fine in IA5String.
    assert_eq!(
        ok(&[0x16, 0x03, b'a', b'@', b'b']),
        json!({"$ia5String": bytes_json(b"a@b")})
    );
}

#[test]
fn ia5_string_non_ascii_byte() {
    fails(&[0x16, 0x01, 0x80]);
}

//----------------------------------------------------------------------
// Time types (validated for shape, returned as raw bytes)

fn time_der(tag: u8, body: &[u8]) -> Vec<u8> {
    let mut der = vec![tag, body.len() as u8];
    der.extend_from_slice(body);
    der
}

#[test]
fn utc_time() {
    let der = time_der(0x17, b"230101120000Z");
    assert_eq!(ok(&der), json!({"$utcTime": bytes_json(b"230101120000Z")}));
}

#[test]
fn utc_time_bad_terminator() {
    fails(&time_der(0x17, b"230101120000A"));
}

#[test]
fn utc_time_too_short() {
    // Two-digit seconds are mandatory in DER (11 digits is BER-only).
    fails(&time_der(0x17, b"23010112000Z"));
}

#[test]
fn generalized_time() {
    let der = time_der(0x18, b"20230101120000Z");
    assert_eq!(
        ok(&der),
        json!({"$generalizedTime": bytes_json(b"20230101120000Z")})
    );
}

#[test]
fn generalized_time_with_fraction() {
    let der = time_der(0x18, b"20230101120000.5Z");
    assert_eq!(
        ok(&der),
        json!({"$generalizedTime": bytes_json(b"20230101120000.5Z")})
    );
}

#[test]
fn generalized_time_multidigit_fraction() {
    let der = time_der(0x18, b"20230101120000.25Z");
    assert_eq!(
        ok(&der),
        json!({"$generalizedTime": bytes_json(b"20230101120000.25Z")})
    );
}

#[test]
fn generalized_time_trailing_zero_fraction() {
    // DER forbids trailing zeros in the fractional part.
    fails(&time_der(0x18, b"20230101120000.0Z"));
}

#[test]
fn generalized_time_empty_fraction() {
    fails(&time_der(0x18, b"20230101120000.Z"));
}

//----------------------------------------------------------------------
// SEQUENCE and SET

#[test]
fn sequence_empty() {
    assert_eq!(ok(&[0x30, 0x00]), json!({"$sequence": []}));
}

#[test]
fn sequence_two_integers() {
    assert_eq!(
        ok(&[0x30, 0x06, 0x02, 0x01, 0x01, 0x02, 0x01, 0x02]),
        json!({"$sequence": [{"$integer": [1]}, {"$integer": [2]}]})
    );
}

#[test]
fn set_two_integers() {
    assert_eq!(
        ok(&[0x31, 0x06, 0x02, 0x01, 0x01, 0x02, 0x01, 0x02]),
        json!({"$set": [{"$integer": [1]}, {"$integer": [2]}]})
    );
}

#[test]
fn sequence_mixed_nested() {
    assert_eq!(
        ok(&[0x30, 0x08, 0x30, 0x03, 0x02, 0x01, 0x01, 0x04, 0x01, b'A']),
        json!({"$sequence": [
            {"$sequence": [{"$integer": [1]}]},
            {"$octetString": bytes_json(b"A")},
        ]})
    );
}

#[test]
fn sequence_deeply_nested() {
    // Five nested SEQUENCEs around a NULL: exercises recursion.
    let der = [0x30, 0x0A, 0x30, 0x08, 0x30, 0x06, 0x30, 0x04, 0x30, 0x02, 0x05, 0x00];
    let mut expected = json!({"$null": {}});
    for _ in 0..5 {
        expected = json!({"$sequence": [expected]});
    }
    assert_eq!(ok(&der), expected);
}

#[test]
fn algorithm_identifier() {
    // A realistic X.509 fragment: AlgorithmIdentifier for sha256WithRSAEncryption.
    let der = [
        0x30, 0x0D, 0x06, 0x09, 0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0B,
        0x05, 0x00,
    ];
    assert_eq!(
        ok(&der),
        json!({"$sequence": [
            {"$oid": [1, 2, 840, 113549, 1, 1, 11]},
            {"$null": {}},
        ]})
    );
}

#[test]
fn sequence_primitive_encoding() {
    // SEQUENCE must have the constructed bit set.
    fails(&[0x10, 0x03, 0x02, 0x01, 0x01]);
}

#[test]
fn sequence_truncated_child() {
    // The last child's header is cut off by the SEQUENCE length.
    fails(&[0x30, 0x04, 0x02, 0x01, 0x01, 0x02]);
}

#[test]
fn sequence_child_garbage() {
    // 0xFF is not a complete TLV, so the SEQUENCE content does not parse.
    fails(&[0x30, 0x04, 0x02, 0x01, 0x01, 0xFF]);
}

#[test]
fn sequence_child_overruns_parent() {
    // Child claims 4 content bytes but the parent chunk only has 1 left.
    fails(&[0x30, 0x03, 0x02, 0x04, 0x01]);
}

//----------------------------------------------------------------------
// Non-universal classes and unknown universal tags

#[test]
fn context_specific_primitive() {
    assert_eq!(
        ok(&[0x80, 0x01, 0xFF]),
        json!({"$other": {
            "tagClass": 2, "constructed": false, "tagNum": 0,
            "content": {"$raw": [0xFF]},
        }})
    );
}

#[test]
fn context_specific_constructed() {
    // [0] EXPLICIT wrapping of an INTEGER; children are parsed recursively.
    assert_eq!(
        ok(&[0xA0, 0x03, 0x02, 0x01, 0x05]),
        json!({"$other": {
            "tagClass": 2, "constructed": true, "tagNum": 0,
            "content": {"$children": [{"$integer": [5]}]},
        }})
    );
}

#[test]
fn application_class() {
    assert_eq!(
        ok(&[0x41, 0x01, 0x00]),
        json!({"$other": {
            "tagClass": 1, "constructed": false, "tagNum": 1,
            "content": {"$raw": [0]},
        }})
    );
}

#[test]
fn private_class() {
    assert_eq!(
        ok(&[0xC1, 0x01, 0xAA]),
        json!({"$other": {
            "tagClass": 3, "constructed": false, "tagNum": 1,
            "content": {"$raw": [0xAA]},
        }})
    );
}

#[test]
fn unknown_universal_enumerated() {
    // ENUMERATED (tag 10) is not decoded specially; it falls back to `other`.
    assert_eq!(
        ok(&[0x0A, 0x01, 0x00]),
        json!({"$other": {
            "tagClass": 0, "constructed": false, "tagNum": 10,
            "content": {"$raw": [0]},
        }})
    );
}

//----------------------------------------------------------------------
// High tag numbers

#[test]
fn high_tag_number_31() {
    // 31 is the smallest tag that requires the high tag-number form.
    assert_eq!(
        ok(&[0x9F, 0x1F, 0x01, 0xAA]),
        json!({"$other": {
            "tagClass": 2, "constructed": false, "tagNum": 31,
            "content": {"$raw": [0xAA]},
        }})
    );
}

#[test]
fn high_tag_number_128() {
    // Multi-byte base-128 tag number: 0x81 0x00 = 128.
    assert_eq!(
        ok(&[0x9F, 0x81, 0x00, 0x01, 0xAA]),
        json!({"$other": {
            "tagClass": 2, "constructed": false, "tagNum": 128,
            "content": {"$raw": [0xAA]},
        }})
    );
}

#[test]
fn high_tag_leading_zero_septet() {
    // The first high-tag byte may not be 0x80 (non-minimal encoding).
    fails(&[0x9F, 0x80, 0x01, 0x01, 0xAA]);
}

#[test]
fn high_tag_fits_low_form() {
    // 30 must be encoded in the low form.
    fails(&[0x9F, 0x1E, 0x01, 0xAA]);
}

#[test]
fn high_tag_truncated() {
    fails(&[0x9F]);
}

//----------------------------------------------------------------------
// Lengths

fn octet_string_long_form(len_octets: &[u8], content_len: usize) -> Vec<u8> {
    let mut der = vec![0x04, 0x80 | len_octets.len() as u8];
    der.extend_from_slice(len_octets);
    der.extend(std::iter::repeat_n(0u8, content_len));
    der
}

#[test]
fn long_form_length_130() {
    let der = octet_string_long_form(&[130], 130);
    assert_eq!(ok(&der)["$octetString"].as_array().unwrap().len(), 130);
}

#[test]
fn long_form_length_300() {
    let der = octet_string_long_form(&[0x01, 0x2C], 300);
    assert_eq!(ok(&der)["$octetString"].as_array().unwrap().len(), 300);
}

#[test]
fn indefinite_length() {
    // Indefinite lengths are BER-only; DER always uses definite lengths.
    fails(&[0x30, 0x80, 0x02, 0x01, 0x01, 0x00, 0x00]);
}

#[test]
fn reserved_length_octet() {
    fails(&[0x04, 0xFF, 0x00]);
}

#[test]
fn long_form_for_short_length() {
    // Length 5 must use the short form.
    fails(&octet_string_long_form(&[5], 5));
}

#[test]
fn long_form_leading_zero() {
    fails(&octet_string_long_form(&[0x00, 130], 130));
}

#[test]
fn length_overruns_input() {
    fails(&[0x02, 0x05, 0x01, 0x02]);
}

//----------------------------------------------------------------------
// Framing

#[test]
fn empty_input() {
    fails(&[]);
}

#[test]
fn lone_tag_byte() {
    fails(&[0x02]);
}

#[test]
fn trailing_garbage() {
    fails(&[0x05, 0x00, 0x00]);
}
