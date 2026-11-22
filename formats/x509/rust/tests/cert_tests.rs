//! Deep assertions on the committed certificate corpus (tests/certs/).
//!
//! Values are pinned from the actual generated certificates; after
//! `make regen-certs` the key- and date-dependent assertions must be
//! re-pinned (serial numbers and subjects are pinned by generate.sh and
//! survive regeneration; the real-world root certificates never change).

use serde_json::{Value, json};
use x509_tests::{attr, bytes_of, ext, key_usage_has, parse, tbs, text};

const RSA2048: &[u8] = include_bytes!("../../tests/certs/rsa2048.der");
const ECDSA_P256: &[u8] = include_bytes!("../../tests/certs/ecdsa-p256.der");
const ED25519: &[u8] = include_bytes!("../../tests/certs/ed25519.der");
const SAN: &[u8] = include_bytes!("../../tests/certs/san.der");
const CA: &[u8] = include_bytes!("../../tests/certs/ca.der");
const EKU: &[u8] = include_bytes!("../../tests/certs/eku.der");
const GENTIME: &[u8] = include_bytes!("../../tests/certs/gentime.der");
const V1: &[u8] = include_bytes!("../../tests/certs/v1.der");
const ISRG: &[u8] = include_bytes!("../../tests/certs/isrg-root-x1.der");
const DIGICERT: &[u8] = include_bytes!("../../tests/certs/digicert-global-root-g2.der");

const ALL: &[(&str, &[u8])] = &[
    ("rsa2048", RSA2048),
    ("ecdsa-p256", ECDSA_P256),
    ("ed25519", ED25519),
    ("san", SAN),
    ("ca", CA),
    ("eku", EKU),
    ("gentime", GENTIME),
    ("v1", V1),
    ("isrg-root-x1", ISRG),
    ("digicert-global-root-g2", DIGICERT),
];

// Attribute type OIDs.
const AT_COUNTRY: &[u64] = &[2, 5, 4, 6];
const AT_STATE: &[u64] = &[2, 5, 4, 8];
const AT_LOCALITY: &[u64] = &[2, 5, 4, 7];
const AT_ORG: &[u64] = &[2, 5, 4, 10];
const AT_ORG_UNIT: &[u64] = &[2, 5, 4, 11];
const AT_CN: &[u64] = &[2, 5, 4, 3];
const AT_EMAIL: &[u64] = &[1, 2, 840, 113549, 1, 9, 1];

// Extension OIDs.
const EXT_SKI: &[u64] = &[2, 5, 29, 14];
const EXT_KEY_USAGE: &[u64] = &[2, 5, 29, 15];
const EXT_SAN: &[u64] = &[2, 5, 29, 17];
const EXT_BASIC_CONSTRAINTS: &[u64] = &[2, 5, 29, 19];
const EXT_AKI: &[u64] = &[2, 5, 29, 35];
const EXT_EKU: &[u64] = &[2, 5, 29, 37];

// Signature / key algorithm OIDs.
const SHA256_WITH_RSA: [u64; 7] = [1, 2, 840, 113549, 1, 1, 11];
const RSA_ENCRYPTION: [u64; 7] = [1, 2, 840, 113549, 1, 1, 1];
const ECDSA_WITH_SHA256: [u64; 7] = [1, 2, 840, 10045, 4, 3, 2];
const ID_EC_PUBLIC_KEY: [u64; 6] = [1, 2, 840, 10045, 2, 1];
const PRIME256V1: [u64; 7] = [1, 2, 840, 10045, 3, 1, 7];
const ID_ED25519: [u64; 4] = [1, 3, 101, 112];

fn cert(bytes: &[u8]) -> Value {
    parse(bytes).expect("corpus certificate should parse")
}

fn time_variant<'a>(t: &'a Value) -> (&'a str, String) {
    let obj = t.as_object().expect("Time should be a union");
    let (k, v) = obj.iter().next().expect("Time union should have a variant");
    (k.as_str(), text(v))
}

//----------------------------------------------------------------------
// Whole-corpus properties

#[test]
fn all_corpus_certificates_parse() {
    for (name, bytes) in ALL {
        assert!(parse(bytes).is_ok(), "{name} failed to parse");
    }
}

#[test]
fn tbs_signature_matches_outer_signature_algorithm() {
    for (name, bytes) in ALL {
        let c = cert(bytes);
        assert_eq!(
            tbs(&c)["signature"], c["signatureAlgorithm"],
            "algorithm mismatch in {name}"
        );
    }
}

#[test]
fn all_corpus_certificates_are_self_signed_or_root() {
    // Everything in the corpus is self-signed: issuer == subject.
    for (name, bytes) in ALL {
        let c = cert(bytes);
        assert_eq!(tbs(&c)["issuer"], tbs(&c)["subject"], "in {name}");
    }
}

#[test]
fn signature_bit_strings_have_no_unused_bits() {
    for (name, bytes) in ALL {
        let c = cert(bytes);
        assert_eq!(c["signatureValue"]["unused"], json!(0), "in {name}");
        assert_eq!(
            tbs(&c)["subjectPublicKeyInfo"]["subjectPublicKey"]["unused"],
            json!(0),
            "in {name}"
        );
    }
}

#[test]
fn unique_ids_absent_in_corpus() {
    for (name, bytes) in ALL {
        let c = cert(bytes);
        assert_eq!(tbs(&c)["issuerUniqueID"], Value::Null, "in {name}");
        assert_eq!(tbs(&c)["subjectUniqueID"], Value::Null, "in {name}");
    }
}

//----------------------------------------------------------------------
// rsa2048.der

#[test]
fn rsa_version_is_v3() {
    assert_eq!(tbs(&cert(RSA2048))["version"], json!(2));
}

#[test]
fn rsa_serial_number() {
    // -set_serial 0x1122334455667788
    assert_eq!(
        tbs(&cert(RSA2048))["serialNumber"],
        json!([0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88])
    );
}

#[test]
fn rsa_signature_algorithm() {
    let c = cert(RSA2048);
    assert_eq!(c["signatureAlgorithm"]["algorithm"], json!(SHA256_WITH_RSA));
    // RSA parameters are an explicit ASN.1 NULL.
    assert_eq!(
        c["signatureAlgorithm"]["parameters"],
        json!({"$$just": {"$null": {}}})
    );
}

#[test]
fn rsa_subject_attributes() {
    let c = cert(RSA2048);
    let subject = &tbs(&c)["subject"];
    assert_eq!(attr(subject, AT_COUNTRY).as_deref(), Some("US"));
    assert_eq!(attr(subject, AT_STATE).as_deref(), Some("Oregon"));
    assert_eq!(attr(subject, AT_LOCALITY).as_deref(), Some("Portland"));
    assert_eq!(attr(subject, AT_ORG).as_deref(), Some("Galois"));
    assert_eq!(attr(subject, AT_ORG_UNIT).as_deref(), Some("Daedalus"));
    assert_eq!(attr(subject, AT_CN).as_deref(), Some("rsa.example.com"));
    assert_eq!(attr(subject, AT_EMAIL).as_deref(), Some("test@example.com"));
}

#[test]
fn rsa_subject_has_one_attribute_per_rdn() {
    let c = cert(RSA2048);
    let rdns = tbs(&c)["subject"].as_array().unwrap().clone();
    assert_eq!(rdns.len(), 7);
    for rdn in &rdns {
        assert_eq!(rdn.as_array().unwrap().len(), 1);
    }
}

#[test]
fn rsa_country_is_printable_string_and_cn_is_utf8() {
    let c = cert(RSA2048);
    let rdns = tbs(&c)["subject"].as_array().unwrap().clone();
    assert!(rdns[0][0]["attrValue"].get("$printableString").is_some());
    assert!(rdns[5][0]["attrValue"].get("$utf8String").is_some());
}

#[test]
fn rsa_validity_is_utc_time() {
    let c = cert(RSA2048);
    let validity = &tbs(&c)["validity"];
    let (k1, v1) = time_variant(&validity["notBefore"]);
    let (k2, v2) = time_variant(&validity["notAfter"]);
    assert_eq!((k1, k2), ("$utcTime", "$utcTime"));
    // YYMMDDHHMMSSZ
    assert_eq!(v1.len(), 13);
    assert_eq!(v2.len(), 13);
    assert!(v1.ends_with('Z') && v2.ends_with('Z'));
}

#[test]
fn rsa_spki() {
    let c = cert(RSA2048);
    let spki = &tbs(&c)["subjectPublicKeyInfo"];
    assert_eq!(spki["algorithm"]["algorithm"], json!(RSA_ENCRYPTION));
    assert_eq!(
        spki["algorithm"]["parameters"],
        json!({"$$just": {"$null": {}}})
    );
    // 2048-bit key: 270 bytes of RSAPublicKey structure.
    assert_eq!(spki["subjectPublicKey"]["bytes"].as_array().unwrap().len(), 270);
}

#[test]
fn rsa_signature_length() {
    // 2048-bit RSA signature = 256 bytes.
    let c = cert(RSA2048);
    assert_eq!(c["signatureValue"]["bytes"].as_array().unwrap().len(), 256);
}

#[test]
fn rsa_ski_is_20_bytes_and_aki_matches() {
    let c = cert(RSA2048);
    let (ski_critical, ski) = ext(&c, EXT_SKI).expect("SKI present");
    assert!(!ski_critical);
    let ski_bytes = bytes_of(&ski["$subjectKeyId"]);
    assert_eq!(ski_bytes.len(), 20);
    // Self-signed with AKI=keyid: the AKI keyIdentifier equals the SKI.
    let (aki_critical, aki) = ext(&c, EXT_AKI).expect("AKI present");
    assert!(!aki_critical);
    assert_eq!(bytes_of(&aki["$authorityKeyId"]["keyId"]["$$just"]), ski_bytes);
}

#[test]
fn rsa_basic_constraints_default_ca() {
    // openssl's default self-signed profile: critical basicConstraints CA:TRUE.
    let c = cert(RSA2048);
    let (critical, bc) = ext(&c, EXT_BASIC_CONSTRAINTS).expect("BC present");
    assert!(critical);
    assert_eq!(bc["$basicConstraints"], json!({"ca": true, "pathLen": null}));
}

//----------------------------------------------------------------------
// ecdsa-p256.der

#[test]
fn ecdsa_signature_algorithm_has_no_parameters() {
    let c = cert(ECDSA_P256);
    assert_eq!(c["signatureAlgorithm"]["algorithm"], json!(ECDSA_WITH_SHA256));
    assert_eq!(c["signatureAlgorithm"]["parameters"], Value::Null);
}

#[test]
fn ecdsa_serial_number() {
    // -set_serial 4660 = 0x1234
    assert_eq!(tbs(&cert(ECDSA_P256))["serialNumber"], json!([0x12, 0x34]));
}

#[test]
fn ecdsa_spki_names_the_curve() {
    let c = cert(ECDSA_P256);
    let spki = &tbs(&c)["subjectPublicKeyInfo"];
    assert_eq!(spki["algorithm"]["algorithm"], json!(ID_EC_PUBLIC_KEY));
    // EC parameters are the named-curve OID, not NULL.
    assert_eq!(
        spki["algorithm"]["parameters"],
        json!({"$$just": {"$oid": PRIME256V1}})
    );
}

#[test]
fn ecdsa_key_is_uncompressed_point() {
    let c = cert(ECDSA_P256);
    let key = bytes_of(&tbs(&c)["subjectPublicKeyInfo"]["subjectPublicKey"]["bytes"]);
    // 0x04 || x (32 bytes) || y (32 bytes)
    assert_eq!(key.len(), 65);
    assert_eq!(key[0], 0x04);
}

#[test]
fn ecdsa_subject_cn() {
    let c = cert(ECDSA_P256);
    assert_eq!(
        attr(&tbs(&c)["subject"], AT_CN).as_deref(),
        Some("ecdsa.example.com")
    );
}

//----------------------------------------------------------------------
// ed25519.der

#[test]
fn ed25519_algorithm_everywhere() {
    let c = cert(ED25519);
    assert_eq!(c["signatureAlgorithm"]["algorithm"], json!(ID_ED25519));
    let spki = &tbs(&c)["subjectPublicKeyInfo"];
    assert_eq!(spki["algorithm"]["algorithm"], json!(ID_ED25519));
    // RFC 8410: parameters MUST be absent.
    assert_eq!(spki["algorithm"]["parameters"], Value::Null);
    assert_eq!(c["signatureAlgorithm"]["parameters"], Value::Null);
}

#[test]
fn ed25519_key_and_signature_sizes() {
    let c = cert(ED25519);
    let key = &tbs(&c)["subjectPublicKeyInfo"]["subjectPublicKey"]["bytes"];
    assert_eq!(key.as_array().unwrap().len(), 32);
    assert_eq!(c["signatureValue"]["bytes"].as_array().unwrap().len(), 64);
}

#[test]
fn ed25519_single_byte_serial() {
    assert_eq!(tbs(&cert(ED25519))["serialNumber"], json!([7]));
}

//----------------------------------------------------------------------
// san.der

#[test]
fn san_extension_present_non_critical() {
    let (critical, _) = ext(&cert(SAN), EXT_SAN).expect("SAN present");
    assert!(!critical);
}

#[test]
fn san_entries_in_order() {
    let c = cert(SAN);
    let (_, san) = ext(&c, EXT_SAN).unwrap();
    let names = san["$subjectAltName"].as_array().unwrap().clone();
    assert_eq!(names.len(), 6);
    assert_eq!(text(&names[0]["$dnsName"]), "a.example.com");
    assert_eq!(text(&names[1]["$dnsName"]), "*.b.example.com");
    assert_eq!(bytes_of(&names[2]["$ipAddress"]), vec![192, 0, 2, 1]);
    assert_eq!(
        bytes_of(&names[3]["$ipAddress"]),
        vec![0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    );
    assert_eq!(text(&names[4]["$rfc822Name"]), "x@example.com");
    assert_eq!(text(&names[5]["$uri"]), "https://example.com/path");
}

#[test]
fn san_ipv4_and_ipv6_lengths() {
    let c = cert(SAN);
    let (_, san) = ext(&c, EXT_SAN).unwrap();
    let ips: Vec<usize> = san["$subjectAltName"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|n| n.get("$ipAddress"))
        .map(|ip| ip.as_array().unwrap().len())
        .collect();
    assert_eq!(ips, vec![4, 16]);
}

//----------------------------------------------------------------------
// ca.der

#[test]
fn ca_serial_number() {
    // -set_serial 6001 = 0x1771
    assert_eq!(tbs(&cert(CA))["serialNumber"], json!([0x17, 0x71]));
}

#[test]
fn ca_basic_constraints_with_path_length() {
    let c = cert(CA);
    let (critical, bc) = ext(&c, EXT_BASIC_CONSTRAINTS).expect("BC present");
    assert!(critical);
    assert_eq!(
        bc["$basicConstraints"],
        json!({"ca": true, "pathLen": {"$$just": 3}})
    );
}

#[test]
fn ca_key_usage_bits() {
    let c = cert(CA);
    let (critical, ku) = ext(&c, EXT_KEY_USAGE).expect("keyUsage present");
    assert!(critical);
    let bits = &ku["$keyUsage"];
    // keyUsage = keyCertSign (bit 5), cRLSign (bit 6): one byte 0000_0110
    // with 1 unused trailing bit.
    assert_eq!(bits["bytes"], json!([0x06]));
    assert_eq!(bits["unused"], json!(1));
    assert!(key_usage_has(bits, 5), "keyCertSign");
    assert!(key_usage_has(bits, 6), "cRLSign");
    assert!(!key_usage_has(bits, 0), "digitalSignature not set");
    assert!(!key_usage_has(bits, 2), "keyEncipherment not set");
}

#[test]
fn ca_has_ski_but_no_aki() {
    // openssl dropped the requested AKI here; pin the observed shape.
    let c = cert(CA);
    assert!(ext(&c, EXT_SKI).is_some());
    assert!(ext(&c, EXT_AKI).is_none());
}

#[test]
fn ca_subject_cn_with_spaces() {
    let c = cert(CA);
    assert_eq!(
        attr(&tbs(&c)["subject"], AT_CN).as_deref(),
        Some("Daedalus Test CA")
    );
}

//----------------------------------------------------------------------
// eku.der

#[test]
fn eku_lists_server_and_client_auth() {
    let c = cert(EKU);
    let (critical, eku) = ext(&c, EXT_EKU).expect("EKU present");
    assert!(!critical);
    assert_eq!(
        eku["$extKeyUsage"],
        json!([[1, 3, 6, 1, 5, 5, 7, 3, 1], [1, 3, 6, 1, 5, 5, 7, 3, 2]])
    );
}

//----------------------------------------------------------------------
// gentime.der

#[test]
fn gentime_has_mixed_time_variants() {
    // notBefore is near-term (UTCTime); notAfter is past 2050, which
    // RFC 5280 requires to be GeneralizedTime.
    let c = cert(GENTIME);
    let validity = &tbs(&c)["validity"];
    let (k1, v1) = time_variant(&validity["notBefore"]);
    let (k2, v2) = time_variant(&validity["notAfter"]);
    assert_eq!(k1, "$utcTime");
    assert_eq!(v1.len(), 13);
    assert_eq!(k2, "$generalizedTime");
    assert_eq!(v2.len(), 15); // YYYYMMDDHHMMSSZ
    assert!(v2.starts_with("20") && v2.ends_with('Z'));
}

//----------------------------------------------------------------------
// v1.der

#[test]
fn v1_version_is_zero() {
    assert_eq!(tbs(&cert(V1))["version"], json!(0));
}

#[test]
fn v1_has_no_extensions() {
    assert_eq!(tbs(&cert(V1))["extensions"], Value::Null);
}

#[test]
fn v1_serial_and_subject() {
    let c = cert(V1);
    assert_eq!(tbs(&c)["serialNumber"], json!([42]));
    assert_eq!(
        attr(&tbs(&c)["subject"], AT_CN).as_deref(),
        Some("v1.example.com")
    );
}

//----------------------------------------------------------------------
// isrg-root-x1.der (real-world: Let's Encrypt root)

#[test]
fn isrg_serial_keeps_leading_zero_pad() {
    // The serial's first content byte is 0x00 because the next byte has
    // its high bit set (positive INTEGER padding: required, minimal DER).
    let serial = bytes_of(&tbs(&cert(ISRG))["serialNumber"]);
    assert_eq!(serial.len(), 17);
    assert_eq!(serial[0], 0x00);
    assert_eq!(serial[1], 0x82);
    assert!(serial[1] & 0x80 != 0);
}

#[test]
fn isrg_subject() {
    let c = cert(ISRG);
    let subject = &tbs(&c)["subject"];
    assert_eq!(attr(subject, AT_COUNTRY).as_deref(), Some("US"));
    assert_eq!(
        attr(subject, AT_ORG).as_deref(),
        Some("Internet Security Research Group")
    );
    assert_eq!(attr(subject, AT_CN).as_deref(), Some("ISRG Root X1"));
}

#[test]
fn isrg_validity_pinned() {
    // Fixed forever: 2015-06-04 11:04:38 UTC to 2035-06-04 11:04:38 UTC.
    let c = cert(ISRG);
    let validity = &tbs(&c)["validity"];
    assert_eq!(time_variant(&validity["notBefore"]), ("$utcTime", "150604110438Z".into()));
    assert_eq!(time_variant(&validity["notAfter"]), ("$utcTime", "350604110438Z".into()));
}

#[test]
fn isrg_is_a_4096_bit_rsa_root() {
    let c = cert(ISRG);
    let spki = &tbs(&c)["subjectPublicKeyInfo"];
    assert_eq!(spki["algorithm"]["algorithm"], json!(RSA_ENCRYPTION));
    assert_eq!(spki["subjectPublicKey"]["bytes"].as_array().unwrap().len(), 526);
    assert_eq!(c["signatureValue"]["bytes"].as_array().unwrap().len(), 512);
}

#[test]
fn isrg_extensions() {
    let c = cert(ISRG);
    let (bc_critical, bc) = ext(&c, EXT_BASIC_CONSTRAINTS).unwrap();
    assert!(bc_critical);
    assert_eq!(bc["$basicConstraints"], json!({"ca": true, "pathLen": null}));
    let (ku_critical, ku) = ext(&c, EXT_KEY_USAGE).unwrap();
    assert!(ku_critical);
    assert!(key_usage_has(&ku["$keyUsage"], 5), "keyCertSign");
    assert!(key_usage_has(&ku["$keyUsage"], 6), "cRLSign");
    let (_, ski) = ext(&c, EXT_SKI).unwrap();
    assert_eq!(bytes_of(&ski["$subjectKeyId"]).len(), 20);
    assert!(ext(&c, EXT_SAN).is_none());
}

//----------------------------------------------------------------------
// digicert-global-root-g2.der (real-world)

#[test]
fn digicert_serial_and_subject() {
    let c = cert(DIGICERT);
    let serial = bytes_of(&tbs(&c)["serialNumber"]);
    assert_eq!(
        serial,
        vec![0x03, 0x3a, 0xf1, 0xe6, 0xa7, 0x11, 0xa9, 0xa0, 0xbb, 0x28, 0x64, 0xb1, 0x1d, 0x09, 0xfa, 0xe5]
    );
    let subject = &tbs(&c)["subject"];
    assert_eq!(attr(subject, AT_CN).as_deref(), Some("DigiCert Global Root G2"));
    assert_eq!(attr(subject, AT_ORG).as_deref(), Some("DigiCert Inc"));
    assert_eq!(attr(subject, AT_ORG_UNIT).as_deref(), Some("www.digicert.com"));
}

#[test]
fn digicert_validity_pinned() {
    let c = cert(DIGICERT);
    let validity = &tbs(&c)["validity"];
    assert_eq!(time_variant(&validity["notBefore"]), ("$utcTime", "130801120000Z".into()));
    assert_eq!(time_variant(&validity["notAfter"]), ("$utcTime", "380115120000Z".into()));
}

#[test]
fn digicert_key_usage_includes_digital_signature() {
    // 0x86 = digitalSignature (bit 0), keyCertSign (5), cRLSign (6).
    let c = cert(DIGICERT);
    let (_, ku) = ext(&c, EXT_KEY_USAGE).unwrap();
    assert_eq!(ku["$keyUsage"]["bytes"], json!([0x86]));
    assert!(key_usage_has(&ku["$keyUsage"], 0), "digitalSignature");
    assert!(key_usage_has(&ku["$keyUsage"], 5), "keyCertSign");
    assert!(key_usage_has(&ku["$keyUsage"], 6), "cRLSign");
}
