#!/usr/bin/env bash
# Regenerate the committed certificate corpus.  Requires openssl 3.x.
#
# The .der files are committed so that running the tests does not need
# openssl.  Regenerating creates fresh keys, signatures, serial-less
# validity dates, etc., so the pinned assertions in
# ../../rust/tests/cert_tests.rs must be re-pinned afterwards.
set -euo pipefail
cd "$(dirname "$0")"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

SUBJ="/C=US/ST=Oregon/L=Portland/O=Galois/OU=Daedalus"

# RSA-2048, sha256WithRSAEncryption, full subject, fixed serial.
openssl req -x509 -newkey rsa:2048 -noenc -keyout "$TMP/k" -sha256 \
  -days 3650 -set_serial 0x1122334455667788 \
  -subj "$SUBJ/CN=rsa.example.com/emailAddress=test@example.com" \
  -outform DER -out rsa2048.der

# ECDSA P-256 (AlgorithmIdentifier parameters = named curve OID).
openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:P-256 -noenc \
  -keyout "$TMP/k" -sha256 -days 3650 -set_serial 4660 \
  -subj "$SUBJ/CN=ecdsa.example.com" \
  -outform DER -out ecdsa-p256.der

# Ed25519 (AlgorithmIdentifier parameters absent).
openssl req -x509 -newkey ed25519 -noenc -keyout "$TMP/k" \
  -days 3650 -set_serial 7 \
  -subj "$SUBJ/CN=ed25519.example.com" \
  -outform DER -out ed25519.der

# Rich subjectAltName: DNS, wildcard, IPv4, IPv6, email, URI.
openssl req -x509 -newkey rsa:2048 -noenc -keyout "$TMP/k" -sha256 \
  -days 3650 -set_serial 5001 \
  -subj "$SUBJ/CN=san.example.com" \
  -addext "subjectAltName=DNS:a.example.com,DNS:*.b.example.com,IP:192.0.2.1,IP:2001:db8::1,email:x@example.com,URI:https://example.com/path" \
  -outform DER -out san.der

# CA certificate: critical basicConstraints with a path length,
# critical keyUsage, SKI, and AKI.
openssl req -x509 -newkey rsa:2048 -noenc -keyout "$TMP/k" -sha256 \
  -days 3650 -set_serial 6001 \
  -subj "$SUBJ/CN=Daedalus Test CA" \
  -addext "basicConstraints=critical,CA:TRUE,pathlen:3" \
  -addext "keyUsage=critical,keyCertSign,cRLSign" \
  -addext "subjectKeyIdentifier=hash" \
  -addext "authorityKeyIdentifier=keyid" \
  -outform DER -out ca.der

# Extended key usage.
openssl req -x509 -newkey rsa:2048 -noenc -keyout "$TMP/k" -sha256 \
  -days 3650 -set_serial 7001 \
  -subj "$SUBJ/CN=eku.example.com" \
  -addext "extendedKeyUsage=serverAuth,clientAuth" \
  -outform DER -out eku.der

# Validity far enough out that notAfter must use GeneralizedTime
# (notBefore stays UTCTime: a mixed-variant Validity).
openssl req -x509 -newkey rsa:2048 -noenc -keyout "$TMP/k" -sha256 \
  -days 20000 -set_serial 8001 \
  -subj "$SUBJ/CN=gentime.example.com" \
  -outform DER -out gentime.der

# Version 1 certificate (no extensions, no [0] version field).  The
# default openssl config adds extensions (which forces v3), so use a
# minimal config to make -x509v1 effective.
printf '[req]\ndistinguished_name=dn\n[dn]\n' > "$TMP/min.cnf"
openssl req -x509v1 -config "$TMP/min.cnf" -newkey rsa:2048 -noenc \
  -keyout "$TMP/k" -sha256 -days 3650 -set_serial 42 \
  -subj "$SUBJ/CN=v1.example.com" \
  -outform DER -out v1.der

# Two real-world root certificates from the system trust store.
openssl x509 -in /etc/ssl/certs/ISRG_Root_X1.pem \
  -outform DER -out isrg-root-x1.der
openssl x509 -in /etc/ssl/certs/DigiCert_Global_Root_G2.pem \
  -outform DER -out digicert-global-root-g2.der

echo "Generated:"
ls -l ./*.der
