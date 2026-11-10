{-
A parser for Variant Call Format v4.2 files, as specified in
https://samtools.github.io/hts-specs/VCFv4.2.pdf.

Where this specification refers to section numbers, it is referring to sections
in the linked specification.
-}

import Daedalus

import Util


-- v4.2 of the specification does not dictate how lines should end, but
-- subsequent versions say that LF and CRLF conventions are both valid, so we
-- recognize either here.
def Newline =
  block
    Optional $cr
    @$lf


def StructuredEntry =
  block
    key = Many1 $[ $printable - '=' ]
    $['=']
    val =
      First
        Many1 $[ $printable - ',' - '>' - $dq ]
        QuotedString


def StructuredVal: [StructuredEntry] =
  block
    $['<']
    $$ = SepBy $[','] StructuredEntry
    $['>']


-- This is a summary of the various forms of metadata that appear in the
-- specification; we do not currently parse each of the metadata forms specified
-- in 1.2 individually.
def MetaVal =
  First
    unstructured =
      First
        Many1 $[ $printable - $dq - '<' ]
        QuotedString
    structured = StructuredVal


-- 1.2: File meta-information is included after the ## string and must be
-- key=value pairs.
def MetaLine =
  block
    Match "##"
    key = Many1 $[ $printable - '=' ]
    $['=']
    val = MetaVal


-- 1.3.1: The header line names the 8 fixed, mandatory columns.
-- ...
-- If genotype data is present in the file, these are followed by a FORMAT
-- column header, then an arbitrary number of sample IDs.
-- ...
-- The header line is tab-delimited.
def HeaderLine =
  block
    Match "#CHROM"
    $tab
    Match "POS"
    $tab
    Match "ID"
    $tab
    Match "REF"
    $tab
    Match "ALT"
    $tab
    Match "QUAL"
    $tab
    Match "FILTER"
    $tab
    Match "INFO"
    samples =
      First
        block
          $tab
          Match "FORMAT"
          let b =
            many (samples = builder)
              block
                $tab
                let sample = Many1 $[ $printable ]
                emit samples sample
          build b

        []


-- 1.4.1: In all cases, missing values are specified with a dot ('.')
def D (P: ?a): maybe ?a =
  First
    block
      $['.']
      nothing

    just P


-- 1.4.1: CHROM - chromosome: An identifier from the reference genome or an
-- angle-bracketed ID String ("<ID>")
-- ...
-- String, no whitespace permitted, Required
def Chrom: [uint 8] =
  First
    Many1 $[ $printable - ' ' - '.' - '<' ]
    Bracketed ($printable - ' ' - '.')


-- 1.4.1: POS - position: The reference position, with the 1st base having
-- position 1
-- ...
-- Integer, Required
def Pos: uint 64 = Number


-- 1.4.1: ID - identifier: Semicolon-separated list of unique identifiers where
-- available
-- ...
-- String, no whitespace or semicolons permitted
def Id: maybe [[uint 8]] = D (SepBy1 $[';'] (Many $[ $printable - ' ' - ';']))


def $base = 'a' | 'c' | 'g' | 't' | 'n' | 'A' | 'C' | 'G' | 'T' | 'N'


-- 1.4.1: REF - reference base(s): Each base must be one of A,C,G,T,N (case
-- insensitive). Multiple bases are permitted.
-- ...
-- String, Required
def Ref: [uint 8] = Many1 $base


def AltBase: [uint 8] =
  First
    Bracketed ($printable - ' ' - ',' - '<')
    Many1 $[ $base | '*' ]

-- 1.4.1: ALT - alternate base(s): Comma separated list of alternate
-- non-reference alleles.
-- ...
-- Options are base Strings made up of the bases A,C,G,T,N,*, (case insensitive)
-- or an angle-bracketed ID String ("<ID>") or a breakend replacement string as
-- described in the section on breakends.
-- ...
-- String; no whitespace, commas, or angle-brackets are permitted in the ID
-- String itself
def Alt: maybe [[uint 8]] = D (SepBy1 $[','] AltBase)


-- 1.4.1: QUAL - quality: Phred-scaled quality score for the assertion made in
-- ALT
-- ...
-- Numeric
def Qual: maybe (uint 64) = D Number


-- 1.4.1: FILTER - filter status: PASS if this position has passed all filters,
-- i.e., a call is made at this position. Otherwise, if the site has not passed
-- all filters, a semicolon-separated list of codes for filters that fail.
-- ...
-- String, no whitespace or semicolons permitted
def Filter: maybe [[uint 8]] =
  D
    First
      { Match "PASS"; [] }
      SepBy1 $[';'] (Many $[ $printable - ' ' - ';'])


def InfoKV =
  block
    key = Many $[ $printable - ' ' - ';' - '=' - ',' ]
    vals =
      First
        block
          $['=']
          SepBy1 $[','] (Many $[ $printable - ' ' - ';' - '=' - ',' ])

        []

-- 1.4.1: INFO - additional information: ... INFO fields are encoded as a
-- semicolon-separated series of short keys with optional values in the format:
-- <key>=<data>[,data].
-- ...
-- String, no whitespace, semicolons, or equals-signs permitted; commas are
-- permitted only as delimiters for lists of values
def Info: maybe [InfoKV] = D (SepBy1 $[';'] InfoKV)


-- 1.4.2: First a FORMAT field is given specifying the data types and order
-- (colon-separated alphanumeric String).
-- ...
-- The first sub-field must always be the genotype (GT) if it is present.
def Format: [[uint 8]] =
  block
    let tys = SepBy1 $[':'] (Many1 $alnum)
    ((Index tys 0) == "GT") is true
    tys


def Sample: [maybe [uint 8]] =
  -- TODO: explicitly parse GT to recognize "./."
  block
    SepBy1 $[':'] (D (Many $[ $printable - ':' ]))


-- 1.4.2: [The FORMAT field] is followed by one field per sample, with the
-- colon-separated data in this field corresponding to the types specified in
-- the format.
def Samples (sample_names: [[uint 8]]) =
  if length sample_names == 0
    then []
    else
      block
        $tab
        let format = Format
        let b =
          for (samples = builder; sample_name in sample_names)
            block
              $tab
              let s = Sample
              emit samples s
        build b


def DataLine (sample_names: [[uint 8]]) =
  block
    chrom = Chrom
    $tab
    pos = Pos
    $tab
    id = Id
    $tab
    ref = Ref
    $tab
    alt = Alt
    $tab
    qual = Qual
    $tab
    filter = Filter
    $tab
    info = Info
    samples = Samples sample_names


def Main =
  block
    meta = SepBy Newline MetaLine
    Newline
    hdr = HeaderLine
    Newline
    data = SepBy Newline (DataLine hdr.samples)
    Many Newline


def Test =
  block
    TestParses MetaLine
      "##fileformat=VCFv4.2"
      { key = "fileformat",
        val = {| unstructured = "VCFv4.2" |}
      }
    TestParses MetaLine
      "##fileDate=20190402"
      { key = "fileDate",
        val = {| unstructured = "20190402" |}
      }
    TestFails MetaLine
      "#fileformat=VCFv4.2"
    TestParses MetaLine
      "##contig=<ID=chr1,length=248956422,assembly=GRCh38>"
      { key = "contig",
        val = {|
          structured = [
            { key = "ID", val = "chr1" },
            { key = "length", val = "248956422" },
            { key = "assembly", val = "GRCh38" }
          ]
        |}
      }
    -- Unclosed bracket (should we parse this as an unstructured value?)
    TestFails MetaLine
      "##contig=<ID=chr1,length=248956422,assembly=GRCh38"

    TestParses MetaLine
      "##source=\"Keystone Molecular Diagnostics: KMD OncoFocus 523-gene tumor panel (tumor-only)\""
      { key = "source",
        val = {| unstructured = "Keystone Molecular Diagnostics: KMD OncoFocus 523-gene tumor panel (tumor-only)" |}
      }
    TestFails MetaLine
      "##source=\"Keystone Molecular Diagnostics: KMD OncoFocus 523-gene tumor panel (tumor-only)"
    TestParses MetaLine
      "##FILTER=<ID=PASS,Description=\"All filters passed\">"
      { key = "FILTER",
        val = {|
          structured = [
            { key = "ID", val = "PASS" },
            { key = "Description", val = "All filters passed" }
          ]
        |}
      }
    TestFails MetaLine
      "##FILTER=<ID=PASS,Description=\"All filters passed>"

    TestParses HeaderLine
      "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO"
      { samples = [] }
    TestParses HeaderLine
      "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT"
      { samples = [] }
    TestParses HeaderLine
      "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tSYN-0001-T1-FFPE"
      { samples = ["SYN-0001-T1-FFPE"] }

    TestFails HeaderLine
      "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINF"
    TestFails HeaderLine
      "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER INFO"
    TestFails HeaderLine
      "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tNOT-FORMAT"

    TestParses Chrom
      "chr17"
      "chr17"
    TestParses Chrom
      "<chr17>"
      "chr17"
    TestFails Chrom
      "."

    TestParses Pos
      "7673803"
      7673803
    TestFails Pos
      "."

    TestParses Id
      "."
      nothing
    TestParses Id
      "foo"
      (just ["foo"])
    TestParses Id
      "foo;bar"
      (just ["foo", "bar"])

    TestParses Ref
      "A"
      ['A']
    TestParses Ref
      "AC"
      ['A', 'C']
    TestParses Ref
      "Ac"
      ['A', 'c']
    TestFails Ref
      "B"
    TestFails Ref
      "."

    TestParses Alt
      "A"
      (just ["A"])
    TestParses Alt
      "AC"
      (just ["AC"])
    TestParses Alt
      "AC,T*"
      (just ["AC", "T*"])
    TestParses Alt
      "<B>"
      (just ["B"])
    TestParses Alt
      "."
      nothing
    TestFails Alt
      "B"

    TestParses Qual
      "7673803"
      (just 7673803)
    TestParses Qual
      "."
      nothing

    TestParses Filter
      "PASS"
      (just [])
    TestParses Filter
      "q10;s50"
      (just ["q10", "s50"])
    TestFails Filter
      "q10; s50"
    TestParses Filter
      "."
      nothing

    TestParses Info
      "GENE=TP53;HGVSC=NM_000546.6:c.817C>T;HGVSP=NP_000537.3:p.Arg273Cys;SOMATIC"
      (just
        [
          {key = "GENE", vals = ["TP53"]},
          {key = "HGVSC", vals = ["NM_000546.6:c.817C>T"]},
          {key = "HGVSP", vals = ["NP_000537.3:p.Arg273Cys"]},
          {key = "SOMATIC", vals = []}
        ])
    TestParses Info
      "GENES=TP53,TP54,TP55"
      (just
        [ {key = "GENES", vals = ["TP53", "TP54", "TP55"]}
        ])
    TestParses Info
      "SOMATIC"
      (just
        [ {key = "SOMATIC", vals = []}
        ])
    TestParses Info
      "SOMATIC"
      (just
        [ {key = "SOMATIC", vals = []}
        ])
    TestParses Info
      "."
      nothing

    TestParses Format
      "GT"
      ["GT"]
    TestParses Format
      "GT:AD:DP:AF"
      ["GT", "AD", "DP", "AF"]
    TestFails Format
      "TG"
    TestFails Format
      "."

    TestParses Sample
      "0/1:318,294:612:0.480"
      [just "0/1", just "318,294", just "612", just "0.480"]
    TestParses Sample
      "0/1:318,294:612:."
      [just "0/1", just "318,294", just "612", nothing]
    -- TODO
    TestFails Sample
      "./.:318,294:612:0.480"
