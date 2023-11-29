{-|
  Name: Phar
  Description: PHP Archive (Phar) File Format, see
  https://www.php.net/manual/en/phar.fileformat.php
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import Daedalus

def Main = Phar

def Phar =
  block
    stub      = Stub
    manifest  = Manifest
    files     = Many manifest.num_files File
    signature = 
      if manifest.flags.has_signature
        then just Signature
        else nothing

-- https://www.php.net/manual/en/phar.fileformat.stub.php
def Stub =
  block
    $$ = UpTo UInt8 (Match "__HALT_COMPILER();")
    Optional {Match " " <| Match "\n"; Match "?>"}
    Many WS

-- https://www.php.net/manual/en/phar.fileformat.phar.php
def Manifest =
  block
    len       = LEUInt32
    num_files = LEUInt32 as uint 64
    version   = BEUInt16
    flags     = GlobalFlags
    alias_len = LEUInt32 as uint 64
    alias     = Many alias_len UInt8

    serialized_metadata_len = LEUInt32 as uint 64
    serialized_metadata     = Many serialized_metadata_len UInt8

-- https://www.php.net/manual/en/phar.fileformat.flags.php
def GlobalFlags =
  block
    let bitmap = LEUInt32
    -- Phar has a verification signature.
    has_signature = ^ (bitmap .&. 0x00010000) != 0
    -- Phar has at least one file compressed with zlib DEFLATE.
    has_zlib      = ^ (bitmap .&. 0x00001000) != 0
    -- Phar has at least one file compressed with bzip2.
    has_bzip2     = ^ (bitmap .&. 0x00002000) != 0

-- https://www.php.net/manual/en/phar.fileformat.manifestfile.php
def File = 
  block
    manifest = FileManifest
    file_size =
      if manifest.flags.is_compressed_zlib || manifest.flags.is_compressed_bzip2
        then manifest.file_size_compressed
        else manifest.file_size_uncompressed
    contents = Many file_size UInt8

-- https://www.php.net/manual/en/phar.fileformat.manifestfile.php
def FileManifest =
  block
    filename_len = LEUInt32 as uint 64
    filename     = Many filename_len UInt8

    file_size_uncompressed = LEUInt32 as uint 64
    unix_timestamp         = LEUInt32
    file_size_compressed   = LEUInt32 as uint 64
    uncompressed_file_crc  = LEUInt32

    flags = FileFlags

    serialized_metadata_len = LEUInt32 as uint 64
    serialized_metadata     = Many serialized_metadata_len UInt8

-- https://www.php.net/manual/en/phar.fileformat.manifestfile.php
def FileFlags =
  block
    let bitmap = LEUInt32
    permissions         = ^ bitmap .&. 0x000001FF
    is_compressed_zlib  = ^ (bitmap .&. 0x00001000) != 0
    is_compressed_bzip2 = ^ (bitmap .&. 0x00002000) != 0

-- https://www.php.net/manual/en/phar.fileformat.signature.php
def Signature =
  First
    block
      signature = []
      flags = SignatureFlags
      Match "GBMB"
    block
      let b = UInt8
      let s = Signature
      signature = build (emitArray (emit builder b) s.signature)
      flags = s.flags

def SignatureFlags =
  block
    let v = LEUInt32
    is_md5     = ^ v == 0x0001
    is_sha1    = ^ v == 0x0002
    is_sha256  = ^ v == 0x0003
    is_sha512  = ^ v == 0x0004
    is_openssl = ^ v == 0x0010
