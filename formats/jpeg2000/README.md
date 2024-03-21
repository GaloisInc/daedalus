# Introduction

This folder contains a partial specification for the JP2 file format.

The specification covers the complete super structure of JP2 files
(i.e. the boxes and marker segment structures), but only "interprets" and
validates a subset thereof. Unparsed boxes and segments are read and their
data collected as raw bytes.

The main entry point is the file [Jpeg2000.ddl](Jpeg2000.ddl).

# Test Files

There are a number of test files in the [jpylyzer test files repo](https://github.com/openpreserve/jpylyzer-test-files)
that works with this specification. Keeping in mind that our specification isn't complete here are some files
(under the subdirectory files) that should work "as expected":

| File (under subdirectory “files”)                | Description                                                 | Status (Reason)                           |
|--------------------------------------------------|-------------------------------------------------------------|-------------------------------------------|
| reference.jp2                                    |                                                             | Valid                                     |
| openJPEG15.jp2                                   |                                                             | Valid                                     |
| aware.jp2                                        |                                                             | Valid                                     |
|                                                  |                                                             |                                           |
| truncated_at_byte_5000.jp2                       | Data after byte 4999 missing                                | Invalid (Error: unexpected end of stream) |
| oj-ytsiz-not-valid-1.jp2                         | Illegal value of yTsiz                                      | Invalid (value out of range)              |
| missing_null_terminator_in_urlbox.jp2            | Loc (URL) field in URL Box not terminated by null character | Invalid (Malformed string)                |
| bitwiser-headerbox-corrupted-boxlength-22181.jp2 | Bit-corrupted Box Length field in JP2 Header Box            | Invalid (Unexpected end of input)         |

# Notes

By default, the raw data from JPEG 2000 tiles are stored and exposed
as arrays of bytes, which is likely what is needed for a practical parser generated
from this specification. However, if we are using the daedalus interpreter to
work with JP2 files, this may cause unnecessary clutter. Turning the implicit
parameter `?collectRawTileData` below to `false` will avoid exposing this data
and produce more readable output on the terminal.
