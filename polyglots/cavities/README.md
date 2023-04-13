# Polyglot Cavities

This directory contains minimal data formats exhibiting different kinds of cavities.
File names reflect the cavity location and properties.

Cavity location:
* [P] Prefix
* [S] Suffix
* [I] Interior
* [U] Unrestricted (can appear anywhere)

Cavity constraints:
* [R/U] Restricted/unrestricted byte sequences
* [N/X/F/U] Minimum/maximum/fixed length/unrestricted

Each Daedalus spec (.ddl) is accompanied by an example data message (.txt).

## Cavity Formats

| Filename  | Description | Represents Formats |
|-----------|-------------|----------|
| cavity-PRU-1 | Bytes up to "SUFFIX" are ignored. | PDF |
| cavity-PUF-1 | The first 255 bytes are ignored. | DICOM |
| cavity-SUF-1 | The "LENGTH N" header specifies bytes to ignore at end of file. | |
| cavity-SUU-1 | Bytes after "END" are ignored. | TAR |
| cavity-SUU-2 | The "LENGTH N" header specifies file size, remaining bytes are ignored. | RIFF, Java Class |
| cavity-IUU-1 | Each block contains an interior cavity with a length specified by a header, with an optional pointer to the next block.  The space between blocks also forms cavities. | TIFF, BMP |
| cavity-IUU-2 | Each block contains an interior cavity with a length specified by a header. | AR |
| cavity-IUU-3 | Table of contents points to blocks.  Space between blocks forms cavities. | WAD, ICO |

## Polyglots

|       | PRU-1 | PUF-1 | SUF-1 | SUU-1 | IUU-1 | IUU-2 | IUU-3 |
|-------|-------|-------|-------|-------|-------|-------|-------|
| PRU-1 | .     |       | X     | X     |       |       |       |
| PUF-1 |       | .     | X     | X     |       |       |       |
| SUF-1 |       |       | .     |       |       |       |       |
| SUU-1 |       |       |       | .     |       |       |       |
| IUU-1 |       |       |       |       | .     |       |       |
| IUU-2 |       |       |       |       |       | .     |       |
| IUU-3 |       |       |       |       |       |       | .     |

