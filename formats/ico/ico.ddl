import Daedalus


-- ICO container format used by MS windows to store image files.

-- References:
-- https://formats.kaitai.io/ico/
-- https://en.wikipedia.org/wiki/ICO_(file_format)
-- https://learn.microsoft.com/en-us/previous-versions/ms997538(v=msdn.10)

def Main =
  block
    Match [0x00, 0x00, 0x01, 0x00] -- fixed
    num_images = LEUInt16 as uint 64
    array_of_images = Many num_images ImageHeader

def ImageHeader =
  block
    width = UInt8
    height = UInt8
    num_colors = UInt8
    Match [0x00]
    num_planes = LEUInt16
    bit_count = LEUInt16
    len_img = LEUInt32
    absolute_offset = LEUInt32
