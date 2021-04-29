import JpegBasics

def Main = block
  SOI
  header   = Exif_APP1
  @Many Segment
  EOI

