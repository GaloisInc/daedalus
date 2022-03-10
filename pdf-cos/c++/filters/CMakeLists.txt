add_library(pdffilters STATIC
  src/asciihex.cpp
  src/ascii85.cpp
  src/bitstream.cpp
  src/lzw.cpp
)

target_include_directories(pdffilters PUBLIC include)
