find_package(OpenSSL REQUIRED)

add_library(opensslxx STATIC
  src/digest.cpp
  src/cryptoerror.cpp
  src/cipher.cpp
)

target_include_directories(opensslxx PUBLIC include)
target_link_libraries(opensslxx OpenSSL::Crypto)