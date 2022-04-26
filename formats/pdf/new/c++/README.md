# C++ PDF driver

To build:

```
cmake -B build
cmake --build build
```

The first command creates the out-of-source build directory.
The second command actually builds the project.

On macOS I need to specify where my HomeBrew-managed, OpenSSL root directory is when setting up the build directory

```
OPENSSL_ROOT_DIR=/usr/local/opt/openssl cmake -B build
```
