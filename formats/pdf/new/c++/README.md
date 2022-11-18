# C++ PDF driver

Dependencies:

* `cmake`
* `doxygen`

To build:

```
cmake -B build
cmake --build build
```

The first command creates the out-of-source build directory.
The second command actually builds the project.

On MacOS, you'll need to specify where the HomeBrew-managed OpenSSL root
directory is when setting up the build, e.g.,

```
OPENSSL_ROOT_DIR=/usr/local/opt/openssl cmake -B build
```
