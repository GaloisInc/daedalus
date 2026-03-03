# Daedalus Docker Development Environment

This Docker container provides a minimal Linux environment with Haskell and C++ development tools.

## Included Tools

- **C++ Development**: gcc, g++, make, cmake
- **Haskell Development**: GHC, cabal (installed via ghcup)
- **Memory Analysis**: valgrind
- **Build essentials**: build-essential, git, curl
- **Haskell dependencies**: libgmp, libffi, libncurses, zlib

## Quick Start (Recommended)

Use the provided shell script (automatically mounts project directory):

```bash
./docker/run.sh
```

This will build the image if needed and start a shell with the project directory mounted at `/workspace`.

## Building the Image Manually

```bash
cd docker
docker build -t daedalus-dev .
```

## Running the Container Manually

### Interactive shell with project mounted:
```bash
docker run -it --rm -v $(pwd)/..:/workspace daedalus-dev
```
