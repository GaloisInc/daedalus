# Use Ubuntu as the base image
FROM ubuntu:22.04

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install essential build tools, C++ development tools, and valgrind
RUN apt-get update && apt-get install -y \
    build-essential \
    g++ \
    gcc \
    make \
    cmake \
    valgrind \
    curl \
    git \
    libgmp-dev \
    libffi-dev \
    libncurses-dev \
    libtinfo-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Create a non-root user for development
RUN useradd -m -s /bin/bash developer
USER developer
WORKDIR /home/developer

# Install ghcup and Haskell toolchain
ENV GHCUP_INSTALL_BASE_PREFIX=/home/developer
ENV PATH="/home/developer/.ghcup/bin:${PATH}"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    sh

# Install GHC and cabal
RUN ghcup install ghc recommended && \
    ghcup install cabal recommended && \
    ghcup set ghc recommended

# Configure cabal
RUN cabal update

# Set working directory for projects
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]
