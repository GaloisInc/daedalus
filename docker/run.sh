#!/bin/bash

# Get the directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Get the project root (parent of docker directory)
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Build the image if it doesn't exist
if [[ "$(docker images -q daedalus-dev 2> /dev/null)" == "" ]]; then
    echo "Building daedalus-dev image..."
    docker build -t daedalus-dev "$SCRIPT_DIR"
fi

# Run the container with the project mounted at /workspace
docker run -it --rm \
    -v "$PROJECT_ROOT:/workspace" \
    -w /workspace \
    daedalus-dev "$@"
