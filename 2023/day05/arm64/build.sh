#!/bin/bash
set -e

SDK_PATH=$(xcrun -sdk macosx --show-sdk-path)

# Assemble
as -arch arm64 -o solution.o solution.s

# Link
ld -o solution solution.o -lSystem -syslibroot "$SDK_PATH" -e _main -arch arm64

echo "Build complete: ./solution"
