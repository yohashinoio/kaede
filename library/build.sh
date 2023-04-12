#!/bin/bash

BOEHM_GC_REPO_DIR=bdwgc
BOEHM_GC_BUILD_DIR=bdwgc_build

# Build boehm GC
cmake -DCMAKE_BUILD_TYPE=Release -S $BOEHM_GC_REPO_DIR -B $BOEHM_GC_BUILD_DIR
cmake --build $BOEHM_GC_BUILD_DIR -j

# Install boehm GC
cmake --install $BOEHM_GC_BUILD_DIR
