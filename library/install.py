#!/usr/bin/env python3
# Some systems don't have `python3` in their PATH. This isn't supported by x.py directly

import subprocess
import os

library_dir = os.path.dirname(os.path.abspath(__file__))

def install_gc():
    BOEHM_GC_REPO_DIR = os.path.join(library_dir, "bdwgc")
    BOEHM_GC_BUILD_DIR = os.path.join(library_dir, "bdwgc_build")

    def build_bdwgc():
        subprocess.run(["cmake", "-DCMAKE_BUILD_TYPE=Release",  "-S",  BOEHM_GC_REPO_DIR, "-B", BOEHM_GC_BUILD_DIR])
        subprocess.run(["cmake", "--build", BOEHM_GC_BUILD_DIR, "-j"])

    build_bdwgc()

    # Install boehm GC
    subprocess.run(["cmake", "--install", BOEHM_GC_BUILD_DIR])

# Install libraries
def install():
    install_gc()

if __name__ == '__main__':
    install()
