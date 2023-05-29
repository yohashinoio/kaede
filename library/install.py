#!/usr/bin/env python3
# Some systems don't have `python3` in their PATH. This isn't supported by x.py directly

import subprocess
import os

library_dir = os.path.dirname(os.path.abspath(__file__))

def install_gc():
    bdwgc_dir = os.path.join(library_dir, "bdwgc")
    bdwgc_build_dir = os.path.join(library_dir, "bdwgc_build")

    def build_bdwgc():
        subprocess.run(["cmake", "-DCMAKE_BUILD_TYPE=Release",  "-S",  bdwgc_dir, "-B", bdwgc_build_dir])
        subprocess.run(["cmake", "--build", bdwgc_build_dir, "-j"])

    build_bdwgc()

    # Install boehm GC
    subprocess.run(["cmake", "--install", bdwgc_build_dir])

# Install libraries
def install():
    install_gc()

if __name__ == '__main__':
    install()
