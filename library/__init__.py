#!/usr/bin/env python3

import subprocess
import os

this_dir = os.path.dirname(os.path.abspath(__file__))


def install_bdwgc(third_party_dir):
    install_dir = os.path.join(third_party_dir, "bdwgc")

    bdwgc_dir = os.path.join(this_dir, "bdwgc")
    bdwgc_build_dir = os.path.join(this_dir, "bdwgc_build")

    def build_bdwgc():
        subprocess.run(["cmake", "-DCMAKE_BUILD_TYPE=Release", "-DCMAKE_INSTALL_PREFIX=%s" %
                       install_dir, "-S", bdwgc_dir, "-B", bdwgc_build_dir])
        subprocess.run(["cmake", "--build", bdwgc_build_dir, "-j"])

    def install_bdwgc():
        build_bdwgc()
        subprocess.run(["cmake", "--install", bdwgc_build_dir])

    install_bdwgc()

    return install_dir


def create_link_to_bdwgc(bdwgc_install_dir, dst):
    bdwgc_lib_path = os.path.join(bdwgc_install_dir, "lib", "libgc.so")

    # Create link to libgc.so
    os.symlink(bdwgc_lib_path, dst)


# Install libraries
def install(kaede_dir):
    print("Installing libraries...")

    third_party_dir = os.path.join(kaede_dir, "third_party")
    if not os.path.exists(third_party_dir):
        os.mkdir(third_party_dir)

    bdwgc_install_dir = install_bdwgc(third_party_dir)

    kaede_lib_dir = os.path.join(kaede_dir, "lib")
    if not os.path.exists(kaede_lib_dir):
        os.mkdir(kaede_lib_dir)

    kaede_libgc_path = os.path.join(kaede_lib_dir, "libkgc.so")
    create_link_to_bdwgc(bdwgc_install_dir, kaede_libgc_path)

    print("Done!")
