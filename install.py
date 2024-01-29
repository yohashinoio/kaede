#!/usr/bin/env python3

import os
import library
import subprocess
import shutil


unexpanded_kaede_dir = "$HOME/.kaede"
kaede_dir = os.path.expandvars(unexpanded_kaede_dir)
unexpanded_kaede_bin_dir = "$HOME/.kaede/bin"
kaede_bin_dir = os.path.expandvars(unexpanded_kaede_bin_dir)


def install_kaede():
    print("Installing kaede...")

    subprocess.run(["cargo", "build", "--release"])
    # Move builded binary
    shutil.move("target/release/kaede", os.path.join(kaede_dir, "bin"))

    print("Done!")


if __name__ == '__main__':
    if os.path.exists(kaede_dir):
        print(
            "Already installed, if you need to reinstall, delete '%s' and run this program again!" % kaede_dir)
        exit(1)

    if not os.path.exists(kaede_dir):
        os.mkdir(kaede_dir)
    if not os.path.exists(kaede_bin_dir):
        os.mkdir(kaede_bin_dir)

    # Install
    library.install(kaede_dir)
    os.environ["KAEDE_DIR"] = kaede_dir
    install_kaede()

    # Create shell script for setting environment variables
    unexpanded_env_script_path = os.path.join(unexpanded_kaede_dir, "env")
    with open(os.path.expandvars(unexpanded_env_script_path), "w+") as f:
        f.writelines(["#!/bin/sh\n", "\n", 'export PATH="%s:$PATH"\n' %
                     unexpanded_kaede_bin_dir])
    with open(os.path.expanduser("~/.profile"), "a+") as f:
        f.write('. "%s"' % unexpanded_env_script_path + "\n")

    print("Enter the following commands:")
    print("source ~/.profile")
