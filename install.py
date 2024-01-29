#!/usr/bin/env python3

import os
import library
import subprocess
import shutil


kaede_dir = os.path.expanduser("~/.kaede")
kaede_bin_dir = os.path.join(kaede_dir, "bin")


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

    library.install(kaede_dir)

    os.environ["KAEDE_DIR"] = kaede_dir
    install_kaede()

    # Create shell script for setting environment variables
    env_script_path = "$HOME/.kaede/env"
    with open(env_script_path, "x") as f:
        f.write("export PATH=$PATH:%s" % kaede_bin_dir + "\n")
    with open(os.path.expanduser("~/.profile"), "a+") as f:
        f.write('. "%s"' % env_script_path + "\n")

    print("Enter the following commands:")
    print("source ~/.profile")
