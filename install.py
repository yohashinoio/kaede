#!/usr/bin/env python3

import os
import library
import subprocess
import shutil


kaede_dir = os.path.expanduser("~/.kaede")
kaede_bin_dir = os.path.join(kaede_dir, "bin")


def shell_config_path():
    shell = os.environ.get("SHELL")

    if shell is None:
        return None
    elif "zsh" in shell:
        return "~/.zprofile"
    elif "fish" in shell:
        return "~/.config/fish/config.fish"
    elif "bash" in shell:
        return "~/.bash_profile"

    return None


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

    # Set environment variable
    profile_path = shell_config_path()
    exoprt_kaede_bin_command = "export PATH=$PATH:%s" % kaede_bin_dir
    if profile_path is None:
        print("Write the following commands in your shell configuration file:")
        print(exoprt_kaede_bin_command)
    else:
        with open(os.path.expanduser(profile_path), "a") as f:
            f.write(exoprt_kaede_bin_command + "\n")
        print("Finally, enter the following command to finish the installation:")
        print("source %s" % profile_path)
