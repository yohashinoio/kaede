#!/usr/bin/env python3

import os
import library
import subprocess
import shutil


unexpanded_kaede_dir = "$HOME/.kaede"
unexpanded_kaede_bin_dir = "$HOME/.kaede/bin"
kaede_dir = os.path.expandvars(unexpanded_kaede_dir)
kaede_bin_dir = os.path.expandvars(unexpanded_kaede_bin_dir)


def install_compiler():
    print("Installing compiler...")

    os.environ["KAEDE_DIR"] = kaede_dir
    subprocess.run(["cargo", "build", "--release"])
    # Move builded binary
    shutil.move("target/release/kaede", os.path.join(kaede_dir, "bin"))

    print("Done!")


def install():
    library.install(kaede_dir)
    install_compiler()


def create_shell_script_for_setting_env():
    unexpanded_env_script_path = os.path.join(unexpanded_kaede_dir, "env")
    with open(os.path.expandvars(unexpanded_env_script_path), "w+") as f:
        f.writelines(["#!/bin/sh\n", "\n", 'export PATH="%s:$PATH"\n' %
                     unexpanded_kaede_bin_dir])
    with open(os.path.expanduser("~/.profile"), "a+") as f:
        f.write('. "%s"' % unexpanded_env_script_path + "\n")


if __name__ == '__main__':
    if os.path.exists(kaede_dir):
        print(
            "If you need to reinstall, delete '%s' and run this program again!" % kaede_dir)
        exit(1)

    if not os.path.exists(kaede_dir):
        os.mkdir(kaede_dir)
    if not os.path.exists(kaede_bin_dir):
        os.mkdir(kaede_bin_dir)

    install()

    create_shell_script_for_setting_env()

    print("Please enter the following commands:")
    print("source ~/.profile")
