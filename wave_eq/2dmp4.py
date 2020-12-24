#-*- coding=utf-8 -*-
from pathlib import Path
import argparse
import numpy as np
import pandas as pd
import pathlib
import pprint
import subprocess
import sys
pp = pprint.pprint

desc = "ディレクトリツリーを記録したファイルを作る"
parser = argparse.ArgumentParser(description=desc)
parser.add_argument(
    "-d",
    "--dirname",
    default="workspace",
    help="ディレクトリを指定する.",
    required=False
)

def main(argv = sys.argv):
    args = parser.parse_args()
    dirname = args.dirname

    if dirname == "workspace":
        # 最新のディレクトリを取得
        p_temp = Path(dirname)
        dirname = sorted([str(d) for d in list(p_temp.iterdir()) if d.is_dir()])[-1]

    target_dir_name = dirname.split("/")[-1]
    print(f"対象ディレクトリは {target_dir_name} です.")
    file_names = sorted(map(str, Path(dirname).glob("*.csv")))
    parent_dir = pathlib.Path(file_names[0]).parent

    cmds = []

    for fname in file_names:
        df = pd.read_csv(fname)
        umax = df["u"].max()
        print(f"u max: {umax}")
        p = pathlib.Path(fname)
        cmd = f"""set terminal png
set datafile separator ","
set output "{parent_dir}/img.{p.stem}.png"
set zrange [-1:1]
set ticslevel 0
set dgrid3d 100,100
splot "{fname}" u 1:2:3 with lines

"""
        cmds.append(cmd)

    with open("2dpngs.tmp.gp", "w", encoding="utf-8") as f:
        f.writelines(cmds)

    run(["gnuplot", "2dpngs.tmp.gp"])
    run(["ffmpeg", "-r", "10", "-i", f"{parent_dir}/img.%08d.png", f"{target_dir_name}.mp4"])
    print(f"動画ファイル {target_dir_name}.mp4 を確認してください.")

def run(cmds):
    return subprocess.run(cmds, stdout=subprocess.PIPE).stdout.decode("utf-8")

if __name__ == '__main__':
    main()
