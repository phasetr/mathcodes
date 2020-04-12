#-*- coding=utf-8 -*-
from pathlib import Path
import argparse
import datetime
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

    print(datetime.datetime.now())

    if dirname == "workspace":
        # 最新のディレクトリを取得
        p_temp = Path(dirname)
        dirname = sorted([str(d) for d in list(p_temp.iterdir()) if d.is_dir()])[-1]

    target_dir_name = dirname.split("/")[-1]
    print(f"対象ディレクトリは {target_dir_name} です.")
    file_names = sorted(map(str, Path(dirname).glob("*.csv")))
    parent_dir = pathlib.Path(file_names[0]).parent

    cmds = []

    print(datetime.datetime.now())

    x_min = -10
    x_max = 10
    y_min = -10
    y_max = 10

    for i, fname in enumerate(file_names):
        if i % 100 == 0:
            print(f"現在 {fname} 処理中")
        p = pathlib.Path(fname)
        cmd = f"""set terminal png
set datafile separator ","
set output "{parent_dir}/img.{p.stem}.png"
set ticslevel 0;
set xrange [{x_min}:{x_max}]
set yrange [{y_min}:{y_max}]
print "Now gnuplot {fname}"
plot "{fname}" u 1:2 pt 7 ps 0.1 lc rgb '#99ccff'

"""
        cmds.append(cmd)

    print(datetime.datetime.now())
    gpname = "ode_dynamical_system001_visualize.tmp.gp"
    with open(gpname, "w", encoding="utf-8") as f:
        f.writelines(cmds)

    print(datetime.datetime.now())
    print(f"{gpname} 処理中: gnuplot {gpname}")
    run(["gnuplot", gpname])
    print(datetime.datetime.now())
    print(f"ffmpeg 処理中: ffmpeg -r 10 -i {parent_dir}/img.%08d.png")
    run(["ffmpeg", "-r", "10", "-i", f"{parent_dir}/img.%08d.png", f"workspace/{target_dir_name}.mp4"])
    print(datetime.datetime.now())
    print(f"動画ファイル workspace/{target_dir_name}.mp4 を確認してください.")

def run(cmds):
    return subprocess.run(cmds, stdout=subprocess.PIPE).stdout.decode("utf-8")

if __name__ == '__main__':
    main()
