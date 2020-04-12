#-*- coding=utf-8 -*-
from pathlib import Path
import argparse
import datetime
import numpy as np
import matplotlib.pyplot as plt
import os
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
    png_save_dir = f"{parent_dir}_png"

    os.makedirs(png_save_dir, exist_ok=True)

    x_min = -10
    x_max = 10
    y_min = -10
    y_max = 10

    fig = plt.figure()
    plt.xlim(x_min, x_max)
    plt.ylim(y_min, y_max)

    for i, fname in enumerate(file_names):
        if i % 100 == 0:
            print(datetime.datetime.now())
            print(f"現在 {fname} 処理中")
        p = pathlib.Path(fname)
        df = pd.read_csv(fname).query('x != 0.0 and y != 0.0')
        plt.plot(df["x"], df["y"], color="#99ccff", linewidth=0.2)
        plt.savefig(f"{png_save_dir}/img.{p.stem}.png")

    print(datetime.datetime.now())
    print(f"ffmpeg 処理中: ffmpeg -r 10 -i {png_save_dir}/img.%08d.png")
    run(["ffmpeg", "-r", "10", "-i", f"{png_save_dir}/img.%08d.png", f"workspace/{target_dir_name}.mp4"])
    print(datetime.datetime.now())
    print(f"動画ファイル workspace/{target_dir_name}.mp4 を確認してください.")

def run(cmds):
    return subprocess.run(cmds, stdout=subprocess.PIPE).stdout.decode("utf-8")

if __name__ == '__main__':
    main()
