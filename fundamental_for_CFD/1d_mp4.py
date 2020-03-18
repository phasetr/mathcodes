#-*- coding=utf-8 -*-
from datetime import datetime
from glob import glob
from os.path import join, relpath
from pathlib import Path
import argparse
import codecs
import csv
import json
import math
import matplotlib.animation as animation
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import pathlib
import pprint
import re
import shutil
import subprocess
import sys
import traceback
import yaml
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
    file_names = sorted(map(str, Path(dirname).glob("*")))

    fig = plt.figure()
    imgs = []
    for fname in file_names:
        df = pd.read_csv(fname)
        xs = df["xs"]
        us = df["us"]
        ues = df["ues"]
        img1 = plt.plot(xs, us, "blue")
        img2 = plt.plot(xs, ues, "red")
        imgs.append(img1 + img2)

    ani = animation.ArtistAnimation(fig, imgs, interval=100)
    ani.save(f"{target_dir_name}.tmp.mp4", writer="ffmpeg")
    print(f"{target_dir_name}.tmp.mp4 を確認してください.")

if __name__ == '__main__':
    main()
