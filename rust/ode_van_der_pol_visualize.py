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

    if dirname == "workspace":
        # 最新のディレクトリを取得
        p_temp = Path(dirname)
        dirname = sorted([str(d) for d in list(p_temp.iterdir()) if d.is_dir()])[-1]

    target_dir_name = dirname.split("/")[-1]
    print(f"\nThe target directory is {target_dir_name}.", flush=True)
    print(datetime.datetime.now(), flush=True)
    file_names = sorted(filter(lambda x: "vf.csv" not in x, map(str, Path(dirname).glob("*.csv"))))
    parent_dir = pathlib.Path(file_names[0]).parent

    #png_save_dir = f"{parent_dir}_png"
    #os.makedirs(png_save_dir, exist_ok=True)

    x_min = -10
    x_max = 10
    y_min = -10
    y_max = 10

    for i, fname in enumerate(file_names):
        if i % 100 == 0:
            print(f"\nNow processing {fname}", flush=True)
            print(datetime.datetime.now(), flush=True)

        sol_csv_path = pathlib.Path(fname)
        vf_csv_path = pathlib.Path(f"{parent_dir}/{sol_csv_path.stem}.vf.csv")

        df_sol = pd.read_csv(fname)
        df_vf = pd.read_csv(vf_csv_path)
        scaled_norm = np.log(np.sqrt((df_vf["u"]) ** 2 + (df_vf["v"]) ** 2) + 1)
        mu = df_sol["mu"][0]
        x0 = df_sol["x"][0]

        fig = plt.figure()
        ax = fig.add_subplot(111)
        #ax.grid()                  # 罫線
        ax.set_aspect('equal')      # スケールを揃える
        ax.set_xlabel('x')          # x 軸ラベル
        ax.set_ylabel('y')          # y 軸ラベル
        ax.set_title(f'$\mu$ = {mu}, init coord = {x0}')   # グラフタイトル
        ax.set_xlim([x_min, x_max]) # x 方向の描画範囲を指定
        ax.set_ylim([y_min, y_max]) # y 方向の描画範囲を指定
        #ax.legend(loc=0)           # 凡例
        fig.tight_layout()          # レイアウトの設定
        ax.plot(df_sol["x"], df_sol["y"], color="black", linewidth=1.0)

        u_normalize = df_vf["u"] / np.sqrt(df_vf["u"]**2 + df_vf["v"]**2)
        v_normalize = df_vf["v"] / np.sqrt(df_vf["u"]**2 + df_vf["v"]**2)
        im = ax.quiver(df_vf["x"], df_vf["y"], u_normalize, v_normalize,
                  scaled_norm,
                  cmap='jet', # cf. https://beiznotes.org/matplot-cmap-list/
                  angles='xy')
        fig.colorbar(im)
        plt.savefig(f"{parent_dir}/img.{sol_csv_path.stem}.png")
        plt.close()

    cmd = f"ffmpeg -r 30 -i {parent_dir}/img.%08d.png workspace/{target_dir_name}.mp4"
    print(f"\nRunning ffmpeg: {cmd}", flush=True)
    print(datetime.datetime.now(), flush=True)
    run(cmd.split(" "))

    print(f"\nCheck the movie file workspace/{target_dir_name}.mp4.", flush=True)
    print("表示しているベクトル場は各点で正規化していて長さに意味はありません.")
    print(datetime.datetime.now(), flush=True)

def run(cmds):
    return subprocess.run(cmds, stdout=subprocess.PIPE).stdout.decode("utf-8")

if __name__ == '__main__':
    main()
