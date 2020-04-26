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

    target_dir_name = os.path.split(dirname)[1]
    print(f"\nThe target directory is {target_dir_name}.", flush=True)
    print(datetime.datetime.now(), flush=True)
    file_names = sorted(filter(lambda x: "vf.csv" not in x, map(str, Path(dirname).glob("*.csv"))))
    parent_dir = pathlib.Path(file_names[0]).parent

    #png_save_dir = f"{parent_dir}_png"
    #os.makedirs(png_save_dir, exist_ok=True)

    x_min = -2.2
    x_max = 2.2
    y_min = -0.5
    y_max = 1.5

    png_number = 0

    for i, fname in enumerate(file_names):
        if i % 100 == 0:
            print(f"\nNow processing {fname}", flush=True)
            print(datetime.datetime.now(), flush=True)

        sol_csv_path = pathlib.Path(fname)
        vf_csv_path = pathlib.Path(f"{parent_dir}/{sol_csv_path.stem}.vf.csv")

        df_sol = pd.read_csv(fname)
        df_vf = pd.read_csv(vf_csv_path)
        scaled_norm = np.log(np.sqrt((df_vf["u"]) ** 2 + (df_vf["v"]) ** 2) + 1)
        x0 = df_sol["x"][0]
        t = np.arange(0.0, len(df_sol["x"]) * 0.01, 0.01)

        for i_data, val in enumerate(df_sol["x"]):
            if i_data % 10 == 0:
                print(f"i_data: {i_data} / {len(df_sol['x'])}", flush=True)

            i_min = 0 if i_data < 20 else i_data - 20

            fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(10,4))
            ax1.plot(df_sol["x"], df_sol["y"], color="black", linewidth=0.5)
            ax1.plot(df_sol["x"][i_min: i_data], df_sol["y"][i_min: i_data], color="r")
            ax1.plot(df_sol["x"][i_data], df_sol["y"][i_data], 'o--', color="r") # u の marker

            ax1.set_xlabel("u: membrane potential / Volt")
            ax1.set_ylabel("v: recovery variable")
            ax1.set_xlim([x_min, x_max])
            ax1.set_ylim([y_min, y_max])
            ax1.set_title("Phase Space: FitzHugh-Nagumo")
            ax1.grid()

            u_normalize = df_vf["u"] / np.sqrt(df_vf["u"]**2 + df_vf["v"]**2)
            v_normalize = df_vf["v"] / np.sqrt(df_vf["u"]**2 + df_vf["v"]**2)
            im = ax1.quiver(df_vf["x"], df_vf["y"], u_normalize, v_normalize,
                           scaled_norm,
                           cmap='jet', # cf. https://beiznotes.org/matplot-cmap-list/
                           angles='xy')
            fig.colorbar(im)

            #Membrane Potential
            ax2.plot(t, df_sol["x"], label="u: membrane potential", color="#ff7f0e")
            ax2.plot(t, df_sol["y"], label="v: recovery variable", color="#1f77b4")
            ax2.plot(t[i_data], df_sol["x"][i_data],'o--', color="#ff7f0e") # u の marker
            ax2.plot(t[i_data], df_sol["y"][i_data],'o--', color="#1f77b4") # v の marker
            ax2.set_title("Membrane Potential / Volt")
            ax2.set_ylim([-2.2,2.0])
            ax2.grid()
            ax2.legend(bbox_to_anchor=(0, 1),
                       loc='upper left',
                       borderaxespad=0)

            png_number_str = str(png_number).zfill(8)
            plt.savefig(f"{parent_dir}/img.{png_number_str}.png")
            plt.close()

            png_number = png_number + 1

    cmd = f"ffmpeg -r 30 -i {parent_dir}/img.%08d.png workspace/{target_dir_name}.mp4"
    print(f"\nRunning ffmpeg: {cmd}", flush=True)
    print(datetime.datetime.now(), flush=True)
    run(cmd.split(" "))

    print(f"\nCheck the movie file workspace/{target_dir_name}.mp4.", flush=True)
    print("表示しているベクトル場は各点で正規化していて長さに意味はありません.", flush=True)
    print(datetime.datetime.now(), flush=True)

def run(cmds):
    return subprocess.run(cmds, stdout=subprocess.PIPE).stdout.decode("utf-8")

if __name__ == '__main__':
    main()
