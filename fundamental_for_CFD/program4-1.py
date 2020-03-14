#-*- coding=utf-8 -*-
from matplotlib import animation
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.pyplot as plt
import numpy as np
import os
import os.path
import pprint
pp = pprint.pprint

"""
      計算変数と数値流速の配置

     セル境界           セル境界
  -------|-----------------|-------
       f[i]     u[i]    f[i+1]      ==> 計算コードの変数定義

      f_i-1/2   u_i     f_i+1/2     ==> テキストの標記


      境界条件
    u[0]     u[1]                    u[i_max-1]  u[i_max]
 ---------|-------|  ---- ---  ----|-----------|---------
         [1]     [2]           [i_max-2]    [i_max-1]
              計算境界            計算境界
"""

i_max = 100 # 格子セル数
i_max_for_loop = i_max + 1 # 格子セル数
XL = -1.0   # 計算領域左端の座標
XR = 1.0    # 計算領域右端の座標
a = 1.0     # 線形移流方程式の移流速度
tstop = 2.0 # 計算停止時刻

x  = np.zeros(i_max + 1)
u  = np.zeros(i_max + 1)
ue = np.zeros(i_max + 1)
ul = np.zeros(i_max + 1)
ur = np.zeros(i_max + 1)
f  = np.zeros(i_max + 1)

def myprint(arr):
    for i, elem in enumerate(arr):
        print(f"arr[{i}]: {elem}")
    exit(0)

def main():
    n = 0
    t = 0.0

    """滑らかな場合"""
    # initc
    dx = (XR - XL) / i_max # 格子間隔, 0.02
    dt = 0.2 * dx          # 時間刻み
    x = np.linspace(XL, XR + dx, int((XR - XL) / dx) + 2)
    u = 0.5 * (1.1 + np.sin(2 * np.pi * (x - a * t) * (i_max + 3) / (i_max) ))

    fig = plt.figure()
    imgs = []
    flag_legend = True # 凡例描画のフラグ

    while(t <= tstop):
        print(f"SMOOTH CASE: t = {t}")
        n = n + 1
        t = t + dt

        # reconstruction_pc
        ul[2: -1] = u[1: -3]
        ur[2: -1] = u[1: i_max - 1]

        # riemann_roe
        f[2: -1] = 0.5 * (a * ul[2: -1] + a * ur[2: -1]) \
            - 0.5 * abs(a) * (ur[2: -1] - ul[2: -1])

        # update
        u[2: -2] = u[2: -2] - dt / dx * (f[3:] - f[2: -1])

        # bc
        u[0] = u[i_max - 3]
        u[1] = u[i_max - 2]
        u[i_max - 1] = u[2]
        u[i_max] = u[3]

        # exact
        ue = 0.5 * (1.1 + np.sin(2 * np.pi * (x - a * t) * (i_max + 3) / (i_max)))

        img1 = plt.plot(x[:-1], u[:-1],  label="Num sol",   color="blue")
        img2 = plt.plot(x[:-1], ue[:-1], label="Exact sol", color="red")
        if flag_legend: # 一回だけ凡例を描画
            plt.legend()
            flag_legend = False
        imgs.append(img1 + img2)

    fname = "program4-1_smooth.tmp.mp4"
    print(f"NOW save {fname}: START")
    if os.path.exists(fname):
        os.remove(fname)
    anim = animation.ArtistAnimation(fig, imgs, interval=100)
    anim.save(fname, 'ffmpeg')
    print(f"NOW save {fname}: END")

    """矩形波"""
    # 時間を初期化
    t = 0.0

    u = np.full(len(x), 0.1)
    u[40:61] = 1

    fig = plt.figure()
    imgs = []
    flag_legend = True # 凡例描画のフラグ

    while(t <= tstop):
        print(f"SQUARE CASE: t = {t}")
        n = n + 1
        t = t + dt

        # reconstruction_pc
        ul[2: -1] = u[1: -3]
        ur[2: -1] = u[1: i_max - 1]

        # riemann_roe
        f[2: -1] = 0.5 * (a * ul[2: -1] + a * ur[2: -1]) \
            - 0.5 * abs(a) * (ur[2: -1] - ul[2: -1])

        # update
        u[2: -2] = u[2: -2] - dt / dx * (f[3:] - f[2: -1])

        # bc
        u[0] = u[i_max - 3]
        u[1] = u[i_max - 2]
        u[i_max - 1] = u[2]
        u[i_max] = u[3]

        # exact
        ue = np.full(len(x), 0.1)
        xc = a * t
        xl = xc - 10 * dx
        if (xl > 1 - 2 * dx):
            xl = -2 + xl + 2 * dx

        xr = xc + dx * 10
        if (xr > 1 - 2 * dx):
            xr = -2 + xr + 3 * dx

        if xl <= xr:
            for i in range(len(ue)):
                if (xl <= (i - i_max / 2) * dx) and (i - i_max / 2) * dx <= xr:
                    ue[i] = 1
        if xl >= xr:
            for i in range(len(ue)):
                if (xr <= (i - i_max / 2) * dx) and (i - i_max / 2) * dx <= xl:
                    ue[i] = 0.1

        img1 = plt.plot(x[:-1], u[:-1],  label="Num sol",   color="blue")
        img2 = plt.plot(x[:-1], ue[:-1], label="Exact sol", color="red")
        if flag_legend: # 一回だけ凡例を描画
            plt.legend()
            flag_legend = False
        imgs.append(img1 + img2)

    fname = "program4-1_square.tmp.mp4"
    print(f"NOW save {fname}: START")
    if os.path.exists(fname):
        os.remove(fname)
    anim = animation.ArtistAnimation(fig, imgs, interval=100)
    anim.save(fname, 'ffmpeg')
    print(f"NOW save {fname}: END")

if __name__ == '__main__':
    main()
