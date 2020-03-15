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
#tstop = 0.5 # 計算停止時刻
c = 2 * np.pi * (i_max + 3) / i_max

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

def exact(case, x, ue, t):
    if case == 1:
        for i in range(0, i_max + 1):
            fi = ue[i] - 0.5 * (1.1 + np.sin(c * (x[i] - ue[i] * t)))
            fi = ue[i] - 0.5 * (1.1 + np.sin(c * (x[i] - ue[i] * t)))
            dfi = 1.0 + 0.5 * c * np.cos(c * (x[i] - ue[i] * t)) * t
            loop_num = 0
            while(abs(fi) >= 1.0e-4):
                ue[i] = ue[i] - fi / dfi # ニュートン法の計算式
                fi = ue[i] - 0.5 * (1.1 + np.sin(c * x[i] - ue[i] * t))
                dfi = 1.0 + 0.5 * c * np.cos(c * (x[i] - ue[i] * t)) * t
                print(f"loop_num: {loop_num}, ue[{i}] = {ue[i]}, f = {fi}, df = {dfi}")
                loop_num = loop_num + 1
    return ue

def main():
    n = 0
    t = 0.0

    """滑らかな場合"""
    # initc
    dx = (XR - XL) / i_max # 格子間隔, 0.02
    dt = 0.2 * dx          # 時間刻み
    x = np.linspace(XL, XR + dx, int((XR - XL) / dx) + 2)
    u = 0.5 * (1.1 + np.sin(c * (x - a * t)))
    #ue = exact(1, x, np.zeros(len(x)), t)

    fig = plt.figure()
    img1 = plt.plot(x[:-1], u[:-1],  label="Num sol",   color="blue")
    #img2 = plt.plot(x[:-1], ue[:-1], label="Exact sol", color="red")
    plt.legend()
    imgs = [img1]
    #imgs = [img1 + img2]

    while(t <= tstop):
        print(f"SMOOTH CASE: t = {t}")
        n = n + 1
        t = t + dt

        # reconstruction_pc
        ul[2: -1] = u[1: -3]
        ur[2: -1] = u[1: i_max - 1]

        # riemann_roe
        f[2: -1] = 0.5 * (0.5 * ul[2: -1] * ul[2: -1] + 0.5 * ur[2: -1] * ur[2: -1]) \
            - 0.5 * abs(0.5 * ur[2: -1] * ul[2: -1]) * (ur[2: -1] - ul[2: -1])

        # update
        u[2: -2] = u[2: -2] - dt / dx * (f[3:] - f[2: -1])

        # bc
        u[0] = u[i_max - 3]
        u[1] = u[i_max - 2]
        u[i_max - 1] = u[2]
        u[i_max] = u[3]

        # exact
        # TODO
        #ue = exact(1, x, ue, t)

        img1 = plt.plot(x[:-1], u[:-1],  label="Num sol",   color="blue")
        #img2 = plt.plot(x[:-1], ue[:-1], label="Exact sol", color="red")
        #imgs.append(img1 + img2)
        imgs.append(img1)

    fname = "program4-2_smooth.tmp.mp4"
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
    img1 = plt.plot(x[:-1], u[:-1],  label="Num sol",   color="blue")
    plt.legend()
    imgs = [img1]

    while(t <= tstop):
        print(f"SQUARE CASE: t = {t}")
        n = n + 1
        t = t + dt

        # reconstruction_pc
        ul[2: -1] = u[1: -3]
        ur[2: -1] = u[1: i_max - 1]

        # riemann_roe
        f[2: -1] = 0.5 * (0.5 * ul[2: -1] * ul[2: -1] + 0.5 * ur[2: -1] * ur[2: -1]) \
            - 0.5 * abs(0.5 * ur[2: -1] * ul[2: -1]) * (ur[2: -1] - ul[2: -1])

        # update
        u[2: -2] = u[2: -2] - dt / dx * (f[3:] - f[2: -1])

        # bc
        u[0] = u[i_max - 3]
        u[1] = u[i_max - 2]
        u[i_max - 1] = u[2]
        u[i_max] = u[3]

        # exact
        # TODO
        # 省略

        img1 = plt.plot(x[:-1], u[:-1],  label="Num sol",   color="blue")
        #img2 = plt.plot(x[:-1], ue[:-1], label="Exact sol", color="red")
        #imgs.append(img1 + img2)
        imgs.append(img1)

    fname = "program4-2_square.tmp.mp4"
    print(f"NOW save {fname}: START")
    if os.path.exists(fname):
        os.remove(fname)
    anim = animation.ArtistAnimation(fig, imgs, interval=100)
    anim.save(fname, 'ffmpeg')
    print(f"NOW save {fname}: END")

if __name__ == '__main__':
    main()
