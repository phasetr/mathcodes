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

partition = 400
node = partition + 1
LL = 1.0
dx = LL / (node - 1)
dt = 0.001
NT = 7000
eps = 2.0**(-50)
V = 0.1
bc = 1 # bc=1 left Dirichlet (V>=0), bc=2 right Dirichlet (V<0)
D0 = 0.0 # for Dirichlet

def main():
    # initialize
    xs = np.linspace(0, LL, node)
    u = np.zeros(node)
    u[40: 80] = 1

    fig = plt.figure()
    imgs = []

    for i in range(NT):
        u_new = np.zeros(node)
        u_new[1: -2] = u[1: -2] - dt / dx * (-(V + abs(V))) * 0.5 * u[0: -3] \
            + abs(V) * u[1: -2] + (V - abs(V)) * 0.5 * u[2: -1]

        # bc
        if bc == 1:
            u_new[-1] = u[-1] - dt / dx * V * (u[-1] - u[-2])
        else:
            u_new[0] = u_new[0] - dt / dx * V (u[1] - u[0])
            u[-1] = D0

        u = u_new

        if i % 50 == 0:
            pp(u_new)
            pp(u)
            print(f"Now Iteration: {i} / {NT}")
            img = plt.plot(xs, u_new, color="blue")
            imgs.append(img)

    fname = "convection.tmp.mp4"
    if os.path.exists(fname):
        os.remove(fname)
    anim = animation.ArtistAnimation(fig, imgs, interval=50)
    anim.save(fname, 'ffmpeg')

if __name__ == '__main__':
    main()
