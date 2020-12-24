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
home = os.path.expanduser('~').replace("\\", "/")
now = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")

desc = "ディレクトリツリーを記録したファイルを作る"
parser = argparse.ArgumentParser(description=desc)
parser.add_argument(
    "-d",
    "--dirname",
    default="~/codes/elkurage",
    help="ディレクトリを指定する.",
    required=False
)

def main(argv = sys.argv):
    b = """set terminal png
set datafile separator ","
set output "sample_00000000.tmp.png"
set ticslevel 0
set dgrid3d 100,100
splot "workspace/20200403-074318-2dim_wave_eq_u_ut/00000000.csv" u 1:2:3 with lines"""
    a = run()

def run(cmds):
    return subprocess.run(cmds, stdout=subprocess.PIPE).stdout.decode("utf-8")


if __name__ == '__main__':
    main()
