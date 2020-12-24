# coding: utf-8
# https://www.hello-python.com/2017/04/30/格子ボルツマン法を用いた流体力学のシミュレー/
import numpy
import matplotlib.pyplot
import matplotlib.animation
import pandas as pd
import pprint
pp = pprint.pprint

height = 80
width = 200
viscosity = 0.02
omega = 1 / (3*viscosity + 0.5)
u0 = 0.10
four9ths = 4.0/9.0
one9th   = 1.0/9.0
one36th  = 1.0/36.0

n0 = four9ths * (numpy.ones((height,width)) - 1.5*u0**2)
nN = one9th * (numpy.ones((height,width)) - 1.5*u0**2)
nS = one9th * (numpy.ones((height,width)) - 1.5*u0**2)
nE = one9th * (numpy.ones((height,width)) + 3*u0 + 4.5*u0**2 - 1.5*u0**2)
nW = one9th * (numpy.ones((height,width)) - 3*u0 + 4.5*u0**2 - 1.5*u0**2)
nNE = one36th * (numpy.ones((height,width)) + 3*u0 + 4.5*u0**2 - 1.5*u0**2)
nSE = one36th * (numpy.ones((height,width)) + 3*u0 + 4.5*u0**2 - 1.5*u0**2)
nNW = one36th * (numpy.ones((height,width)) - 3*u0 + 4.5*u0**2 - 1.5*u0**2)
nSW = one36th * (numpy.ones((height,width)) - 3*u0 + 4.5*u0**2 - 1.5*u0**2)
rho = n0 + nN + nS + nE + nW + nNE + nSE + nNW + nSW
ux = (nE + nNE + nSE - nW - nNW - nSW) / rho
uy = (nN + nNE + nNW - nS - nSE - nSW) / rho

barrier = numpy.zeros((height,width), bool)
#pd.DataFrame(barrier).to_csv("sample1_0_orig.tmp.csv")
barrier[int(height/2)-8: int(height/2)+8, int(height/2)] = True
#pd.DataFrame(barrier).to_csv("sample1_1.tmp.csv")
barrier[int(height/2)-8, int(height/2) + 1] = True
#pd.DataFrame(barrier).to_csv("sample1_2.tmp.csv")
barrierN = numpy.roll(barrier,  1, axis=0)
barrierS = numpy.roll(barrier, -1, axis=0)
barrierE = numpy.roll(barrier,  1, axis=1)
barrierW = numpy.roll(barrier, -1, axis=1)
barrierNE = numpy.roll(barrierN,  1, axis=1)
barrierNW = numpy.roll(barrierN, -1, axis=1)
barrierSE = numpy.roll(barrierS,  1, axis=1)
barrierSW = numpy.roll(barrierS, -1, axis=1)

def stream():
    global nN, nS, nE, nW, nNE, nNW, nSE, nSW
    nN  = numpy.roll(nN,   1, axis=0)
    nNE = numpy.roll(nNE,  1, axis=0)
    nNW = numpy.roll(nNW,  1, axis=0)
    nS  = numpy.roll(nS,  -1, axis=0)
    nSE = numpy.roll(nSE, -1, axis=0)
    nSW = numpy.roll(nSW, -1, axis=0)
    nE  = numpy.roll(nE,   1, axis=1)
    nNE = numpy.roll(nNE,  1, axis=1)
    nSE = numpy.roll(nSE,  1, axis=1)
    nW  = numpy.roll(nW,  -1, axis=1)
    nNW = numpy.roll(nNW, -1, axis=1)
    nSW = numpy.roll(nSW, -1, axis=1)

    nN[barrierN] = nS[barrier]
    nS[barrierS] = nN[barrier]
    nE[barrierE] = nW[barrier]
    nW[barrierW] = nE[barrier]
    nNE[barrierNE] = nSW[barrier]
    nNW[barrierNW] = nSE[barrier]
    nSE[barrierSE] = nNW[barrier]
    nSW[barrierSW] = nNE[barrier]

def collide():
    global rho, ux, uy, n0, nN, nS, nE, nW, nNE, nNW, nSE, nSW
    rho = n0 + nN + nS + nE + nW + nNE + nSE + nNW + nSW
    ux = (nE + nNE + nSE - nW - nNW - nSW) / rho
    uy = (nN + nNE + nNW - nS - nSE - nSW) / rho
    ux2 = ux * ux
    uy2 = uy * uy
    u2 = ux2 + uy2
    omu215 = 1 - 1.5*u2
    uxuy = ux * uy
    n0 = (1-omega)*n0 + omega * four9ths * rho * omu215
    nN = (1-omega)*nN + omega * one9th * rho * (omu215 + 3*uy + 4.5*uy2)
    nS = (1-omega)*nS + omega * one9th * rho * (omu215 - 3*uy + 4.5*uy2)
    nE = (1-omega)*nE + omega * one9th * rho * (omu215 + 3*ux + 4.5*ux2)
    nW = (1-omega)*nW + omega * one9th * rho * (omu215 - 3*ux + 4.5*ux2)
    nNE = (1-omega)*nNE + omega * one36th * rho * (omu215 + 3*(ux+uy) + 4.5*(u2+2*uxuy))
    nNW = (1-omega)*nNW + omega * one36th * rho * (omu215 + 3*(-ux+uy) + 4.5*(u2-2*uxuy))
    nSE = (1-omega)*nSE + omega * one36th * rho * (omu215 + 3*(ux-uy) + 4.5*(u2-2*uxuy))
    nSW = (1-omega)*nSW + omega * one36th * rho * (omu215 + 3*(-ux-uy) + 4.5*(u2+2*uxuy))
    nE[:,0] = one9th * (1 + 3*u0 + 4.5*u0**2 - 1.5*u0**2)
    nW[:,0] = one9th * (1 - 3*u0 + 4.5*u0**2 - 1.5*u0**2)
    nNE[:,0] = one36th * (1 + 3*u0 + 4.5*u0**2 - 1.5*u0**2)
    nSE[:,0] = one36th * (1 + 3*u0 + 4.5*u0**2 - 1.5*u0**2)
    nNW[:,0] = one36th * (1 - 3*u0 + 4.5*u0**2 - 1.5*u0**2)
    nSW[:,0] = one36th * (1 - 3*u0 + 4.5*u0**2 - 1.5*u0**2)

def curl(ux, uy):
    return numpy.roll(uy,-1,axis=1) - numpy.roll(uy,1,axis=1) - numpy.roll(ux,-1,axis=0) + numpy.roll(ux,1,axis=0)

theFig = matplotlib.pyplot.figure(figsize=(8,3))
fluidImage = matplotlib.pyplot.imshow(curl(ux, uy), origin='lower', norm=matplotlib.pyplot.Normalize(-.1,.1),
                                      cmap=matplotlib.pyplot.get_cmap('jet'), interpolation='none')
bImageArray = numpy.zeros((height, width, 4), numpy.uint8)
bImageArray[barrier,3] = 255
barrierImage = matplotlib.pyplot.imshow(bImageArray, origin='lower', interpolation='none')

def nextFrame(arg):
    for step in range(20):
        stream()
        collide()
    fluidImage.set_array(curl(ux, uy))
    return (fluidImage, barrierImage)

animate = matplotlib.animation.FuncAnimation(theFig, nextFrame, interval=0.1, blit=True, frames = 2000)
#matplotlib.pyplot.show()
#animate.save('output.tmp.gif', writer='imagemagick', fps=50);
animate.save('sample1.tmp.mp4', writer='ffmpeg', fps=50);
