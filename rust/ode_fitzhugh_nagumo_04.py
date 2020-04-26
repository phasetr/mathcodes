# coding: utf-8
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import pandas as pd
import pprint
import scipy.integrate as integrate
pp = pprint.pprint

I = 0.34 #external stimulus
a = 0.7
b = 0.8
c = 10

def FHN(state, t, I):
    """
    FitzHugh-Nagumo Equations
    u : the membrane potential
    v : a recovery variable
    """
    u, v = state
    dot_u = c * (-v + u - pow(u,3)/3 + I)
    dot_v = u - b * v + a
    return dot_u, dot_v

#initial state
u0 = 2.0
v0 = 1.0
y0 = [u0, v0]

t = np.arange(0.0, 5, 0.01)

#全軌道の表示用
t0 = np.arange(0.0, 20, 0.01)
y_all = integrate.odeint(FHN, [u0, v0], t0, args=(I,))
u_all = y_all[:,0]
v_all = y_all[:,1]

np.savetxt("workspace/ode_fitzhugh_nagumo_04_I_033.tmp.csv",
           y_all,
           delimiter=',')

I = 0.33
y_all = integrate.odeint(FHN, [u0, v0], t0, args=(I,))
u_all = y_all[:,0]
v_all = y_all[:,1]
np.savetxt("workspace/ode_fitzhugh_nagumo_04_I_034.tmp.csv",
           y_all,
           delimiter=',')
