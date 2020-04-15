# coding: utf-8
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import scipy.integrate as integrate

I = 0.34 #external stimulus
a = 0.7
b = 0.8
c = 10

def FHN(state, t):
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

fig = plt.figure()
t = np.arange(0.0, 1, 0.01)
t0 = np.arange(0.0, 20, 0.01)

#全ての軌道を表示するため
y_all = integrate.odeint(FHN, [u0, v0], t0)
u_all = y_all[:,0]
v_all = y_all[:,1]

len_t = len(t)
dt = 5 #time steps

#animationの1step
def update(i):
    global y, y0

    #y0の初期値の設定
    if i ==0:
        y0 = [u0, v0]

    plt.cla() #現在描写されているグラフを消去

    #微分方程式を解く
    y = integrate.odeint(FHN, y0, t)

    #1Step(=dt)後のyの値を次のステップでのy0の値に更新する
    y0 = (y[dt,0], y[dt,1])

    #配列としてu,vを取得
    u = y[:,0]
    v = y[:,1]

    #描画
    plt.plot(u_all, v_all, color="k", dashes=[1, 5])
    plt.plot(u, v,color="r")
    plt.plot(u[len_t-1], v[len_t-1], 'o--', color="r") # u の marker
    plt.xlabel("u : membrane potential / Volt")
    plt.ylabel("v : recovery variable")
    plt.xlim([-2.5,2.5])
    plt.ylim([-0.5,1.5])
    plt.title("FitzHugh-Nagumo Phase Space")
    plt.grid()

ani = animation.FuncAnimation(fig, update, interval=100,
                              frames=300)
#plt.show() #表示
ani.save("ode_fitzhugh_nagumo_02_phase_space.tmp.mp4") #保存
