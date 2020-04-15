# coding: utf-8
# https://omedstu.jimdofree.com/2018/06/21/fitzhugh-nagumoモデルをアニメーションで見る/
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
t = np.arange(0.0, 10, 0.01)
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
    plt.plot(t, u, label="u : membrane potential", color="#ff7f0e")
    plt.plot(t, v, label="v : recovery variable", color="#1f77b4")
    plt.plot(t[len_t-1], u[len_t-1],'o--', color="#ff7f0e") #uのmarker
    plt.plot(t[len_t-1], v[len_t-1],'o--', color="#1f77b4") #vのmarker
    plt.title("Membrane Potential / Volt")
    plt.grid()
    plt.legend(bbox_to_anchor=(0, 1),
               loc='upper left',
               borderaxespad=0)

ani = animation.FuncAnimation(fig, update, interval=100,
                              frames=300)
#plt.show() #表示
ani.save("ode_fitzhugh_nagumo_01_membrane_voltage.tmp.mp4") #保存
