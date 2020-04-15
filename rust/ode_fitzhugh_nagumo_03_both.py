# coding: utf-8
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import scipy.integrate as integrate

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

fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(10,4))
fig.suptitle(f"FitzHugh-Nagumo Model, I={I}")

len_t = len(t)
dt = 5 #time steps

#animationの1step
def update(i, I):
    global y, y0

    #y0の初期値の設定
    if i ==0:
        y0 = [u0, v0]

    #現在描写されているグラフを消去
    ax1.cla()
    ax2.cla()

    #微分方程式を解く
    y = integrate.odeint(FHN, y0, t, args=(I,))

    #1Step(=dt)後のyの値を次のステップでのy0の値に更新する
    y0 = (y[dt,0], y[dt,1])

    #配列としてu,vを取得
    u = y[:,0]
    v = y[:,1]

    #Phase Space
    ax1.plot(u_all, v_all, color="k", dashes=[1, 6])
    ax1.plot(u[len_t-20:len_t-1], v[len_t-20:len_t-1], color="r")
    ax1.plot(u[len_t-1], v[len_t-1], 'o--', color="r") #uのmarker
    ax1.set_xlabel("u : membrane potential / Volt")
    ax1.set_ylabel("v : recovery variable")
    ax1.set_xlim([-2.2, 2.2])
    ax1.set_ylim([-0.5, 1.5])
    ax1.set_title("Phase Space")
    ax1.grid()

    #Membrane Potential
    ax2.plot(t, u, label="u : membrane potential", color="#ff7f0e")
    ax2.plot(t, v, label="v : recovery variable", color="#1f77b4")
    ax2.plot(t[len_t-1], u[len_t-1],'o--', color="#ff7f0e") #uのmarker
    ax2.plot(t[len_t-1], v[len_t-1],'o--', color="#1f77b4") #vのmarker
    ax2.set_title("Membrane Potential / Volt")
    ax2.set_ylim([-2.2,2.0])
    ax2.grid()
    ax2.legend(bbox_to_anchor=(0, 1),
               loc='upper left',
               borderaxespad=0)

ani = animation.FuncAnimation(fig, update,
                              fargs=(I,),
                              interval=100, frames=300)
# plt.show() #表示
ani.save("ode_fitzhugh_nagumo_03_both_I_034.tmp.mp4") #保存

# I = 0.33
I = 0.33
y_all = integrate.odeint(FHN, [u0, v0], t0, args=(I,))
u_all = y_all[:,0]
v_all = y_all[:,1]
fig, (ax1, ax2) = plt.subplots(ncols=2, figsize=(10,4))
fig.suptitle(f"FitzHugh-Nagumo Model, I={I}")
ani = animation.FuncAnimation(fig, update,
                              fargs=(I,),
                              interval=100, frames=300)
# plt.show() #表示
ani.save("ode_fitzhugh_nagumo_03_both_I_033.tmp.mp4") #保存
