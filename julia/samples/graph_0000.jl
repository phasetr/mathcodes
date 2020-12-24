# ## グラフを描いてみる
# ### 三角関数
# %%
#import Pkg
#Pkg.add("Plots")
#Pkg.add("GR")
using Plots; gr();
plot([sin, cos], -π, π)
savefig("sin-cos.tmp.png")
# ### シグモイド関数
x = -5.0:0.1:5.0
f(x) = 1 ./ (1 .+ exp.(-x))
y = f(x)
plot(x, y)
savefig("sigmoid.tmp.png")
# %%
