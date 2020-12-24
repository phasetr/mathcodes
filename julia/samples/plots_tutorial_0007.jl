using Plots

datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
ws = "workspace/"
tgtdir = ws * datetime * "/"
mkpath(tgtdir)

#%%
@userplot CirclePlot
@recipe function f(cp::CirclePlot)
    x, y, i = cp.args
    n = length(x)
    inds = circshift(1:n, 1 - i)
    linewidth --> range(0, 10, length = n)
    seriesalpha --> range(0, 1, length = n)
    aspect_ratio --> 1
    label --> false
    x[inds], y[inds]
end
#%%

#%%
n = 150
t = range(0, 2π, length = n)
x = sin.(t)
y = cos.(t)

anim = @animate for i ∈ 1:n
    circleplot(x, y, i)
end
gif(anim, tgtdir * "anim_fps15.gif", fps = 15)
gif(anim, tgtdir * "anim_fps30.gif", fps = 30)
#%%

#%%
anim = @animate for i ∈ 1:n
    circleplot(x, y, i, line_z = 1:n, cbar = false, framestyle = :zerolines)
end every 5
mp4(anim, tgtdir * "anim_001.mp4")
#%%

#%%
n = 400
t = range(0, 2π, length = n)
x = 16sin.(t).^3
y = 13cos.(t) .- 5cos.(2t) .- 2cos.(3t) .- cos.(4t)
anim = @animate for i ∈ 1:n
    circleplot(x, y, i, line_z = 1:n, cbar = false, c = :reds, framestyle = :none)
end when i > 40 && mod1(i, 10) == 5
mp4(anim, tgtdir * "anim_002.mp4", fps = 15)
#%%
