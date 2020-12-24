# http://docs.juliaplots.org/latest/layouts/
using Plots; gr()

#%%
# create a 2x2 grid, and map each of the 4 series to one of the subplots
plot(rand(100, 4), layout = 4)
#%%

#%%
# create a 4x1 grid, and map each of the 4 series to one of the subplots
plot(rand(100, 4), layout = (4, 1))
#%%

#%%
plot(rand(100, 4), layout = grid(4, 1, heights=[0.1 ,0.4, 0.4, 0.1]))
#%%

#%%
plot(rand(100,4), layout = 4, label=["a" "b" "c" "d"],
    title=["1" "2" "3" "4"])
#%%

#%%
l = @layout [
    a{0.3w} [grid(3,3)
             b{0.2h}  ]
]
plot(
    rand(10, 11),
    layout = l, legend = false, seriestype = [:bar :scatter :path],
    title = ["($i)" for j in 1:1, i in 1:11], titleloc = :right, titlefont = font(8)
)
#%%

#%%
# boxplot is defined in StatsPlots
using StatsPlots, StatsPlots.PlotMeasures
gr(leg = false, bg = :lightgrey)

# Create a filled contour and boxplot side by side.
plot(contourf(randn(10, 20)), boxplot(rand(1:4, 1000), randn(1000)))

# Add a histogram inset on the heatmap.
# We set the (optional) position relative to bottom-right of the 1st subplot.
# The call is `bbox(x, y, width, height, origin...)`, where numbers are treated as
# "percent of parent".
histogram!(
    randn(1000),
    inset = (1, bbox(0.05, 0.05, 0.5, 0.25, :bottom, :right)),
    ticks = nothing,
    subplot = 3,
    bg_inside = nothing
)

# Add sticks floating in the window (inset relative to the window, as opposed to being
# relative to a subplot)
sticks!(
    randn(100),
    inset = bbox(0, -0.2, 200px, 100px, :center),
    ticks = nothing,
    subplot = 4
)
#%%
