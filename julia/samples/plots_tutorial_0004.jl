# http://docs.juliaplots.org/latest/input_data/
#using Dates
using Plots; gr()

# datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
# ws = "workspace/"
# tgtdir = ws * datetime
# mkpath(tgtdir)

# %%
# どれも同じプロット
plot(x = 1:10, y = rand(10))
plot(1:10, rand(10))
plot(rand(10))
# %%

# %%
# 10 data points in 4 series
xs = range(0, 2π, length = 10)
data = [sin.(xs) cos.(xs) 2sin.(xs) 2cos.(xs)]

# We put labels in a row vector: applies to each series
labels = ["Apples" "Oranges" "Hats" "Shoes"]

# Marker shapes in a column vector: applies to data points
markershapes = [:circle, :star5]

# Marker colors in a matrix: applies to series and data points
markercolors = [
    :green :orange :black :purple
    :red   :yellow :brown :white
]

plot(
    xs,
    data,
    label = labels,
    shape = markershapes,
    color = markercolors,
    markersize = 10
)
# %%

# %%
plotly()

function rectangle_from_coords(xb,yb,xt,yt)
    [
        xb  yb
        xt  yb
        xt  yt
        xb  yt
        xb  yb
        NaN NaN
    ]
end

some_rects=[
    rectangle_from_coords(1, 1, 5, 5)
    rectangle_from_coords(10, 10, 15, 15)
]
other_rects=[
    rectangle_from_coords(1, 10, 5, 15)
    rectangle_from_coords(10, 1, 15, 5)
]

plot(some_rects[:,1], some_rects[:,2], label = "some group")
plot!(other_rects[:,1], other_rects[:,2], label = "other group")
# %%

# %%
using StatsPlots, RDatasets
gr()
iris = dataset("datasets", "iris")
@df iris scatter(
    :SepalLength,
    :SepalWidth,
    group = :Species,
    m = (0.5, [:+ :h :star7], 12),
    bg = RGB(0.2, 0.2, 0.2)
)
# %%

# %%
tmin = 0
tmax = 4π
tvec = range(tmin, tmax, length = 100)
plot(sin.(tvec), cos.(tvec))
plot(sin, cos, tvec)
plot(sin, cos, tmin, tmax)
# %%

#%%
P2 = Plots.P2
function make_batman()
    p = P2[(0, 0), (0.5, 0.2), (1, 0), (1, 2),  (0.3, 1.2), (0.2, 2), (0, 1.7)]
    m = P2[(p[i] + p[i + 1]) / 2 for i in 1:length(p) - 1]
    m += P2[(0.2, 1), (0.4, 1), (2, 0), (0.5, -0.6), (0, 0), (0, -0.15)]

    pts = P2[]
    for (i, mi) in enumerate(m)
        append!(
            pts,
            map(BezierCurve(P2[p[i], m[i], p[i + 1]]), range(0, 1, length = 30))
        )
    end
    x, y = Plots.unzip(pts)
    Shape(vcat(x, -reverse(x)), vcat(y, reverse(y)))
end

# background and limits
plt = plot(
    bg = :black,
    xlim = (0.1, 0.9),
    ylim = (0.2, 1.5),
    framestyle = :none,
    size = (400, 400),
    legend = false,
)

# create an ellipse in the sky
pts = Plots.partialcircle(0, 2π, 100, 0.1)
x, y = Plots.unzip(pts)
x = 1.5x .+ 0.7
y .+= 1.3
pts = collect(zip(x, y))

# beam
beam = Shape([(0.3, 0.0), pts[95], pts[50], (0.3, 0.0)])
plot!(beam, fillcolor = plot_color(:yellow, 0.3))

# spotlight
plot!(Shape(x, y), c = :yellow)

# buildings
rect(w, h, x, y) = Shape(x .+ [0, w, w, 0, 0], y .+ [0, 0, h, h, 0])
gray(pct) = RGB(pct, pct, pct)
function windowrange(dim, denom)
    range(0, 1, length = max(3, round(Int, dim/denom)))[2:end - 1]
end

for k in 1:50
    w, h, x, y = 0.1rand() + 0.05, 0.8rand() + 0.3, rand(), 0.0
    shape = rect(w, h, x, y)
    graypct = 0.3rand() + 0.3
    plot!(shape, c = gray(graypct))

    # windows
    I = windowrange(w, 0.015)
    J = windowrange(h, 0.04)
    pts = vec([(Float64(x + w * i), Float64(y + h * j)) for i in I, j in J])
    windowcolors = Symbol[rand() < 0.2 ? :yellow : :black for i in 1:length(pts)]
    scatter!(pts, marker = (stroke(0), :rect, windowcolors))
end
plt

# Holy plotting, Batman!
batman = Plots.scale(make_batman(), 0.07, 0.07, (0, 0))
batman = translate(batman, 0.7, 1.23)
plot!(batman, fillcolor = :black)
#%%
