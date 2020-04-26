# https://heliosdrm.github.io/GRUtils.jl/stable/api/plotting/#Plotting-functions-1
using Dates
using GRUtils

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    #mkpath(tgtdir)

    # plot
    # Create example data
    x = LinRange(-2, 2, 40)
    f(t) = t^3 + t^2 + t
    y = f.(x)
    # Plot x and y
    plot(x, y)
    # Plot x using the callable
    plot(x, f)
    # Plot y, using its indices for the x values
    plot(y)
    # Plot two columns
    plot(x, [y 3x.^2 .- 3])
    # Plot various series them with custom line specs
    y2 = 3x.^2 .- 3
    plot(x, y, "-r", x, y2, ":*b")
    #Figure((800.0, 450.0))
    #aspectratio(16/9)
    savefig(tgtdir * "_graph_0001_001_plot.tmp.png")

    # oplot
    x = LinRange(-2, 2, 40)
    y = 2 .* x .+ 4
    # Draw the first plot
    plot(x, y)
    # Plot another graph over it
    oplot(x, x -> x^3 + x^2 + x)
    savefig(tgtdir * "_graph_0001_002_oplot.tmp.png")

    # stair
    # Create example data
    x = LinRange(-2, 2, 40)
    y = x.^3 .+ x.^2 .+ x
    # Plot x and y
    stair(x, y)
    # Plot y with indices for x values
    stair(y)
    # step directly after x each position
    stair(y, where="pre")
    # step between two x positions
    stair(y, where="mid")
    # step immediately before x each position
    stair(y, where="post")
    savefig(tgtdir * "_graph_0001_003_stair.tmp.png")

    # plot3
    # Create example data
    x = LinRange(0, 30, 1000)
    y = cos.(x) .* x
    z = sin.(x) .* x
    # Draw a solid line and another with star markers
    # in one of every 10 points
    plot3(x, y, z, x[1:10:end], y[1:10:end], z[1:10:end], "p")
    savefig(tgtdir * "_graph_0001_004_plot3.tmp.png")

    # polar
    # Create example data
    angles = LinRange(0, 360, 40)
    radii = LinRange(0, 2, 40)
    # Draw the polar plot in degrees
    polar(angles, radii, radians=false)
    savefig(tgtdir * "_graph_0001_005_polar.tmp.png")

    # scatter
    # Create example data
    x = LinRange(0, 1, 11)
    y = LinRange(0, 1, 11)
    s = LinRange(50, 400, 11)
    c = LinRange(0, 255, 11)
    # Plot the points with increasing size and color
    scatter(x, y, s, c)
    savefig(tgtdir * "_graph_0001_006_scatter.tmp.png")

    # scatter3
    # Create example data
    x = 2 .* rand(100) .- 1
    y = 2 .* rand(100) .- 1
    z = 2 .* rand(100) .- 1
    c = 999 .* rand(100) .+ 1
    # Plot the points with colors
    scatter3(x, y, z, c)
    savefig(tgtdir * "_graph_0001_007_scatter3.tmp.png")

    # quiver
    # Create example data
    x = repeat(LinRange(-2, 2, 20), inner=10)
    y = repeat(LinRange(-1, 1, 10), outer=20)
    u = x .* (x.^2 .+ y.^2)
    v = y .* (x.^2 .+ y.^2)
    # Plot arrows
    quiver(x, y, u, y, arrowscale=0.1)
    # Expand the y-axes to see the whole arrows
    ylim(-1.15, 1.15)
    savefig(tgtdir * "_graph_0001_008_quiver.tmp.png")

    # quiver3
    # Create example data
    x = repeat(LinRange(-2, 2, 20), inner=10)
    y = repeat(LinRange(0, pi, 10), outer=20)
    z = sin.(x) .+ cos.(y)
    u = 0.1ones(200)
    v = zeros(200)
    w = 0.5z
    # Plot vectors
    quiver3(x, y, z, u, v, w, "o", markersize=0.5)
    savefig(tgtdir * "_graph_0001_009_quiver3.tmp.png")

    # hexbin: hexagon binning
    # Create example data
    x = randn(100_000)
    y = randn(100_000)
    # Draw the hexbin plot
    hexbin(x, y)
    savefig(tgtdir * "_graph_0001_010_hexbin.tmp.png")

    println("""その他は次の URL 参照.
https://heliosdrm.github.io/GRUtils.jl/stable/api/plotting/#Plotting-functions-1""")
end

main()
