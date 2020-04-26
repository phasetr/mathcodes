# https://heliosdrm.github.io/GRUtils.jl/stable/api/control/
using Dates
using GRUtils

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    # Create example data
    x = randn(100_000)
    y = randn(100_000)
    # Draw an hexagonal plot in the bigger bottom-right region
    subplot(3, 3, (5, 9))
    hexbin(x, y, colorbar = false)
    # Draw marginal histograms
    subplot(3, 3, (2, 3))
    histogram(x)
    subplot(3, 3, (4, 7))
    histogram(y, horizontal = true, xflip = true)
    # Draw a shade plot in the smaller top-left region
    subplot(3, 3, 1)
    shade(x, y)

    savefig(tgtdir * ".png")
end

main()
