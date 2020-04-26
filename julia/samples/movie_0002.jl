# https://heliosdrm.github.io/GRUtils.jl/stable/api/plotting/#Plotting-functions-1
using Dates
using GRUtils

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    # Create example data
    x = LinRange(0, 30, 1000)
    y = cos.(x) .* x
    z = sin.(x) .* x
    # Draw a solid line and another with star markers
    # in one of every 10 points
    plot3(x, y, z, x[1:10:end], y[1:10:end], z[1:10:end])

    x = LinRange(0, xmax, 100)
    y = sind.(x)
    plot(x,y)
    # Make a video sliding over the X axis
    videofile(ws * datetime * ".mp4") do
        for d = 0:10:1000
            xlim(d, d+360)
            draw(gcf())
        end
    end
end

main()
