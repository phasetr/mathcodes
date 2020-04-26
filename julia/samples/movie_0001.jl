# https://heliosdrm.github.io/GRUtils.jl/stable/animations/
using Dates
using GRUtils

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    xmax = 2000

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
