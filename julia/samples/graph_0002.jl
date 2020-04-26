using Dates
using GRUtils

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    x = LinRange(0, 30, 1000)
    y = cos.(x) .* x
    z = sin.(x) .* x
    plot3(x, y, z, x[1:10:end], y[1:10:end], z[1:10:end])
    savefig(tgtdir * ".png")
end

main()
