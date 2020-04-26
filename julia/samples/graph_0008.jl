# http://docs.juliaplots.org/latest/generated/gr/#gr-examples-1
using Dates
using Plots; gr()

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    plot(sin, (x->begin
               sin(2x)
               end), 0, 2π, line = 4, leg = false, fill = (0, :orange))
    savefig(tgtdir * "/001.png")
end

main()
