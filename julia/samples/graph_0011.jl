using Dates
using Plots; gr()

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    Θ = range(0, stop = 1.5π, length = 100)
    r = abs.(0.1 * randn(100) + sin.(3Θ))
    plot(Θ, r, proj = :polar, m = 2)

    savefig(tgtdir * "/001.png")
end

main()
