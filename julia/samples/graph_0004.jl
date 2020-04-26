# http://www.cas.cmc.osaka-u.ac.jp/~paoon/misc/julia/post/trouble-plots-package/
using Dates
using Plots
gr()

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)
    println("Temporal target directly is " * tgtdir)

    N = 200
    d = 8π/N
    t = LinRange(0, N, 500)
    x = d .* t .* cos.(d .* t)
    y = d .* t .* sin.(d .* t)
    z = t

    plot(x, y, z, marker=:circle)

    savefig(tgtdir * ".png")
end

main()
