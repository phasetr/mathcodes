# https://qiita.com/Lirimy/items/4cb7adf149d75753bf25
using Dates
using Plots; gr()

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    xe = range(-1, 1, length=15)
    ye = range(-1, 1, length=15)

    x = repeat(xe, outer=length(ye))
    y = repeat(ye, inner=length(xe))

    u = y
    v = x
    k = 0.1
    quiver(x, y, quiver=(k*u, k*v))
    savefig(tgtdir * "/001.png")

    u = y
    v = -x
    k = 0.1
    quiver(x, y, quiver=(k*u, k*v))
    savefig(tgtdir * "/002.png")
end

main()
