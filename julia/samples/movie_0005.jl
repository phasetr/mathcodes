# http://docs.juliaplots.org/latest/generated/gr/#gr-examples-1
using Dates
using Plots; gr()

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    p = plot([sin, cos], zeros(0), leg = false)
    anim = @animate for x = range(0, stop = 10π, length = 100)
        push!(p, x, Float64[sin(x), cos(x)])
    end
    mp4(anim, tgtdir * "/001.mp4")
end

main()
