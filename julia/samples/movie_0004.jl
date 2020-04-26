# http://optie.hatenablog.com/entry/2018/03/29/210619
using Dates
using Plots
import ProgressMeter
using LaTeXStrings

gr(
    legend=false,
    titlefont=Plots.font("sans-serif", 12),
    legendfont=Plots.font("sans-serif", 8),
    guidefont=Plots.font("sans-serif", 10),
    tickfont=Plots.font("sans-serif", 8),
    markersize=4, markerstrokewidth = 1,
    markercolor= "#FF445B",markerstrokecolor="#4A4A4A",
    linewidth=2
)

ENV["PLOTS_TEST"] = "true"

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    n = 100  # フレーム数
    prog = ProgressMeter.Progress(n,1)


    anim = @animate for i in range(-10.0, stop=10.0, length=n)
        k = i

        x1 = range(-50.0, stop=50.0, length=201)
        y1 = k
        z1 = x1 + y1*im

        x2 = k
        x1 = range(-50.0, stop=50.0, length=201)
        z2 = x2 + y2*im

        z3 =  k + k*im

        w1 = z1.^2
        w2 = z2.^2
        w3 = z3.^2

        p1 = plot(real(z1), imag(z1), linecolor="#F5A623", lims=(-20,20), title="x-y")
        plot!(p1, real(z2), imag(z2), linecolor="#4A90E2")
        vline!(p1,[0],line=(:black, 1)) # y軸
        hline!(p1,[0],line=(:black, 1)) # x軸
        scatter!(p1, (real(z3),imag(z3)))


        p2 = plot(real(w1),imag(w1), linecolor="#F5A623",  lims=(-200,200), title="u-v")
        plot!(p2, real(w2),imag(w2), linecolor="#4A90E2")
        vline!(p2,[0],line=(:black, 1))
        hline!(p2,[0],line=(:black, 1))
        scatter!(p2, (real(w3),imag(w3)))

        plot(p1,p2,size=(800,350), plot_title=L"w=z^2")

        ProgressMeter.next!(prog)
    end


    dir = "/"
    fps = 30
    filename = "01_fps$fps.gif"

    gif(anim, filename, fps = fps)

    displayfile(dir, filename)
end

main()
