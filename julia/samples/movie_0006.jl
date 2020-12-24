# http://optie.hatenablog.com/entry/2018/03/29/210619
import ProgressMeter
using Dates
using LaTeXStrings
using Plots; gr()

datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
ws = "workspace/"
tgtdir = ws * datetime
mkpath(tgtdir)

n = 101  # フレーム数
prog = ProgressMeter.Progress(n, 1)
is = range(-10, 10, length=n)
xs = range(-50, 50, length=201)
ys = range(-50, 50, length=201)

anim = @animate for i in range(-10, 10, length=n)
    x1 = xs
    y1 = i
    z1 = x1 .+ y1 .* im

    x2 = i
    y2 = ys
    z2 = x2 .+ y2 .* im

    z3 =  i .+ i .* im

    w1 = z1.^2
    w2 = z2.^2
    w3 = z3.^2

    # z1 が描く直線
    p1 = plot(real(z1), imag(z1),
              linecolor="#F5A623", lims=(-20,20), title="x-y")
    # z2 が描く直線の追記
    plot!(p1, real(z2), imag(z2),
          linecolor="#4A90E2")
    vline!(p1, [0], line=(:black, 1)) # y軸
    hline!(p1, [0], line=(:black, 1)) # x軸
    # z3 の点を描く
    scatter!(p1, (real(z3),imag(z3)))

    # 放物線の「右側」: 実際のグラフと左右のグラフの色対応参照
    p2 = plot(real(w1), imag(w1),
              linecolor="#F5A623",  lims=(-200,200), title="u-v")
    # 放物線の「左側」: 実際のグラフと左右のグラフの色対応参照
    plot!(p2, real(w2), imag(w2),
          linecolor="#4A90E2")
    vline!(p2, [0], line=(:black, 1)) # y 軸
    hline!(p2, [0], line=(:black, 1)) # x 軸
    scatter!(p2, (real(w3), imag(w3)))

    plot(p1, p2, size=(800, 350), plot_title=L"w=z^2")

    ProgressMeter.next!(prog)
end
mp4(anim, tgtdir * "/001.mp4", fps=30)
