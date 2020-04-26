# https://qiita.com/Mizuto_Kadowaki/items/0a1f67441568424ccb64
using Dates
using DifferentialEquations
using Plots; gr()

@time function main()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    # 微分方程式を定義
    f(y, t) = 3.0 * y + 2

    # 初期値を設定
    y0 = 1.0

    # 時間間隔を設定
    tspan = (0.0, 1.0)

    # ODEProblemで解く
    prob = ODEProblem(f, y0, tspan)
    sol = solve(prob)

    # plotする
    plot(sol,linewidth=5, title="Solution to the linear ODE with a thick line",
         xaxis="Time (t)", yaxis="y(t) (in micro.m)",label="My Thick Line!")

    # 厳密解を用意する
    g(t) = 5//3 * exp.(3*t) - 2//3

    # 加えてplotする
    plot!(g, lw=3, ls=:dash, label="True Solution!")

    savefig(tgtdir * ".png")
end

main()
