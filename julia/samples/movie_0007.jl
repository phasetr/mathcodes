#=
オリジナル: http://optie.hatenablog.com/entry/2018/03/29/210619
このコードを Julia 1.4.1 での mp4 生成用に修正している.

- [Julia 複素変数のw=1/z Plots/GR](https://youtu.be/DCHdA9bOg-4)
- [Julia 複素変数のw=1/z その2 Plots/GR](https://youtu.be/DCHdA9bOg-4)
- [Julia 複素変数のw=sin z その1 Plots/GR](https://youtu.be/UhRbtFksitw)
- [Julia 複素変数のw=sin z その2 Plots/GR](https://youtu.be/xbxrXuWidFU)
- [Julia 複素変数のw=tan z その1 Plots/GR](https://youtu.be/xBRwvk9BXls)
- [Julia 複素変数のw=tan z その2 Plots/GR](https://youtu.be/TuA70-FJB-w)
- [Julia 複素変数のw=e^z その1 Plots/GR 008.mp4](https://youtu.be/TuA70-FJB-w)
- [Julia 複素変数のw=e^z その2 Plots/GR 009.mp4](https://youtu.be/3mj_2AIhrFM)
- [Julia 複素変数のw=log z Plots/GR 010.mp4](https://youtu.be/uWMqawcSI_Q)
- [Julia 複素変数のw=sinh z その1 Plots/GR 011.mp4](https://youtu.be/pZ9Rbv7BmNo)
- [Julia 複素変数のw=sinh z その2 Plots/GR 012.mp4](https://youtu.be/q-ueY7gHEv8)
- [Julia 複素変数のw=z^{1/3} その2 Plots/GR 013.mp4](https://youtu.be/U1U8GBUoNGE)
=#
# Julia 複素変数のw=1/z Plots/GR
import ProgressMeter
using Dates
using Plots
gr()

datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
ws = "workspace/"
tgtdir = ws * datetime
mkpath(tgtdir)

# 複素関数 f, 定数kを受け取り複素数列(点群)を返す shape の配列 shapes
function makeanim(
    f,
    title,
    shapes;
    n = 100,
    kmin = -10,
    kmax = 10,
    zsize = 10,
    wsize = 10,
    unitcircle = true,
)
    prog = ProgressMeter.Progress(n, 1)

    anim = @animate for i in range(kmin, kmax, length = n)
        p1 = plot(lims = (-zsize, zsize), title = "z: x-y")
        p2 = plot(lims = (-wsize, wsize), title = "f(z)=" * title)
        vline!(p1, [0], line = (:black, 1))
        hline!(p1, [0], line = (:black, 1))
        vline!(p2, [0], line = (:black, 1))
        hline!(p2, [0], line = (:black, 1))

        if unitcircle
            θ = range(0, 2π, length = 60)
            plot!(p1, sin.(θ), cos.(θ), line = (:black, 1))
            plot!(p2, sin.(θ), cos.(θ), line = (:black, 1))
        end

        k = i

        for shape in shapes
            z = shape(k)
            w = f(z)

            if length(z) > 1  # 点ならscatterを使う
                plot!(p1, real(z), imag(z))
            else
                scatter!(p1, (real(z), imag(z)))
            end

            if length(w) > 1
                plot!(p2, real(w), imag(w))
            else
                scatter!(p2, (real(w), imag(w)))
            end
        end

        plot(p1, p2, size = (800, 400))
        ProgressMeter.next!(prog)
    end

    anim
end

function yline(k)
    x = range(-50, 50, length = 1001)
    y = k
    z = x .+ y .* im
end

function xline(k)
    x = k
    y = range(-50, 50, length = 1001)
    z = x .+ y .* im
end

function xylinecrosspoint(k)
    x = k
    y = k
    z = x + y * im
end

function circle(k)
    r = k
    θ = range(-π, π, length = 10000)
    z = r .* (cos.(θ) .+ im .* sin.(θ))
end

function circleline(k)
    r = range(0, 100, length = 10001)
    θ = 2k
    z = r .* (cos.(θ) .+ im .* sin.(θ))
end

function circlepoint(k)
    k_f = float(k)
    r = k_f
    z = r * (cos(2k_f) + im * sin(2k_f))
end

fps = 30

#################################################

function f1(z)
    z .^ 2
end

shapes = [yline, xline, xylinecrosspoint]
anim = makeanim(f1, "z^2", shapes, zsize = 20, wsize = 200, unitcircle = false)
mp4(anim, tgtdir * "/001.mp4", fps = 30)

#################################################

function f2(z)
    1 ./ z
end

shapes1 = [yline, xline, xylinecrosspoint]
shapes2 = [circle, circleline, circlepoint]
anim = makeanim(f2, "1/z", shapes1, n = 150, kmin = -2, kmax = 2, zsize = 2, wsize = 2)
mp4(anim, tgtdir * "/002.mp4", fps = 30)

anim2 =
    makeanim(f2, "1/z", shapes2, n = 250, kmin = 2π, kmax = 0, zsize = 2π, wsize = 2π)
mp4(anim2, tgtdir * "/003.mp4", fps = fps)

# w = sin(z)
function f3(z)
    sin.(z)
end
anim1 =
    makeanim(f3, "sin z", shapes1, n = 150, kmin = -2, kmax = 2, zsize = 2, wsize = 2)
mp4(anim1, tgtdir * "/004.mp4", fps = fps)
anim2 =
    makeanim(f3, "sin z", shapes2, n = 500, kmin = π, kmax = 0, zsize = 2π, wsize = 2π)
mp4(anim2, tgtdir * "/005.mp4", fps = fps)

# w=tan(z)
function f4(z)
    tan.(z)
end
anim1 = makeanim(f4, "tan z", shapes1, n=150, kmin=-2, kmax=2, zsize=2, wsize=2)
mp4(anim1, tgtdir * "/006.mp4", fps = fps)
anim2 = makeanim(f4, "tan z", shapes2, n=500, kmin=π, kmax=0, zsize=2π, wsize=2π)
mp4(anim2, tgtdir * "/007.mp4", fps = fps)

# w = e^z
function f5(z)
    ℯ.^z # ネイピア数は \euler で打ち込めばいい
end
anim1 = makeanim(f5, "e^z", shapes1, n=150, kmin=-2, kmax=2, zsize=2, wsize=2)
mp4(anim1, tgtdir * "/008.mp4", fps = fps)
anim2 = makeanim(f5, "e^z", shapes2, n=500, kmin=π, kmax=0, zsize=2π, wsize=4π)
mp4(anim2, tgtdir * "/009.mp4", fps = fps)

# w = log z
function f6(z)
    log.(z)
end
anim1 = makeanim(f6, "log z", shapes1, n=150, kmin=-2, kmax=2, zsize=2, wsize=2)
mp4(anim1, tgtdir * "/010.mp4", fps = fps)

# w = sinh z
function f7(z)
    sinh.(z)
end
anim1 = makeanim(f7, "sinh z", shapes1, n=150, kmin=-2, kmax=2, zsize=2, wsize=2)
mp4(anim1, tgtdir * "/011.mp4", fps = fps)
anim2 = makeanim(f7, "sinh z", shapes2, n=200, kmin=π, kmax=0, zsize=π, wsize=π)
mp4(anim2, tgtdir * "/012.mp4", fps = fps)

# w = z^{1/3}
function f8(z)
    z.^(1/3)
end
anim2 = makeanim(f8, "z^{1/3}", shapes2, n=200, kmin=π, kmax=0, zsize=π, wsize=π)
mp4(anim2, tgtdir * "/013.mp4", fps = fps)
