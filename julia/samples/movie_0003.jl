# https://nbviewer.jupyter.org/gist/genkuroki/56d3063f90f3e3a16d529f8feca2ba5b
using Base64
using Statistics

using Plots
#clibrary(:colorcet)
default(fmt=:svg, size=(240, 240))
gr()

using Measures: cm

# 領域
function domain(isinDomain, x, y)
    Domain = isinDomain.(x, y')
    Domain[1:end, 1] .= false
    Domain[1:end, end] .= false
    Domain[1, 1:end] .= false
    Domain[end, 1:end] .= false
    Domain
end

interiorof(Domain) = [
    (i,j) for i in 1:size(Domain,1) for j in 1:size(Domain,2) if Domain[i,j]
]

complementof(Domain) = [
    (i,j) for i in 1:size(Domain,1) for j in 1:size(Domain,2) if !Domain[i,j]
]

isinBoundary(Domain, i, j) = (
    (i != 1 && Domain[i-1,j]) ||
    (i != size(Domain,1) && Domain[i+1,j]) ||
    (j != 1 && Domain[i,j-1]) ||
    (j != size(Domain,2) && Domain[i,j+1])
)

boundaryof(Domain, Complement) = [(i,j) for (i,j) in Complement if isinBoundary(Domain, i, j)]

exteriorof(Domain, Complement) = [(i,j) for (i,j) in Complement if !isinBoundary(Domain, i, j)]

function boundaryformof(Domain, Boundary)
    BF = Array{Tuple{eltype(Boundary), Array{eltype(Boundary),1}}}(undef, length(Boundary))
    V = [(-1,0), (1,0), (0,-1), (0,1)]
    for (k, (i,j)) in enumerate(Boundary)
        A = [(i+p, j+q) for (p,q) in V if (1 ? i+p ? size(Domain,1)) && (1 ? j+q ? size(Domain,2)) && Domain[i+p,j+q]]
        BF[k] = ((i,j), A)
    end
    BF
end

@time function test()
    datetime = Dates.format(now(), "yyyymmdd-HHMMSS")
    ws = "workspace/"
    tgtdir = ws * datetime
    mkpath(tgtdir)

    x = -0.6:0.2:0.6
    y = -0.5:0.2:0.5
    isinDomain(x,y) = (x^2+y^2 < 0.5^2)
    Domain = domain(isinDomain, x, y)
    Interior = interiorof(Domain)
    Complement = complementof(Domain)
    Boundary = boundaryof(Domain, Complement)
    Exterior = exteriorof(Domain, Complement)

    heatmap(Domain', colorbar=false)
    scatter!(Interior, legend=false, color=:blue)
    scatter!(Complement, legend=false, color=:cyan, markersize=8)
    scatter!(Boundary, legend=false, color=:red)
    scatter!(Exterior, legend=false, color=:yellow)
    savefig(tgtdir * ".png")
end

# Falling Membrane Problem 型の定義
struct FMProblem{
        Q<:Integer,
        R<:AbstractFloat,
        Sx<:AbstractArray{R,1}, Sy<:AbstractArray{R,1}, St<:AbstractArray{R,1},
        D<:AbstractArray{Bool,2}, BC,
        TI<:AbstractArray, TB<:AbstractArray, TE<:AbstractArray, TBF<:AbstractArray,
        U1<:AbstractArray{R,2}, U0<:AbstractArray{R,2},
        Fg, Fprepare, Fupdate
    }
    nskips::Q;  nsteps::Q
    Δx::R;  Δy::R;  Δt::R
    δ::R;  δ2::R;  Δt2::R
    x::Sx;  y::Sy;  t::St
    Domain::D;  bc::BC;
    Interior::TI;  Boundary::TB;  Exterior::TE;  BoundaryForm::TBF
    u1::U1;  u0::U0
    g::Fg;  prepare!::Fprepare;  update!::Fupdate
end

function spanstointervals(Δx, Δt, xspan, yspan, tspan)
    nskips = round(Int, Δx/Δt)
    δ = 1/nskips
    h, Δy, Δt = Δx, Δx, δ*Δx
    δ2, Δt2 = δ^2, Δt^2

    xmax = xspan[1] + ceil((xspan[2] - xspan[1])/h) * h
    ymax = yspan[1] + ceil((yspan[2] - yspan[1])/h) * h
    tmax = tspan[1] + ceil((tspan[2] - tspan[1])/h) * h
    x = range(xspan[1], xmax, step=h)
    y = range(yspan[1], ymax, step=h)
    t = range(tspan[1], tmax, step=h)
    nsteps = length(t) - 1

    nskips, δ, h, Δy, Δt, δ2, Δt2, x, y, t, nsteps
end

function domainsetting(Domain)
    Complement = complementof(Domain)
    Interior = interiorof(Domain)
    Boundary = boundaryof(Domain, Complement)
    Exterior = exteriorof(Domain, Complement)
    BoundaryForm = boundaryformof(Domain, Boundary)

    Complement, Interior, Boundary, Exterior, BoundaryForm
end

function solversetting(bc)
    if lowercase(string(bc)) == "dirichlet"
        prepare! = prepare_dirichlet!
        update! = update_dirichlet!
    elseif lowercase(string(bc)) == "neumann"
        prepare! = prepare_neumann!
        update! = update_neumann!
    else
        error("bc must be Dirichlet or Neumann")
    end
    prepare!, update!
end

function FMProblem(
        F1, F0,
        Δx::R, Δt::R,
        xspan::Tuple{R,R}, yspan::Tuple{R,R}, tspan::Tuple{R,R},
        isinDomain, bc, g
    ) where R<:AbstractFloat
    nskips, δ, h, Δy, Δt, δ2, Δt2, x, y, t, nsteps = spanstointervals(Δx, Δt, xspan, yspan, tspan)
    Domain = domain(isinDomain, x, y)
    Complement, Interior, Boundary, Exterior, BoundaryForm = domainsetting(Domain)
    u1 = F1.(x,y')
    u0 = F0.(x,y')
    prepare!, update! = solversetting(bc)

    FMProblem(
        nskips, nsteps,
        Δx, Δy, Δt,
        δ, δ2, Δt2,
        x, y, t,
        Domain, bc,
        Interior, Boundary, Exterior, BoundaryForm,
        u1, u0,
        g, prepare!, update!
    )
end

function FMProblem(
        F1, F0,
        Δx, Δt,
        xspan, yspan, tspan,
        isinDomain, bc, g
    )
    FMProblem(
        F1, F0,
        float(Δx), float(Δt),
        float.(xspan), float.(yspan), float.(tspan),
        isinDomain, bc, g
    )
end

# 境界条件をみたす状態を作る函数達
function fillexteriorwithnans!(s::FMProblem, u)
    for (i,j) in s.Exterior
        u[i,j] = NaN
    end
end

function fillboundarywithzeros!(s::FMProblem, u)
    for (i,j) in s.Boundary
        u[i,j] = 0
    end
end

function makeboundaryfree!(s::FMProblem, u)
    for ((i,j), A) in s.BoundaryForm
        u[i,j] = mean(u[k,l] for (k,l) in A)
    end
end

function prepare_dirichlet!(s::FMProblem, u)
    fillboundarywithzeros!(s, u)
    fillexteriorwithnans!(s, u)
end

function prepare_neumann!(s::FMProblem, u)
    makeboundaryfree!(s, u)
    fillexteriorwithnans!(s, u)
end

# 時間発展
function update_interior!(s::FMProblem, u, u1, u0)
    for (i,j) in s.Interior
        u[i,j] = 2u1[i,j] - u0[i,j] + s.δ2*(-4u1[i,j] + u1[i-1,j] + u1[i+1,j] + u1[i,j-1] + u1[i,j+1]) - s.Δt2*s.g(u1[i,j])
    end
end

function update_dirichlet!(s::FMProblem, u, u1, u0)
    update_interior!(s::FMProblem, u, u1, u0)
end

function update_neumann!(s::FMProblem, u, u1, u0)
    update_interior!(s, u, u1, u0)
    makeboundaryfree!(s, u)
end

# 数値解を求める函数
function solve(s::FMProblem)
    nx, ny = size(s.Domain)
    u = Array{eltype(s.u1), 3}(undef, nx, ny, s.nsteps+1)
    u[:,:,1] .= s.u1
    for j in 1:s.nsteps+1
        @views s.prepare!(s, u[:,:,j])
    end

    utmp = Array{eltype(s.u1), 3}(undef, nx, ny, s.nskips+2)
    utmp[:,:,end]   .= s.u1
    utmp[:,:,end-1] .= s.u0
    for i in 1:s.nskips+2
        @views s.prepare!(s, utmp[:,:,i])
    end

    for j in 1:s.nsteps
        @. @views utmp[:,:,1] = utmp[:,:,end-1]
        @. @views utmp[:,:,2] = utmp[:,:,end]
        for i in 2:s.nskips+1
            @views s.update!(s, utmp[:,:,i+1], utmp[:,:,i], utmp[:,:,i-1])
        end
        @. @views u[:,:,j+1] = utmp[:,:,end]
    end
    u
end

# プロット時に数値解の値の制限を計算する函数
rmnans(u) = u[.!isnan.(u)]

ulim_auto(ut) = :auto

function ulim_dirichlet(ut)
    umax = min(eltype(ut)(10.0), maximum(abs.(rmnans(ut))))
    umin = -umax
    umin, umax
end

function make_ulim_fix(u)
    uminmax = minimum(maximum(rmnans(@view u[:,:,j])) for j in 1:size(u,3))
    umaxmin = maximum(minimum(rmnans(@view u[:,:,j])) for j in 1:size(u,3))
    @show uminmax, umaxmin
    umin1 = uminmax - 0.15*(umaxmin - uminmax)
    umax1 = umaxmin + 0.15*(umaxmin - uminmax)
    @show umin1, umax1
    umin_all = minimum(rmnans(u))
    umax_all = maximum(rmnans(u))
    @show umin_all, umax_all
    umax = min(umax1, umax_all)
    umin = max(umin1, umin_all)
    @show umin, umax
    ulim_neumann(ut) = umin, umax
    ulim_neumann
end

# 2 次元プロット
function plot_sol(u, ut, clim, color; kwargs...)
    plot(; legend=false, showaxis=false, grid=false, kwargs...)
    heatmap!(ut', color=color, clim=clim)
end

function plot_sol25(s::FMProblem, u; color=:RdBu, ulim=ulim_dirichlet, kwargs...)
    @views P = plot_sol(u, u[:,:,1], ulim(u[:,:,1]), color; kwargs...)
    PP = [P]
    for k in 1:24
        j = k*size(u,3) ÷ 24
        @views P = plot_sol(u, u[:,:,j], ulim(u[:,:,j]), color; kwargs...)
        push!(PP, P)
    end
    plot(PP..., size=(800, 700), layout=(5,5))
end

function animate_sol(s::FMProblem, u; fps=10, figsize=(220, 220), color=:RdBu, ulim=ulim_dirichlet)
    M = size(u,3)
    tskip = round(Int, 1/(fps*s.Δx))
    anim = @animate for j in [fill(1,10); 1:tskip:M; fill(M, 10)]
        plot(; size=figsize, legend=false, showaxis=false, grid=false)
        plot!(left_margin=-1cm, right_margin=-1cm, top_margin=-1cm, bottom_margin=-1cm)
        @views heatmap!(u[:,:,j]', color=color, clim=ulim(u[:,:,j]))
    end
    anim
end

function gif_sol(s::FMProblem, u; fps=10, figsize=(220, 220), color=:RdBu, gifname="test.gif", ulim=ulim_dirichlet)
    @time anim = animate_sol(s, u; fps=fps, figsize=figsize, color=color, ulim=ulim)
    gif(anim, gifname, fps=fps)
    sleep(0.1)
    showimg("image/gif", gifname)
end

# 3次元プロット
function plot_sol3d(s::FMProblem, u, j; zlim=extrema(rmnans(u)), color=:RdYlBu, kwargs...)
    plot(; colorbar=false, gridalpha=0.3, tickfontsize=8, kwargs...)
    @views surface!(s.x, s.y, u[:,:,j]'; zlim=zlim, color=color)
end

function plot_sol3d25(s::FMProblem, u; zlim=extrema(rmnans(u)), color=:RdYlBu, kwargs...)
    P = plot_sol3d(s, u, 1; zlim=zlim, color=color, tickfontsize=4, kwargs...)
    PP = [P]
    for k in 2:25
        j = k*size(u,3) ÷ 25
        P = plot_sol3d(s, u, j; zlim=zlim, color=color, tickfontsize=4, kwargs...)
        push!(PP, P)
    end
    plot(PP..., layout=(5,5), size=(900, 800))
end

function animate_sol3d(s::FMProblem, u; zlim=extrema(rmnans(u)), fps=10,
        figsize=(400, 400), color=:RdYlBu, titlestr="",
        kwargs...
    )
    M = size(u,3)
    tskip = max(1, round(Int, 1/(3.33fps*s.Δx)))
    @show tskip, M
    anim = @animate for j in [fill(1,10); 1:tskip:M; fill(M, 10)]
        plot_sol3d(s, u, j; zlim=zlim, color=color)
        plot!(; size=figsize, kwargs...)
        if titlestr != ""
            title!(titlestr, titlefontsize=10)
        end
    end
    anim
end

function gif_sol3d(s::FMProblem, u; zlim=extrema(rmnans(u)), fps=10,
        figsize=(400, 400), color=:RdYlBu, titlestr="", gifname="test3d.gif", kwargs...
    )
    @time anim = animate_sol3d(s, u; zlim=zlim, fps=fps,
        figsize=figsize, color=color, titlestr=titlestr,
        kwargs...
    )
    gif(anim, gifname, fps=fps)
    sleep(0.1)
    showimg("image/gif", gifname)
end

# 波動方程式 (Neumann (3))
### x軸とt軸の刻み幅
###
### Δt は Δx の正の整数分の1にする. 以下では10分の1にしてある.
###
Δx, Δt = 1/100, 1/2000

### x軸とy軸とt軸の範囲
xspan = (-1,1)
yspan = (-1,1)
tspan = (0,10)

### 領域内で真になり, 領域外で偽になる函数
isinDomain(x,y) = true

### 境界条件 :Neumann または :Dirichlet
bc = :Neumann

### 高さ u での重力加速度
g(u) = 0.0

### 初期値
###
### F1(x, y) = u(t?,      x, y)
### F0(x, y) = u(t? - Δt, x, y)
###
F1(x,y) = exp(-((x+y+1)/√2)^2/(2*0.02^2))
F0(x,y) = F1(x + Δt/√2, y + Δt/√2)

### 以上のデータで決まる偏微分方程式の問題を作成
@time s6 = FMProblem(F1, F0, Δx, Δt, xspan, yspan, tspan, isinDomain, bc, g)

### その問題の数値解を計算
@time u6 = solve(s6)

### プロット
@time plot_sol25(s6, u6)
