---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.2'
      jupytext_version: 1.5.1
  kernelspec:
    display_name: Julia 1.5.2
    language: julia
    name: julia-1.5
---

# 最小二乗法の信頼区間と予測区間

黒木玄

2020-06-11, 2020-10-26

* [Jupyterノートブック版](https://nbviewer.jupyter.org/github/genkuroki/Statistics/blob/master/Least%20squares%20estimates.ipynb)
* [PDF版](https://genkuroki.github.io/documents/Statistics/Least%20squares%20estimates.pdf)

**以下の説明では直交射影が本質的な役割を果たす.**

**最小二乗法は直交射影の言い換えに過ぎない.**

$
\newcommand\eps{\varepsilon}
\newcommand\R{{\mathbb R}}
\newcommand\Normal{\operatorname{Normal}}
\newcommand\Image{\operatorname{Im}}
\newcommand\T{{\mathtt T}}
\newcommand\x{{\boldsymbol x}}
\newcommand\E{{\mathbb E}}
\newcommand\tr{\operatorname{tr}}
$


## 追記
ここでは数学的な記述は別にまとめておき、プログラムだけを載せてあります。
ipynbの形で全て確認したい場合は上のリンクから該当するノートブックを確認してください。


## 正規分布の仮定に注意せよ


### ライブラリ読み込み・関数定義

```julia
using LinearAlgebra, Distributions

norm2(x) = dot(x, x)

X_matrix(F, x) = hcat((f.(x) for f in F)...)
```

### 直交射影

```julia
function orthogonal_projection(X, y)
    # b̂ = inv(X'X)*X'y
    # b̂ = (X'X)\X'y # is equivalent to b̂ = inv(X'X)*X'y
    b̂ = X\y # equivalent to b̂ = (X'X)\X'y
    ŷ = X*b̂
    Ŝ² = norm2(y - ŷ)
    b̂, ŷ, Ŝ² 
end
```

### 線型回帰

```julia
function linear_regression(F, x, y)
    X = X_matrix(F, x)
    b̂, ŷ, Ŝ² = orthogonal_projection(X, y)
    n, r = size(X)
    û² = Ŝ²/(n - r)
    return b̂, √û², X
end
```

### 信頼区間

```julia
function confidence_interval_functions(F, X, b̂, û; α=0.05)
    n, r = size(X)
    t = quantile(TDist(n-r), 1-α/2) # 累積分布函数の逆函数
    f(xstar) = (φ -> φ(xstar)).(F)
    m(xstar) = f(xstar)'b̂
    # d(xstar) = û*√(f(xstar)'/(X'X)*f(xstar))
    # Modify X'X to √eps()*I + X'X in order to prevent Singular Exception.
    d(xstar) = û*√(f(xstar)'/(√eps()*I + X'X)*f(xstar))
    g₋(xstar) = m(xstar) - t*d(xstar)
    g₊(xstar) = m(xstar) + t*d(xstar)
    g₋, g₊
end
```

### 予測区間

```julia
function prediction_interval_functions(F, X, b̂, û; α=0.05)
    n, r = size(X)
    t = quantile(TDist(n-r), 1-α/2)
    f(xstar) = (φ -> φ(xstar)).(F)
    m(xstar) = f(xstar)'b̂
    # d(xstar) = û*√(1 + f(xstar)'/(X'X)*f(xstar))
    # Modify X'X to √eps()*I + X'X in order to prevent Singular Exception.
    d(xstar) = û*√(1 + f(xstar)'/(√eps()*I + X'X)*f(xstar))
    h₋(xstar) = m(xstar) - t*d(xstar)
    h₊(xstar) = m(xstar) + t*d(xstar)
    h₋, h₊
end
```

### ライブラリ読み込み・関数定義

```julia
using Plots
pyplot(fmt = :svg)

using Random: seed!

rd(x, d=3) = round(x; digits=d)

reg_func(F, b) = (x -> (φ -> φ(x)).(F)'b)
```

### 線型回帰のプロット関数

```julia
function plot_linear_regression(F, x, f_true;
        n = length(x),
        y = f_true.(x) + σ*randn(n),
        α = 0.05, 
        b = nothing, 
        σ = nothing, 
        xs = nothing,
        ylim = nothing
    )
    
    b̂, û, X = linear_regression(F, x, y)
    f_fit = reg_func(F, b̂)
    g₋, g₊ = confidence_interval_functions(F, X, b̂, û, α=α)
    h₋, h₊ = prediction_interval_functions(F, X, b̂, û, α=α)
    
    !isnothing(b) && @show b
    !isnothing(σ) && @show σ
    !isnothing(b) && !isnothing(σ) && println()
    @show b̂
    @show û
    
    isnothing(xs) && (xs = range(minimum(x), maximum(x), length=600))
    P = plot()
    isnothing(ylim) || plot!(ylim = ylim)
    plot!(xs, f_fit.(xs); label="fitting curve", color=:red, lw=2)
    plot!(xs, g₋.(xs); color=:red, ls=:dot, label="$(100(1-α))% conf. int.")
    plot!(xs, g₊.(xs); color=:red, ls=:dot, label="")
    plot!(xs, h₋.(xs); color=:red, ls=:dash, label="$(100(1-α))% pred. int.")
    plot!(xs, h₊.(xs); color=:red, ls=:dash, label="")
    plot!(xs, f_true.(xs); label="true curve", color=:blue, lw=1.4, alpha=0.85)
    scatter!(x, y; label="sample", color=:blue, msc=:blue, alpha=0.5)
end
```

### 計算その1
- $n=2^5$
- $x=[0,10]$
- $\sigma = 0.5$
- $f(x) = 1 + \sigma x$

```julia
n = 2^5
x = range(0, 10, length=n)
F = [one, identity]
b = [1, 0.5]
σ = 0.5
f_true = reg_func(F, b)

seed!(4649)
plot_linear_regression(F, x, f_true; xs=range(-1, 11; length=400))
```

### 計算その2
- $n=2^5$
- $x = [0, 10]$
- $f(x) = 5 - 2x + 0.2 x^2$
- $\sigma=0.5$
- $y = f(x) + \sigma \varepsilon$

```julia
n = 2^5
x = range(0, 10, length=n)
F = [one, identity, x->x^2]
b = [5, -2, 0.2]
σ = 0.5
f_true = reg_func(F, b)

seed!(4649)
y = f_true.(x) + σ*randn(n)

plot_linear_regression(F, x, f_true; b=b, σ=σ, y=y, xs=range(-0.5, 10.5, length=400), ylim=(-2, 8))
```

### 計算その3
- $G(x) = 1 + \sum_{k=1}^{10} x^k$

```julia
G = [one, identity, x->x^2, [x->x^k for k in 3:10]...]
plot_linear_regression(G, x, f_true; b=b, σ=σ, y=y, xs=range(-0.5, 10.5, length=400), ylim=(-2, 8))
```

### 計算その4
- $n=2^5$
- $x = [0, 2 \pi]$
- $f(x) = 3 + 2 \cos x + \sin x + 0 \cos 2x + \sin 2x$
- $\sigma = 0.5$
- $y = f(x) + \sigma \varepsilon$

```julia
n = 2^5
x = range(0, 2π, length=n)
F = [one, cos, sin, x->cos(2x), x->sin(2x)]
b = Float64[3; 2; 1; 0; 1]
σ = 0.5
f_true = reg_func(F, b)

seed!(4649)
y = f_true.(x) + σ*randn(n)

plot_linear_regression(F, x, f_true; b=b, σ=σ, y=y, xs=range(-2π, 4π, length=400), ylim=(-2, 11))
```

### 計算その5
- 別の$G$で計算

```julia
G = [one; cos; sin; x->cos(2x); x->sin(2x); [[x->cos(3x), x->sin(3x)] for k in 3:10]...]
plot_linear_regression(G, x, f_true; b=b, σ=σ, y=y, xs=range(-2π, 4π, length=400), ylim=(-2, 11))
```

### 計算その6
- $n=2^5$
- $x = [0,10]$
- $F$は正規分布
- $f=2 - 2x + 0.2 x^2$
- $\sigma = 0.5$
- $y=f(x) + \sigma \varepsilon$

```julia
n = 2^5
x = range(0, 10, length=n)
F = [x -> exp(-(x-μ)^2/(2*0.5^2)) for μ in range(minimum(x)-0.3, maximum(x)+0.3, length=n÷2)]
f_true = (x -> 2 - 2x + 0.2x^2)
y = f_true.(x) + 0.5randn(n)

plot_linear_regression(F, x, f_true; y=y, xs=range(-1, 11, length=400), ylim=(-6, 6))
```

### 計算その7
- $n=200$
- $f(x) = 0.12 + 0.23x + 0.34x^2 + 4.32 \sin x$
- $\sigma = 1.5$
- $y = f(x) + \sigma \varepsilon$

```julia
n = 200

b = [0.12, 0.23, 0.34, 4.32]
σ = 1.5
f_true = (x ->  b[1] + b[2]*x + b[3]*x^2 + b[4]*sin(x))

seed!(102)
x = rand(Uniform(-5, 5), n)
y = f_true.(x) + σ*randn(n)

F = [x -> x^k for k in 0:10]
plot_linear_regression(F, x, f_true; y=y, xs=range(-5.5, 5.5, length=400), ylim=(-10, 24))
```

### 計算その8
- $n=16$
- $\sigma = 1.5$
- $f(x) = 0.12 + 0.23x + 0.34 x^2 + 4.32 \sin x$
- $y = f(x) + \sigma \varepsilon(n)$
- $F(x) = \sum_{k=0}^{10} x^k$

```julia
n = 16

b = [0.12, 0.23, 0.34, 4.32]
σ = 1.5
f_true = (x ->  b[1] + b[2]*x + b[3]*x^2 + b[4]*sin(x))

seed!(102)
x = rand(Uniform(-5, 5), n)
y = f_true.(x) + σ*randn(n)

F = [x -> x^k for k in 0:10]
plot_linear_regression(F, x, f_true; y=y, xs=range(-5.5, 5.5, length=400), ylim=(-10, 24))
```
