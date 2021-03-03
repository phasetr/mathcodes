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

# 尤度函数のプロット

黒木玄

2019-09-17


## 追記
- [黒木さんオリジナル](https://github.com/genkuroki/Statistics/blob/master/likelihood%20functions%20of%20mixture%20normal%20distributions.ipynb), [nbviewer](https://nbviewer.jupyter.org/github/genkuroki/Statistics/blob/master/likelihood%20functions%20of%20mixture%20normal%20distributions.ipynb)

上記リンク先のファイルにコメントをつけています。

このノートブックでは「二つの山」を持つパラメータつきの確率分布$p(x|a,b)$を考え、次のようにふるまうとします。

- 二つの山は$b$が大きいとき（**正則なとき**）は分離して見える。
- 二つの山は$b$が小さくなるとき（**特異なとき**）山が合体して見える。

このとき$b$の大小で尤度関数（のヒートマップ）がどう見えるかを具体的に調べてみます。
特に次の二つの場合を考えます。

- パラメータ$(a_i, b_i)$ ($i=1,2,3,4$) に対してそれぞれ**サンプル数$n$を変えた時**にどうふるまうか？
- パラメータ$(a_i, b_i)$ ($i=1,2,3,4$) に対して**サンプル数を固定**して乱数のシードを変えたときどう振る舞うか？

サンプル$X$は上記特定のパラメータで作り、同じ関数$p(x|a,b)$から作った$L(a,b)=p(X|a,b)$でヒートマップを作っています。

<!-- #region toc=true -->
## 目次
- [確率分布のパラメータを変えつつ、$n$を変えて実験](#確率分布のパラメータを変えつつ、$n$を変えて実験)
    - [$(a_0,-b_0)-=-(0.5,-0.1)$-near-singular](#$(a_0,-b_0)-=-(0.5,-0.1)$-near-singular)
    - [$(a_1,-b_1)-=-(0.5,-0.5)$-regular](#$(a_1,-b_1)-=-(0.5,-0.5)$-regular)
    - [$(a_2,-b_2)-=-(0.5,-0.0)$-singular](#$(a_2,-b_2)-=-(0.5,-0.0)$-singular)
    - [$(a_3,-b_3)-=-(0.5,-4.0)$-very-regular](#$(a_3,-b_3)-=-(0.5,-4.0)$-very-regular)
- [n-=-256-で固定しつつ、乱数のシードを変えて実験](#n-=-256-で固定しつつ、乱数のシードを変えて実験)
    - [n-=-256,-$(a_0,-b_0)-=-(0.5,-0.1)$,-near-singular](#n-=-256,-$(a_0,-b_0)-=-(0.5,-0.1)$,-near-singular)
    - [n-=-256,-$(a_1,-b_1)-=-(0.5,-0.5)$,-regular](#n-=-256,-$(a_1,-b_1)-=-(0.5,-0.5)$,-regular)
    - [n-=-256-$(a_2,-b_2)-=-(0.5,-0.0)$,-singularr](#n-=-256,-$(a_2,-b_2)-=-(0.5,-0.0)$,-singular)
    - [n-=-256,-$(a_3,-b_3)-=-(0.5,-4.0)$,-very-regular](#n-=-256,-$(a_3,-b_3)-=-(0.5,-4.0)$,-very-regular)
<!-- #endregion -->

## 準備


### ライブラリ読み込み

```julia
using Distributions
using Plots
gr(size=(400, 250), titlefontsize=10, fmt=:png)
using Random
```

### 定数設定

```julia
const s = 4649
Random.seed!(s)

# あとで n 固定でテストするときの変数
const loopnum = 16
const rands = rand(UInt, loopnum)
```

### 関数定義
\begin{align}
\mathrm{logp}(x | a,b) &=
- \frac{x^2}{2} - \log \sqrt{2 \pi} + \log 
\left(
(1-a) + a \exp \left[ bx - \frac{b^2}{2} \right]
\right), \\
p(x | a, b) &=
\exp \mathrm{logp}(x | a, b) \\ 
&=
\frac{1}{\sqrt{2\pi}}
e^{\frac{x^2}{2}} \left(
(1-a) + a \exp \left[ bx - \frac{b^2}{2} \right] 
\right).
\end{align}


#### 注意
あとでヒストグラムを見れば具体的にわかるように、
パラメータのうち$b$が小さいとき$p$は「二つの山」が区別しにくくなります。
この状況をもって$b$が小さい$p(x|a,b)$を**特異的**と呼び、そうでないときを正則と呼びます。
このノートブックのテーマは正則な場合と特異的な場合で尤度関数の振る舞いがどう変わるか調べることです。

```julia
# 上のセル参照
logp(x, a, b) = -x^2/2 - log(√(2π)) + log((1-a) + a*exp(b*x - b^2/2))
# サンプル生成に使う確率分布
p(x, a, b) = exp(logp(x, a, b))
# 対数尤度
loglik(X, a, b) = sum(logp(x, a, b) for x in X)
# 正規分布の混合モデル
mixnormal(a, b) = MixtureModel([Normal(), Normal(b, 1.0)], [1-a, a])
```

### 尤度関数のプロット用関数

```julia
function plot_lik(a₀, b₀, n; seed=s, alim=(0, 1), blim=(-1, 2), kwargs...)
    Random.seed!(seed)
    
    # 真の分布
    dist_true = mixnormal(a₀, b₀)
    # サンプル生成
    X = rand(dist_true, n)
    
    # 対数尤度関数の定義
    L(a, b) = loglik(X, a, b)

    # ヒートマップ用の範囲指定
    a = range(alim..., length=100)
    b = range(blim..., length=200)
    # 尤度の計算
    @time z = L.(a, b')
    zmax = maximum(z)
    idx = findmax(z)[2]
    w = @. exp(z - zmax) # is very important!

    plot!(; title="\$n = $n,\\quad (a_0, b_0) = ($(a₀), $(b₀))\$")
    plot!(; xlabel="\$b\$", ylabel="\$a\$")
    heatmap!(b, a, w; colorbar=false)
    # 真の値
    scatter!([b₀], [a₀]; markersize=4, markerstrokewidth=0, color=:cyan, label="true")
    # 最尤法：星型マーカー
    scatter!([b[idx[2]]], [a[idx[1]]]; markersize=5, markershape=:star, color=:lightgreen, label="MLE")
    plot!(; xlim=extrema(b), ylim=extrema(a))
    plot!(; kwargs...)
end
```

### 正則な場合：$(a,b)=(0.3, 5)$の混合正規分布のヒストグラムと分布

```julia
(a, b) = (0.3, 5)
histogram(rand(mixnormal(a, b), 1000), bin=20, normed=true, legend=false, alpha=0.5)
plot!(x->p(x, a, b), lw=2, ls=:dash, size=(400, 250))
```

### 特異性が強い場合：$(a,b)=(0.3, 0.1)$の混合正規分布のヒストグラムとサンプル

```julia
(a, b) = (0.3, 0.1)
histogram(rand(mixnormal(a, b), 1000), bin=20, normed=true, legend=false, alpha=0.5)
plot!(x->p(x, a, b), lw=2, ls=:dash, size=(400, 250))
```

### 特異的な場合：$(a,b)=(0.3, 0.0)$の混合正規分布のヒストグラムとサンプル

```julia
(a, b) = (0.3, 0.0)
histogram(rand(mixnormal(a, b), 1000), bin=20, normed=true, legend=false, alpha=0.5)
plot!(x->p(x, a, b), lw=2, ls=:dash, size=(400, 250))
```

## 確率分布のパラメータを変えつつ、$n$を変えて実験
- [目次](#目次)


### $(a_0, b_0) = (0.5, 0.1)$ near singular
- [目次](#目次)


#### まとめ
- $n$が増えるとヒートマップの範囲は細長くなっていく
- （ここで確認した範囲では）最後までヒートマップの裾野が広い
- 最尤法の位置（星型マーカー）もあまり精度がよくない

```julia
a₀, b₀ = 0.5, 0.1
```

#### 尤度関数のプロット: $n=2^4$

```julia
plot_lik(a₀, b₀, 2^4)
```

#### 尤度関数のプロット: $n=2^6$

```julia
plot_lik(a₀, b₀, 2^6)
```

#### 尤度関数のプロット: $n=2^8$

```julia
plot_lik(a₀, b₀, 2^8)
```

#### 尤度関数のプロット: $n=2^{10}$

```julia
plot_lik(a₀, b₀, 2^10)
```

#### 尤度関数のプロット: $n=2^{12}$

```julia
plot_lik(a₀, b₀, 2^12)
```

#### 尤度関数のプロット: $n=2^{14}$

```julia
plot_lik(a₀, b₀, 2^14)
```

### $(a_1, b_1) = (0.5, 0.5)$ regular
- [目次](#目次)


#### まとめ
- $n$が増えるとヒートマップの範囲は細長くなっていく
- （ここで確認した範囲では）ヒートマップの裾野も狭くなる
- 最尤法の位置（星型マーカー）の精度もよい

```julia
a₁, b₁ = 0.5, 0.5
```

#### 尤度関数のプロット: $n=2^{4}$

```julia
plot_lik(a₁, b₁, 2^4)
```

#### 尤度関数のプロット: $n=2^{6}$

```julia
plot_lik(a₁, b₁, 2^6)
```

#### 尤度関数のプロット: $n=2^{8}$

```julia
plot_lik(a₁, b₁, 2^8)
```

#### 尤度関数のプロット: $n=2^{10}$

```julia
plot_lik(a₁, b₁, 2^10)
```

#### 尤度関数のプロット: $n=2^{12}$

```julia
plot_lik(a₁, b₁, 2^12)
```

#### 尤度関数のプロット: $n=2^{14}$

```julia
plot_lik(a₁, b₁, 2^14)
```

### $(a_2, b_2) = (0.5, 0.0)$ singular
- [目次](#目次)


#### まとめ
- $n$が増えるとヒートマップの範囲は細長くなっていく
- （ここで確認した範囲では）ヒートマップの裾野は広い
- 最尤法の位置（星型マーカー）の精度はよくない
- $a=0$で$b$に関する広がりがある

```julia
a₂, b₂ = 0.5, 0.0
```

#### 尤度関数のプロット: $n=2^{4}$

```julia
plot_lik(a₂, b₂, 2^4; blim=(-1.5, 1.5))
```

#### 尤度関数のプロット: $n=2^{6}$

```julia
plot_lik(a₂, b₂, 2^6; blim=(-1.5, 1.5))
```

#### 尤度関数のプロット: $n=2^{8}$

```julia
plot_lik(a₂, b₂, 2^8; blim=(-1.5, 1.5))
```

#### 尤度関数のプロット: $n=2^{10}$

```julia
plot_lik(a₂, b₂, 2^10; blim=(-1.5, 1.5))
```

#### 尤度関数のプロット: $n=2^{12}$

```julia
plot_lik(a₂, b₂, 2^12; blim=(-1.5, 1.5))
```

#### 尤度関数のプロット: $n=2^{14}$

```julia
plot_lik(a₂, b₂, 2^14; blim=(-1.5, 1.5))
```

### $(a_3, b_3) = (0.5, 4.0)$ very regular
- [目次](#目次)


#### まとめ
- $n$が増えるとヒートマップの範囲は同心（楕）円上に小さくなる
- （ここで確認した範囲では）ヒートマップの裾野も狭くなる
- 最尤法の位置（星型マーカー）の精度もよい

```julia
a₃, b₃ = 0.5, 4.0
```

#### 尤度関数のプロット: $n=2^{4}$

```julia
plot_lik(a₃, b₃, 2^4; alim=(0, 1), blim=(1.5, 4.5))
```

#### 尤度関数のプロット: $n=2^{6}$

```julia
plot_lik(a₃, b₃, 2^6; alim=(0.3, 0.7), blim=(3.3, 4.7))
```

#### 尤度関数のプロット: $n=2^{8}$

```julia
plot_lik(a₃, b₃, 2^8; alim=(0.3, 0.7), blim=(3.3, 4.7))
```

#### 尤度関数のプロット: $n=2^{10}$

```julia
plot_lik(a₃, b₃, 2^10; alim=(0.3, 0.7), blim=(3.3, 4.7))
```

#### 尤度関数のプロット: $n=2^{12}$

```julia
plot_lik(a₃, b₃, 2^12; alim=(0.3, 0.7), blim=(3.3, 4.7))
```

#### 尤度関数のプロット: $n=2^{14}$

```julia
plot_lik(a₃, b₃, 2^14; alim=(0.3, 0.7), blim=(3.3, 4.7))
```

## n = 256 で固定しつつ、乱数のシードを変えて実験
- [目次](#目次)


### n = 256, $(a_0, b_0) = (0.5, 0.1)$, near singular
- [目次](#目次)


#### まとめ
- $a=0$近くで$b$に関する左右の広がりが大きい

```julia
for i in 1:loopnum
    plot_lik(a₀, b₀, 2^8; seed=rands[i]) |> display
end
```

### n = 256, $(a_1, b_1) = (0.5, 0.5)$, regular
- [目次](#目次)


#### まとめ
- 三日月型で$b$が小さい方には振れない

```julia
for i in 1:16
    plot_lik(a₁, b₁, 2^8; seed=rands[i]) |> display
end
```

### n = 256, $(a_2, b_2) = (0.5, 0.0)$, singular
- [目次](#目次)


#### まとめ
- $a=0$ の近くでの$b$方向の広がりが大きい
- 最尤法の精度はひどい

```julia
for i in 1:loopnum
    plot_lik(a₂, b₂, 2^8; blim=(-2, 2), seed=rands[i]) |> display
end
```

### n = 256, $(a_3, b_3) = (0.5, 4.0)$, very regular
- [目次](#目次)


#### まとめ
- ヒートマップの広がり自体は同心円状
- 最尤法の値（円の中心）自体は真の値に近いことがある（多少ずれていることもある）
    - ほかのグラフと比較するときは軸の範囲にも注意すること！

```julia
for i in 1:loopnum
    plot_lik(a₃, b₃, 2^8; alim=(0.3, 0.7), blim=(3.5, 4.5), seed=rands[i]) |> display
end
```
