# # 5日目 統計力学
# Julia 1.7で本書を再現するコード：
# Julia 1.7以降の場合、乱数の仕様が1.6と変化したため、本書と同じ結果を再現するには乱数を指定する必要があります。
# そのため、本書では指定していない、```rng```という変数が引数に入っています。

# ## ログ出力の設定

import Logging
Logging.disable_logging(Logging.Info)

# ## パッケージ読み込み・シード固定

using IJulia
using Plots
using Random
using Measures
myseed = 4096
Random.seed!(myseed)

# （本のなかで）ここまで主に微分方程式を解いていた。
# 線型の方程式で、数値計算用にいろいろやっていると行列の話になってくる。
#
# 統計力学：確率論を何らかの形で使う。
# 確率の話、乱数使って色々やろう。
# 乱数を使ったシミュレーション、イジング模型による相転移。
# モンテカルロ法。
#
# 物理の話。
# 解析力学のフォーミュレーション、ラグランジアン・ハミルトニアン。
# ハミルトニアン：エネルギーを表す関数。（古典論。）
# 統計力学：ハミルトニアンをいろいろ調べましょう。
# （HMC：ハミルトニアン モンテカルロ法。）
#
# 一般に統計力学は多体系：$N$=アボガドロ定数のオーダー。
# まともに解けない。
# 何らかの意味で力づくで解く。
# 理論物理として有限の$N$ではなく$N \to \infty$：余計な部分が見えなくなって綺麗になる。
# 数値的に解く。
# $N$が大きいときの議論が大事。
# もちろん$N$が大きいと手計算で処理しきれなくなるので、ここで数値計算が大事。

# ## 5.1 ヒストグラム表示
# 原子や分子が数個しかない場合でいろいろ遊んでみよう。
#
# 時々衝突する、エネルギーのやり取り。
# エネルギーのやり取りをして何が起きるか？
#
# 「大沢流手づくり統計力学」：ここで計算やって遊んでいる。
# エネルギー：チップ、粒子：人。
#
# ルールを平等にしても、各人の（最終的な）チップに大きな不均衡が現れる。
# ガウシアン分布。
# 気体分子運動論の速度分布。
# ルールの平等：等重率の仮定。

# ### 5.1.1 基本ルール
# - 6人組のグループを$S$個つくる：$6S$人。それぞれのグループにサイコロ二つとチップ$M$枚を渡す。グループ内で1-6までの数字を各人に割り当てる
# - サイコロを一つ振って、出た目の人にチップを分配。繰り返してグループのチップを全部分配。
# - それぞれが何枚持っているか記録、$S$個の結果を合算してグラフを描く。
# - サイコロを二つ振って、一つ目の目の人が二目の目の人にチップを渡す。チップがなかったらチップを渡さない。（借金はなし）
# - （4）を$N$回繰り返す。
# - それぞれが何枚もっているか記録して、$S$個の結果を合算してグラフに描く

# ### 5.1.2 ルールの実装

# #### ルール2

function make_initial(numpeople, numchip, seed=myseed)
    # Random.seed!(seed)
    boxes = zeros(Int64, numpeople)
    for targetbox in rand(1:numpeople, numchip)
        boxes[targetbox] += 1
    end
    return boxes
end

# #### ルール4

function giveandtake(oldboxes, seed=myseed)
    # Random.seed!(seed)
    newboxes = copy(oldboxes)
    numpeople = length(oldboxes)
    A = rand(1:numpeople) # 渡す人
    B = rand(1:numpeople) # もらう人
    while B == A # 渡す人ともらう人は別にしたい
        B = rand(1:numpeople)
    end
    if newboxes[A] > 0
        newboxes[A] -= 1
        newboxes[B] += 1
    end
    return newboxes
end

# ### 5.1.3 初期分布のヒストグラム

function test2(seed=myseed)
    # Random.seed!(seed)
    numpeople = 6
    numballs = 30
    numgroups = 100
    totalpeople = numpeople * numgroups
    totalboxes = zeros(Int64, numgroups, numpeople)

    for i = 1:numgroups
        totalboxes[i, :] = make_initial(numpeople, numballs)
    end

    hist = histogram(totalboxes[:],
        nbins=-0.5:1:numballs,
        label="initial",
        ylims=(0, totalpeople * 0.3))
    savefig("P154_hist.png")
    display(hist)
end

# テスト実行

test2()

# ### 5.1.4 ゲーム後のヒストグラム

function test3(seed=myseed)
    # Random.seed!(seed)
    numpeople = 6
    numballs = 30
    numgroups = 100
    totalpeople = numpeople * numgroups
    totalboxes = zeros(Int64, numgroups, numpeople)

    for i = 1:numgroups
        totalboxes[i, :] = make_initial(numpeople, numballs)
    end

    numtotal = 300
    for itrj = 1:numtotal
        for i = 1:numgroups
            totalboxes[i, :] = giveandtake(totalboxes[i, :])
        end
    end
    hist = histogram(totalboxes[:],
        nbins=-0.5:1:numballs,
        label="$numtotal",
        ylims=(0, totalpeople * 0.3))
    savefig("P155_hist_300.png")
    display(hist)
end

# テスト実行

test3()

# #### 300回に至るまでのチップの分布の変化：GIF

function test3()
    numpeople = 6
    numballs = 30
    numgroups = 100
    totalpeople = numpeople * numgroups
    totalboxes = zeros(Int64, numgroups, numpeople)

    for i = 1:numgroups
        totalboxes[i, :] = make_initial(numpeople, numballs)
    end

    numtotal = 300
    anim = Animation()
    @gif for itrj = 1:numtotal
        println("$itrj-th")
        for i = 1:numgroups
            totalboxes[i, :] = giveandtake(totalboxes[i, :])
        end
        plt = histogram(totalboxes[:], nbins=-0.5:1:numballs, label="$itrj", ylims=(0, totalpeople * 0.3))
        frame(anim, plt)
    end
    gif(anim, "P156_test3_histplot.gif", fps=30)
    gif(anim, fps=50)
    display(gif)
    hist = histogram(totalboxes[:], nbins=-0.5:1:numballs, label="$numtotal", ylims=(0, totalpeople * 0.3))
    savefig("P156_test3_hist_300.png")
    display(hist)
end

# テスト実行

test3()

# ### TODO 5.1.5 ゲームの中の各人のチップ枚数
# - 一回金持ちになった人はずっと金持ちでいられるか？
# - 時系列で枚数をプロットしてみよう
#
# `numgroups=1`で確認する.

function test4()
    numpeople = 6
    numballs = 30
    # numgroups = 1
    numtotal = 600
    timedepboxes = zeros(Int64, numtotal, numpeople) # 時系列データを格納

    timedepboxes[1, :] = make_initial(numpeople, numballs) # init dist
    for itrj = 2:numtotal
        timedepboxes[itrj, :] = giveandtake(timedepboxes[itrj-1, :])
    end
    for i = 1:numpeople
        plot!(timedepboxes[:, i], label="$i")
    end
    savefig("P157_test4_history.png")
    # TODO Jupyterで画像を表示させたい
    # display()
end

# テスト実行

test4()

# ### 5.1.6 ボルツマン分布と等重律

function make_states!(states, allstates, numpeople, numballs, i)
    if i <= length(states)
        for j = 0:numballs
            states[i] = j
            make_states!(states, allstates, numpeople - sum(states), numballs, i + 1)
        end
    else
        if sum(states) == numballs
            push!(allstates, copy(states))
        end
    end
end

# テスト実行

function test5()
    numpeople = 4
    numballs = 4
    states = zeros(Int64, numpeople)
    allstates = []
    make_states!(states, allstates, numpeople, numballs, 1)
    println(allstates)
    println("Total number of states: ", length(allstates))
end

# テスト実行

test5()

# #### P.160 `find_state()`

function find_state_id(states, allstates)
    id = findfirst(x -> x == states, allstates)
    return id
end

# テスト実行用関数

function test6()
    numpeople = 4
    numballs = 4
    states = zeros(Int64, numpeople)
    allstates = []
    make_states!(states, allstates, numpeople, numballs, 1)
    numtotalstates = length(allstates)
    numstates = zeros(Int64, numtotalstates)
    numgroups = 100

    allstatesname = string.(allstates)
    # totalpeople = numpeople * numgroups

    totalboxes = zeros(Int64, numgroups, numpeople)
    for i = 1:numgroups
        totalboxes[i, :] = make_initial(numpeople, numballs)
        id = find_state_id(totalboxes[i, :], allstates)
        numstates[id] += 1
    end

    plot(numstates, xticks=(1:1:numtotalstates, allstatesname), xrotation=45, xtickfontsize=6, markershape=:circle, margin=15mm, label="initial", ylims=(0, maximum(numstates) + 1))
    savefig("P160_allstateinit.png")

    numtotal = 300
    anim = Animation()
    for itrj = 1:numtotal
        println("$itrj-th")
        for i = 1:numgroups
            totalboxes[i, :] = giveandtake(totalboxes[i, :])
            id = find_state_id(totalboxes[i, :], allstates)
            numstates[id] += 1
        end
        plt = plot(numstates, xticks=(1:1:numtotalstates, allstatesname), xrotation=45, xtickfontsize=6, markershape=:circle, margin=15mm, label="$itrj-th", ylims=(0, maximum(numstates) + 1))
        frame(anim, plt)
    end
    gif(anim, "P160_allstates.gif", fps=30)
end

# テスト実行

test6()

# ## 5.2 イジング模型のモンテカルロシミュレーション：可視化と動画作成
# マルコフ連鎖モンテカルロ法（Markov chain Monte Carlo method, MCMC法）。
#
# 乱数生成。
# 物理で出てくる乱数は高次元空間でいろいろやる。
# 問題が一次元であっても、確率分布が正規分布ではない場合、直接乱数発生させるのが難しい。
# こういう場合にMCMC。
#
# マルコフ連鎖モンテカルロ法：あるルールに従って次々に乱数を生成する。
# このルールをうまく設定すると自分の扱いたい確率分布に従った乱数が生成できる。
# マルコフ過程：確率過程、直前の値にだけ依存してそれより過去の状態には依存しない。

# ### 5.2.1 イジング模型
# 古典スピン系（スピンは本来量子力学に起源がある対象）。
# 磁性の議論で一番なカンタンなモデル。
#
# 物質が磁石になる場合、物質中の電子のスピンが一方向に揃う。
# イジング模型は電子を忘れて、格子上（固体、原子の配置）にスピンが置いてあると思う。
# スピンの値は$\pm 1$を取ることにして、設定したハミルトニアンに応じた相互作用をする。
# ある温度を設定して、ある温度以下（転移温度）なら磁石になってほしい。
# ある温度以上なら磁石にならない：こういう現象を再現したい。
#
# \begin{align}
# H = - J \sum_{\langle ij \rangle} \sigma_i \sigma_j - h \sum_{i} \sigma_i
# \end{align}

# ##### 補足：ヒステリシスと順序交換
# 極限の順序交換は一般に不可能。
# \begin{align}
# \lim_{h \to 0} \lim_{L \to \infty} \omega_{L,h}(M) \neq
# \lim_{L \to \infty} \lim_{h \to 0} \omega_{L,h}(M)
# \end{align}

# #### $h=0$（外部磁場なし）、$J>0$の場合
# 絶対零度の場合はシンプルにエネルギーだけ考えればよく、最低エネルギーを取るのは全てのスピンの向きが揃っている場合で、$H=-JN$、$N$の格子点の数。
#
# 有限温度の場合が問題。
# 有限温度の場合は熱力学的に考える必要があって、特に自由エネルギーの最小化を考える。
# ヘルムホルツの自由エネルギーは$F = U - TS$で、$U$は内部エネルギー、$T$が絶対温度、$S$がエントロピー。

# 有限温度の系での物理量$\langle A \rangle$での期待値は
# \begin{align}
# \langle A \rangle =
# \frac{1}{Z} \sum_C \exp \left[- \frac{H(C)}{k_BT} A(C) \right]
# \end{align}
# $H(C)$はハミルトニアンで、$C$がスピン配位。

# $Z$は分配関数で
# \begin{align}
# Z = \sum_C \exp \left(- \frac{H(C)}{k_BT} \right)
# \end{align}
# $\exp (- \frac{H(C)}{k_BT})$はBoltzmann因子。

# Boltzmann因子は指数関数：$H(C)$が最小の寄与が最も大きく、$H(C)$が少し大きくなると指数関数の値が一気に小さくなるので、和に寄与しない。
# 物理量や分配関数の計算での「被積分関数」は高次元で局在している。
# 局在をうまく引っ張れるのが重みつきのモンテカルロ法が有効な手段。
#
# この確率分布$\exp (- \frac{H(C)}{k_BT}) / Z$は正規分布ではない。

# プログラムで計算しようと思うと、正規分布と一様分布くらい。
# あとは特殊関数をうまく使った分布はあるが、いまは$H$と$C$に依存する一般的にはよくわからない分布で、計算するのが難しい。
# この状況下での乱数生成がいい感じにできるMCMC法がありがたい。
#
# イジング模型のMCMCではあるスピン配位$C_1$が与えられたとき、何らかの方法で配位を変更、$C_2$を出す。
# これを繰り返して配位の列をつくる。
# スピンの配位ごとに物理量$A(C_i)$を計算して、統計力学的な物理量の意味で足し上げると「物理量の期待値のサンプル」が得られる。

# ### 5.2.2 2次元のイジング模型のMCMC法
# 二次元のイジング模型のコードを書いて遊ぼう。
# 模型としては$L_x \times L_y$の正方格子の二次元イジング模型を考える。
# 格子点の総数は$N = L_x L_y$個。
# 各格子点に$\sigma_i = \pm 1$の古典スピンが乗っている。
# $x,y$の両方向に周期境界条件を付ける。

# ある格子点のインデックスを$i = (i_x, i_y)$とすると、格子点$i$と相互作用する最近接格子点は次の四点。
# \begin{align}
# d_1 = (i_{x}+1,i_y), d_2 = (i_x-1,i_y), d_3 = (i_x,i_y+1), d_4 = (i_x,i_y-1).
# \end{align}
# これを使うとイジング模型のハミルトニアンは
# \begin{align}
# H(C) = - \frac{J}{2} \sum_i \sum_{l=1}^4 \sigma_i \sigma_{i+d_l} - h \sum_{i} \sigma_i
# \end{align}
# と書ける。

# ここで$\sigma_i \sigma_j = \frac{1}{2} (\sigma_i \sigma_j + \sigma_j \sigma_i)$から来る係数$1/2$が出ている。
# さらに$S_i = \sum_{l=1}^4 \sigma_{i+d_l}$とすれば
# \begin{align}
# H(C) = - \frac{J}{2} \sum_i \sum_{l=1}^4 \sigma_i S_i - h \sum_{i} \sigma_i
# \end{align}
# と書ける。
# つまりある格子点$i$の最近接格子点に対するスピンの和$S_i$がわかれば全エネルギーが計算できる。

# 参考：$S_i$を適当な「平均」で近似したのが平均場近似。

# ### MCMCの説明
# 二次元イジング模型をMCMCでのシミュレーションは、
# あるスピン配位$C$の実現する確率が
# \begin{align}
# P(C) &= \frac{1}{Z} \exp \left[ - \frac{H(C)}{k_BT} \right], \\
# Z &= \sum_{C} \exp \left[ -  \frac{H(C)}{k_BT} \right].
# \end{align}
# になるようなMCMC。
# イジング模型は磁性を持つかが重要で、磁化$M(C) = \frac{1}{N} \sum_{i} \sigma_i$の絶対値の期待値
# \begin{align}
# \langle |M| \rangle = \sum_C P(C) |M(C)|
# \end{align}
# を計算する。
# 磁化には温度依存性がある。

# #### メトロポリス法
# あるスピン配位$C$に対して確率$1/N$である格子点$i$の上のスピンをフリップさせた配位$C'$の遷移確率を考える。
# この配位の採択率は
# \begin{align}
# A(C \to C') = \min \left( \frac{P(C')}{P(C)} \right)
# \end{align}
# で、これは
# \begin{align}
# A(C \to C') &= \min \left( 1, \exp -\left( - \frac{\Delta E(C,i)}{k_BT} \right) \right) \\
# \Delta E(C) &= H(C') - H(C)
# \end{align}
# と書ける。
# このエネルギー差は二次元イジング模型では
# \begin{align}
# \Delta E(C,i) = 2 J \sigma_i S_i + 2h \sigma_i
# \end{align}
# と書ける。
# メトロポリス法では$\Delta E < 0$、つまりスピンをフリップしてエネルギーが下がった場合は確率1で採択され、
# そうでない場合は$e^{- \Delta E / k_BT}$で採択される。

# #### 熱浴法
# 遷移確率は条件付き確率で計算する。
# ランダムに選んだ格子点$i$でのスピンを$\sigma_i$、
# それ以外の格子点でのスピンをまとめて$\sigma(C_{k,i/})$とし、
# このときスピン配位を$C_k = (\sigma_i, \sigma(C_{k,i/}))$と書く。
# 格子点$i$以外のスピン配位が$\sigma(C_{k,i/})$の時、
# 格子点$i$でスピン$\sigma_i$が選ばれる条件付き確率を$P(\sigma_i | \sigma(C_{k,i/}))$とすれば、スピン配位$C_k=(\sigma_i, \sigma(C_{k,i/}))$から$C_k = (+1, \sigma(C_{k,i/}))$に遷移する確率は
# \begin{align}
# T_{C_k \to C'_k} = P(+1 | \sigma(C_{k,i/}))
# \end{align}
# と書ける。条件付き確率は
# \begin{align}
# P(+1 | \sigma(C_{k,i/})) = \frac{P(+1 | \sigma(C_{k,i/}))}{P(\sigma (C_{k,i/}))}
# \end{align}
# と書ける。
# ここで$P(\sigma(C_{k,i/}))$はスピン配位$\sigma (C_{k,i/})$が実現する確率で、
# 格子点$i$のスピンの向き以外は任意だから
# \begin{align}
# P(\sigma(C_{k,i/})) = \sum_{\sigma_i = \pm 1} P((\sigma_i, \sigma(C_{k,i/})))
# \end{align}
# と書ける。
# 以上から
# \begin{align}
# P(+1 | \sigma(C_{k,i/}))
# &=
# \frac{P(+1, \sigma(C_{k,i/}))}{P(+1, \sigma(C_{k,i/})) + P(-1, \sigma(C_{k,i/}))}
# \end{align}
# で、
# \begin{align}
# \frac{P((-1, \sigma(C_{k,i/})))}{P(+1, \sigma(C_{k,i/}))}
# =
# \exp \left( - \frac{\Delta E(C, i; +1 \to -1)}{k_BT} \right).
# \end{align}
# ここで$\Delta E(C, i; +1 \to -1)$は格子点$i$のスピンが$+1$から$-1$になった時のエネルギー差で、
# \begin{align}
# \Delta E(C,i; +1 \to -1)
# =
# 2JS_i + 2h
# =
# \Delta E(C,i) \sigma_i
# \end{align}
# と書ける。これを使って遷移確率$T$の表式を静止すれば、
# 熱浴法の格子点$i$のスピンが$+1$になる確率は
# \begin{align}
# T_{C_k \to C'_k}
# =
# \frac{1}{1 + \exp \left( - \frac{\Delta E(C,i; +1 \to -1)}{k_BT} \right)}
# \end{align}
# と書ける。
# 一様乱数を振って、この値以下ならスピン$+1$に、
# そうでなければ$-1$にすれば熱浴法によるMCMCが実行できる。

# ### 5.2.3 マルコフ連鎖モンテカルロシミュレーションの流れ
# スピン配位$C$の関数として磁化を$M(C) = \sum_{i} \frac{1}{N} \sigma_i$とする。
#
# 0. 目的：ある温度$T$での磁化の期待値$\langle |M| \rangle$とエネルギーの期待値を計算する。
# 1. 初期化：$L_x \times L_y$の格子点を用意し、スピンの初期配位$C_0$を設定する。
# 2. 配位$C_k$で一つの格子点$i$を適当に選ぶ。
# 3. メトロポリス法か熱浴法で格子点$i$のスピンを変える。この配位を$C_{k+1}$とする。
# 4. 熱化：上記2-3を$N_{\mathrm{thermal}}$回繰り返してスピン配位の初期配位依存性を消す。
# 5. 期待値計算：2-3を$N_{\mathrm{MC}}$回繰り返す。適当な間隔（$N_M$回に一回）で$|M(C_k)|$と$E(C_k)$を計算して期待値を求める。

# ### 5.2.4 二次元イジング模型のコーディング
# まず、スピン配位だけから決まる初期化と測定の関数を作る。

# #### P.168 スピン配位の初期化

function initialize_spins(Lx, Ly, rng)
    return rand(rng, [-1, 1], Lx, Ly)
end

# #### P.168 磁化の測定
# 単純に足し上げるだけ。

function measure_Mz(Ck)
    return sum(Ck)
end

# #### P.169 エネルギーの計算
# `calc_Si`は最近接格子点のスピン和で、すぐ次で定義する。

function measure_energy(Ck, J, h, Lx, Ly)
    energy = 0
    for iy = 1:Ly
        for ix = 1:Lx
            Si = calc_Si(ix, iy, Lx, Ly, Ck)
            σi = Ck[ix, iy]
            energy += -(J / 2) * σi * Si - h * σi
        end
    end
    return energy
end

# #### P.168 $S_i$の測定
# $i=(i_x,i_y)$の周囲のスピンの値を計算する。
# $C_k$がスピン配位で、格子点上のスピンの値を持っている。

function calc_Si(ix, iy, Lx, Ly, Ck)
    jx = ix + 1
    if jx > Lx
        jx -= Lx
    end
    jy = iy
    Si = Ck[jx, jy]

    jx = ix - 1
    if jx < 1
        jx += Lx
    end
    jy = iy
    Si += Ck[jx, jy]

    jy = iy + 1
    if jy > Ly
        jy -= Ly
    end
    jx = ix
    Si += Ck[jx, jy]

    jy = iy - 1
    if jy < 1
        jy += Ly
    end
    jx = ix
    Si += Ck[jx, jy]
    return Si
end

# #### P.169 エネルギー差の計算
# ある格子点でのハミルトニアンの値を取っているイメージ。

function calc_ΔE(Ck, ix, iy, J, h, Lx, Ly)
    Si = calc_Si(ix, iy, Lx, Ly, Ck)
    return 2J * Ck[ix, iy] * Si + 2h * Ck[ix, iy]
end

# #### P.169 メトロポリス法：スピンの更新
# 一様乱数を振って、その値が$e^{- \Delta E/ T}$より小さければスピンをフリップする。

function metropolis(σi, ΔE, T, rng)
    is_accepted = ifelse(rand(rng) <= exp(-ΔE / T), true, false)
    σ_new = ifelse(is_accepted, -σi, σi)
    return σ_new, is_accepted
end

# #### P.169 熱浴法
# 一様乱数を振って、その値が$1 / (1 + e^{-\alpha/T})$より小さいなら上向きスピン、それ以外は下向きスピンにする。
# スピンがフリップしたかどうかを`is_accepted`に溜めている。

function heatbath(σi, ΔE, T, rng)
    α = ΔE * σi
    σ_new = ifelse(rand(rng) <= 1 / (1 + exp(-α / T)), +1, -1)
    is_accepted = ifelse(σ_new == σi, false, true)
    return σ_new, is_accepted
end

# #### P.169 メトロポリス法でのある格子点でのスピンの更新関数
# ボルツマン定数は$1$にした。

function local_metropolis_update(Ck, ix, iy, T, J, h, Lx, Ly, rng)
    ΔE = calc_ΔE(Ck, ix, iy, J, h, Lx, Ly)
    σi = Ck[ix, iy]
    return metropolis(σi, ΔE, T, rng)
end

# #### P.169 熱浴法でのある格子点でのスピンの更新関数

function local_heatbath_update(Ck, ix, iy, T, J, h, Lx, Ly, rng)
    ΔE = calc_ΔE(Ck, ix, iy, J, h, Lx, Ly)
    σi = Ck[ix, iy]
    return heatbath(σi, ΔE, T, rng)
end

# #### P.170 モンテカルロ法
# - `num_thermal`: 熱化の回数（測定しない区間）
# - `measure_interval`: 測定のインターバル
# - `num_MC`: 残りのモンテカルロステップ数

using Random
using Plots
function montecarlo(num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal + num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck, ix, iy) = local_metropolis_update(Ck, ix, iy, T, J, h, Lx, Ly, rng)

    Ck = initialize_spins(Lx, Ly, rng)

    Nxy = Lx*Ly

    for trj = 1:num_total
        for isweep = 1:Nxy
            ix = rand(rng, 1:Lx)
            iy = rand(rng, 1:Ly)
            Ck[ix, iy], is_accepted = update(Ck, ix, iy)

            accept_count += ifelse(is_accepted, 1, 0)
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck) / (Lx * Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data, mz)
            end
        end
    end
    return mz_data, accept_count / (num_total * Lx * Ly), absmz_meanvalue / measure_count
end

# #### P.171 モンテカルロ実行用コード

function test()
    Lx = 100
    Ly = 100
    J = 1
    h = 0
    num_thermal = 200
    num_MC = 10000 - num_thermal
    measure_interval = 10
    T = 1
    @time mz_data, acceptance_ratio, absmz = montecarlo(num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)
    println("average acceptance ratio ", acceptance_ratio)
    histogram(mz_data, bin=-1:0.01:1)
    savefig("P171_mz_data_$T.png")
    return
end

# #### モンテカルロ実行

test()

# #### P.172 高速化に向けて

function montecarlo_fast(num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal + num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck, ix, iy) = local_metropolis_update(Ck, ix, iy, T, J, h, Lx, Ly, rng)

    Ck = initialize_spins(Lx, Ly, rng)

    for trj = 1:num_total
        if trj > num_thermal && rand(rng) < 0.01
            @. Ck *= -1
            continue
        end
        # 高速化
        for ix = 1:Lx
            for iy = 1:Ly
                Ck[ix, iy], is_accepted = update(Ck, ix, iy)
                accept_count += ifelse(is_accepted, 1, 0)
            end
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck) / (Lx * Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data, mz)
            end
        end
    end
    return mz_data, accept_count / (num_total * Lx * Ly), absmz_meanvalue / measure_count
end

# #### P.173 高速化コードの実行

function test_tdep()
    Lx = 100
    Ly = 100
    J = 1
    h = 0
    num_thermal = 5000
    num_MC = 50000 - num_thermal
    measure_interval = 10
    mz_Tdep = []
    nT = 20
    Ts = range(0.5, 4.0, length=nT)
    for T in Ts
        @time mz_data, acceptance_ratio, absmz = montecarlo_fast(num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)
        push!(mz_Tdep, absmz)
        println("$T $absmz")
        histogram(mz_data, bin=-1:0.01:1)
        savefig("P173_mz_data_$(T).png")
    end
    plot(Ts, mz_Tdep)
    savefig("P173_mz_tdep.png")
    return
end

# #### 高速化の実行

test_tdep()

# ### 5.2.5 二次元イジング模型のモンテカルロシミュレーションの可視化

function montecarlo_fast(filename, num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)
    ENV["GKSwstype"] = "nul"
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal + num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck, ix, iy) = local_metropolis_update(Ck, ix, iy, T, J, h, Lx, Ly, rng)

    Ck = initialize_spins(Lx, Ly, rng)

    ising = @animate for trj = 1:num_total
        for ix = 1:Lx
            for iy = 1:Ly
                Ck[ix, iy], is_accepted = update(Ck, ix, iy)

                accept_count += ifelse(is_accepted, 1, 0)
            end
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck) / (Lx * Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data, mz)
            end
        end
        heatmap(1:Lx, 1:Ly, Ck, aspect_ratio=:equal)
    end every 100
    gif(ising, "./" * filename, fps=15)
    return mz_data, accept_count / (num_total * Lx * Ly), absmz_meanvalue / measure_count
end

# #### テスト実行用関数

function test_anime()
    Lx = 100
    Ly = 100
    J = 1
    h = 0
    num_thermal = 5000
    num_MC = 20000 - num_thermal
    measure_interval = 10
    T = 0.5
    @time mz_data, acceptance_ratio, absmz = montecarlo_fast("ising_T$T.gif", num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)

    println("average acceptance ratio ", acceptance_ratio)
    histogram(mz_data, bin=-1:0.01:1)
    savefig("P174_mz_data_$(T).png")
    return
end

# #### テスト実行

test_anime()

# #### 比熱の計算

function montecarlo_fast(num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal + num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck, ix, iy) = local_metropolis_update(Ck, ix, iy, T, J, h, Lx, Ly, rng)
    E2_meanvalue = 0.0
    E_meanvalue = 0.0

    Ck = initialize_spins(Lx, Ly, rng)

    for trj = 1:num_total
        if trj > num_thermal && rand(rng) < 0.01
            @. Ck *= -1
            continue
        end

        for ix = 1:Lx
            for iy = 1:Ly
                Ck[ix, iy], is_accepted = update(Ck, ix, iy)

                accept_count += ifelse(is_accepted, 1, 0)
            end
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck) / (Lx * Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data, mz)

                E = measure_energy(Ck, J, h, Lx, Ly)
                E2_meanvalue += E^2
                E_meanvalue += E
            end
        end
    end
    Cv = (E2_meanvalue / measure_count - (E_meanvalue / measure_count)^2) / T^2

    return mz_data, accept_count / (num_total * Lx * Ly), absmz_meanvalue / measure_count, Cv
end

# #### 比熱の計算実行用

function test_tdep()
    Lx = 96
    Ly = 96
    J = 1
    h = 0
    num_thermal = 20000
    num_MC = 100000 - num_thermal
    measure_interval = 10
    mz_Tdep = []
    Cv_Tdep = []

    nT = 20
    Ts = range(0.5, 4.0, length=nT)
    for T in Ts
        @time mz_data, acceptance_ratio, absmz, Cv = montecarlo_fast(num_thermal, num_MC, measure_interval, T, J, h, Lx, Ly)
        push!(mz_Tdep, absmz)
        push!(Cv_Tdep, Cv)
        println("$T $absmz, $Cv")
        histogram(mz_data, bin=-1:0.01:1)
        savefig("P176_mz_data_L$(Lx)_T$(T).png")

        plot(mz_data)
        savefig("P176_mz_trjdep_L$(Lx)_T$(T).png")
    end
    plot(Ts, mz_Tdep)
    savefig("P176_mz_tdep_L$(Lx).png")

    plot(Ts, Cv_Tdep)
    savefig("P176_Cv_tdep_L$(Lx).png")
    return
end

# テスト実行

test_tdep()
