# # 5日目 統計力学
# ## パッケージ読み込み・シード固定
using IJulia
using Plots
using Random
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
    while B==A # 渡す人ともらう人は別にしたい
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

    for i=1:numgroups
        totalboxes[i,:] = make_initial(numpeople, numballs)
    end

    hist = histogram(totalboxes[:],
        nbins=-0.5:1:numballs,
        label="initial",
        ylims=(0,totalpeople*0.3))
    # savefig("hist.png")
    display(hist)
end

#

test2()

# ### 5.1.4 ゲーム後のヒストグラム
function test3(seed=myseed)
    # Random.seed!(seed)
    numpeople = 6
    numballs = 30
    numgroups = 100
    totalpeople = numpeople * numgroups
    totalboxes = zeros(Int64, numgroups, numpeople)

    for i=1:numgroups
        totalboxes[i,:] = make_initial(numpeople, numballs)
    end

    numtotal = 300
    for itrj = 1:numtotal
        for i=1:numgroups
            totalboxes[i,:] = giveandtake(totalboxes[i,:])
        end
    end
    hist = histogram(totalboxes[:],
        nbins=-0.5:1:numballs,
        label="$numtotal",
        ylims=(0,totalpeople*0.3))
    # savefig("hist_300.png")
    display(hist)
end

#

test3()

# #### 300回に至るまでのチップの分布の変化：GIF

function test3()
    numpeople = 6
    numballs = 30
    numgroups = 100
    totalpeople = numpeople * numgroups
    totalboxes = zeros(Int64, numgroups, numpeople)

    for i=1:numgroups
        totalboxes[i,:] = make_initial(numpeople, numballs)
    end

    numtotal = 300
    anim = Animation()
    @gif for itrj = 1:numtotal
        println("$itrj-th")
        for i=1:numgroups
            totalboxes[i,:] = giveandtake(totalboxes[i,:])
        end
        plt = histogram(totalboxes[:], nbins=-0.5:1:numballs, label="$itrj", ylims=(0,totalpeople*0.3))
        frame(anim, plt)
    end
    gif(anim, "histplot.gif", fps=30)
    # gif(anim, fps=50)
    # display(gif)
    # hist = histogram(totalboxes[:],nbins=-0.5:1:numballs,label="$numtotal",ylims=(0,totalpeople*0.3))
    # savefig("hist_300.png")
    # display(hist)
end

#

test3()

#

using Random
function initialize_spins(Lx,Ly,rng)
    return rand(rng,[-1,1],Lx,Ly)
end

#

function measure_Mz(Ck)
    return sum(Ck)
end

#

function measure_energy(Ck,J,h,Lx,Ly)
    energy = 0
    for iy=1:Ly
        for ix=1:Lx
            Si = calc_Si(ix,iy,Lx,Ly,Ck)
            σi = Ck[ix,iy]
            energy += -(J/2)*σi*Si - h*σi
        end
    end
    return energy
end

#

function calc_Si(ix,iy,Lx,Ly,Ck)
    jx = ix + 1
    if jx > Lx
        jx -= Lx
    end
    jy=iy
    Si = Ck[jx,jy]

    jx = ix - 1
    if jx < 1
        jx += Lx
    end
    jy = iy
    Si += Ck[jx,jy]

    jy = iy + 1
    if jy > Ly
        jy -= Ly
    end
    jx = ix
    Si += Ck[jx,jy]

    jy = iy-1
    if jy < 1
        jy += Ly
    end
    jx = ix
    Si += Ck[jx,jy]
    return Si
end

#

function calc_ΔE(Ck,ix,iy,J,h,Lx,Ly)
    Si = calc_Si(ix,iy,Lx,Ly,Ck)
    return 2J*Ck[ix,iy]*Si + 2h*Ck[ix,iy]
end

#

function metropolis(σi,ΔE,T,rng)
    is_accepted = ifelse(rand(rng) <= exp(-ΔE/T),true,false)
    σ_new = ifelse(is_accepted,-σi,σi)
    return σ_new,is_accepted
end

#

function heatbath(σi,ΔE,T,rng)
    α = ΔE*σi
    σ_new = ifelse(rand(rng) <= 1/(1+exp(-α/T)),+1,-1)
    is_accepted = ifelse(σ_new == σi,false,true)
    return σ_new,is_accepted
end

#

function local_metropolis_update(Ck,ix,iy,T,J,h,Lx,Ly,rng)
    ΔE = calc_ΔE(Ck,ix,iy,J,h,Lx,Ly)
    σi = Ck[ix,iy]
    return metropolis(σi,ΔE,T,rng)
end

#

function local_heatbath_update(Ck,ix,iy,T,J,h,Lx,Ly,rng)
    ΔE = calc_ΔE(Ck,ix,iy,J,h,Lx,Ly)
    σi = Ck[ix,iy]
    return heatbath(σi,ΔE,T,rng)
end

#

using Random
using Plots
function montecarlo(num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal+num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck,ix,iy) = local_metropolis_update(Ck,ix,iy,T,J,h,Lx,Ly,rng)

    Ck = initialize_spins(Lx,Ly,rng)

    for trj = 1:num_total
        for isweep = 1:Lx*Ly
            ix = rand(rng,1:Lx)
            iy = rand(rng,1:Ly)
            Ck[ix,iy],is_accepted = update(Ck,ix,iy)

            accept_count += ifelse(is_accepted,1,0)
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck)/(Lx*Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data,mz)
            end
        end
    end
    return mz_data,accept_count/(num_total*Lx*Ly),absmz_meanvalue/measure_count
end

#

function test()
    Lx = 100
    Ly = 100
    J = 1
    h = 0
    num_thermal = 200
    num_MC = 10000-num_thermal
    measure_interval = 10
    T = 1
    @time mz_data,acceptance_ratio,absmz = montecarlo(num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)
    println("average acceptance ratio ",acceptance_ratio)
    histogram(mz_data,bin=-1:0.01:1)
    savefig("mz_data_$T.png")
    return
end

#

test()

#

function montecarlo_fast(num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal+num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck,ix,iy) = local_metropolis_update(Ck,ix,iy,T,J,h,Lx,Ly,rng)

    Ck = initialize_spins(Lx,Ly,rng)

    for trj = 1:num_total
        if trj > num_thermal && rand(rng) < 0.01
            @. Ck *= -1
            continue
        end
        for ix = 1:Lx
            for iy=1:Ly
                Ck[ix,iy],is_accepted = update(Ck,ix,iy)

                accept_count += ifelse(is_accepted,1,0)
            end
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck)/(Lx*Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data,mz)
            end
        end
    end
    return mz_data,accept_count/(num_total*Lx*Ly),absmz_meanvalue/measure_count
end

#

function test_tdep()
    Lx = 100
    Ly = 100
    J = 1
    h = 0
    num_thermal = 5000
    num_MC = 50000-num_thermal
    measure_interval = 10
    mz_Tdep = []
    nT = 20
    Ts = range(0.5,4.0,length = nT)
    for T in Ts
        @time mz_data,acceptance_ratio,absmz = montecarlo_fast(num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)
        push!(mz_Tdep,absmz)
        println("$T $absmz")
        histogram(mz_data,bin=-1:0.01:1)
        savefig("mz_data_$(T).png")
    end
    plot(Ts,mz_Tdep)
    savefig("mz_tdep.png")
    return
end

#

test_tdep()

#

function montecarlo_fast(filename,num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)
    ENV["GKSwstype"] = "nul"
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal+num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck,ix,iy) = local_metropolis_update(Ck,ix,iy,T,J,h,Lx,Ly,rng)

    Ck = initialize_spins(Lx,Ly,rng)

    ising = @animate for trj = 1:num_total
        for ix = 1:Lx
            for iy=1:Ly
                Ck[ix,iy],is_accepted = update(Ck,ix,iy)

                accept_count += ifelse(is_accepted,1,0)
            end
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck)/(Lx*Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data,mz)
            end
        end
        heatmap(1:Lx,1:Ly,Ck,aspect_ratio=:equal)
    end every 100
    gif(ising, "./"*filename, fps = 15)
    return mz_data,accept_count/(num_total*Lx*Ly),absmz_meanvalue/measure_count
end

#

function test_anime()
    Lx = 100
    Ly = 100
    J = 1
    h = 0
    num_thermal = 5000
    num_MC =20000-num_thermal
    measure_interval = 10
    T = 0.5
    @time mz_data,acceptance_ratio,absmz = montecarlo_fast("ising_T$T.gif",num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)

    println("average acceptance ratio ",acceptance_ratio)
    histogram(mz_data,bin=-1:0.01:1)
    savefig("mz_data_$(T).png")
    return
end

#

test_anime()

#

function montecarlo_fast(num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)
    #Random.seed!(123)
    rng = MersenneTwister(123)
    num_total = num_thermal+num_MC
    accept_count = 0
    absmz_meanvalue = 0
    measure_count = 0
    mz_data = []
    update(Ck,ix,iy) = local_metropolis_update(Ck,ix,iy,T,J,h,Lx,Ly,rng)
    E2_meanvalue = 0.0
    E_meanvalue = 0.0

    Ck = initialize_spins(Lx,Ly,rng)

    for trj = 1:num_total
        if trj > num_thermal && rand(rng) < 0.01
            @. Ck *= -1
            continue
        end

        for ix = 1:Lx
            for iy=1:Ly
                Ck[ix,iy],is_accepted = update(Ck,ix,iy)

                accept_count += ifelse(is_accepted,1,0)
            end
        end

        if trj > num_thermal
            if trj % measure_interval == 0
                measure_count += 1
                mz = measure_Mz(Ck)/(Lx*Ly)
                absmz_meanvalue += abs(mz)
                push!(mz_data,mz)

                E = measure_energy(Ck,J,h,Lx,Ly)
                E2_meanvalue += E^2
                E_meanvalue += E

            end
        end
    end
    Cv = (E2_meanvalue/measure_count - (E_meanvalue/measure_count)^2)/T^2

    return mz_data,accept_count/(num_total*Lx*Ly),absmz_meanvalue/measure_count,Cv
end

#

function test_tdep()
    Lx = 96
    Ly = 96
    J = 1
    h = 0
    num_thermal = 20000
    num_MC =100000-num_thermal
    measure_interval = 10
    mz_Tdep = []
    Cv_Tdep = []

    nT = 20
    Ts = range(0.5,4.0,length= nT)
    for T in Ts
        @time mz_data,acceptance_ratio,absmz,Cv = montecarlo_fast(num_thermal,num_MC,measure_interval,T,J,h,Lx,Ly)
        push!(mz_Tdep,absmz)
        push!(Cv_Tdep,Cv)
        println("$T $absmz, $Cv")
        histogram(mz_data,bin=-1:0.01:1)
        savefig("mz_data_L$(Lx)_T$(T).png")

        plot(mz_data)
        savefig("mz_trjdep_L$(Lx)_T$(T).png")
    end
    plot(Ts,mz_Tdep)
    savefig("mz_tdep_L$(Lx).png")

    plot(Ts,Cv_Tdep)
    savefig("Cv_tdep_L$(Lx).png")
    return
end

#

test_tdep()
