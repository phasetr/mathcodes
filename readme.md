I collected my scientific codes, especially math and physics.

- [GitHub URL](https://github.com/phasetr/mathcodes)

# python

Jupyter がすごいのでそれを使ったコードも書いてみたい.
[ここ](https://try.jupyter.org/)に行けばネット越しに Jupyter が使えるので,
インストールしたりコマンド叩いたりがつらい人はとりあえずこちらで実験してみよう.

以下, インストールについての注意を書いておく.
2016-08 時点では[Qiita のこの記事](http://qiita.com/y__sama/items/5b62d31cb7e6ed50f02c)が参考になる.

詳しくはそちらを見てもらうことにして,
ここでは簡単にまとめておく.

readme を書いた時点では次のバージョンで動かしている.

- Python 3.5.1 :: Anaconda 4.1.0 (x86_64)

ライブラリのバージョンも書いた方がいいのかもしれないが,
それはサボる.

## Anaconda でのインストール

### Windows (未検証)

- [公式サイト](https://www.continuum.io/downloads#_windows)からダウンロード, インストール.

### Mac (検証)

Homebrew で次のコマンド実行でインストールした.
brew upgrade でかなり時間がかかった.
注意してほしい.

また次のコマンドリストの # が入っている箇所では, # 以下は入力**しない**ようにすること.
コメントのつもりで書いている.

~~~~
$ brew update && brew upgrade
$ brew install pyenv
$ echo 'export PATH="$HOME/.pyenv/bin:$PATH"' >> ~/.bash_profile
$ echo 'eval "$(pyenv init -)"' >> ~/.bash_profile
$ exec $SHELL -l
$ pyenv install -l | grep anaconda # Check the newest version
$ pyenv install anaconda3-4.1.0    # Input the newest
$ pyenv global anaconda3-4.1.0     # Input the newest
$ python --version
~~~~

## TODO VPython インストール

2016-08 時点で Python3 対応ができていないようだ.
計算物理の文献で VPython を使っているケースを見かけるので,
Python3 に対応次第, こちらも追記したい.

## Jupyter 起動
### シンプルに

本来はコマンドプロンプトやターミナル (いわゆる「黒いやつ」) で起動しないといけない.
しかしこのハードルは高いだろうという気もする.
そこでダブルクリックすれば自動で起動できるファイルを作った.

- jupyter/runjupyter.bat
- jupyter/runjupyter.command

Windows の方は bat ファイルを,
Mac の方は command ファイルを使うこと.
これを適当なフォルダに置いてダブルクリックで実行すれば,
そのフォルダで jupyter が起動する.

### 詳細

jupyter/fundamental.ipynb にも書いておいたが,
そもそも起動できないのに確認しようもない.
簡単に書いておく.

~~~~
$ cd some_directory # 適当なディレクトリに移動
$ jupyter notebook
~~~~

これでそのディレクトリ上で jupyter が起動する.
ファイルを作ったりすると some_directory にファイルが作られる.

Jupyter Notebook については jupyter/fundamental.ipynb にまとめてあるのでそちら参照.
次のようなことがまとめてある.

- Markdown 記法が使えること
- mathjax が使えるので TeX が使えること.
- 簡単なキーバインドまとめ.
- Python の基礎の基礎.
- IPython の基礎の基礎.

簡単なものでもアニメーションやグラフを描けるのが楽しい.
jupyter/fundamental.ipynb を軽く眺めたら,
次に jupyter/math_simple_graph.ipynb を見てほしい.
あとは適当に書き進めていく.

# visualization
## VTK
- [VTK documentation](https://vtk.org/documentation/)
- [データサンプル](https://vtk.org/Wiki/VTK_Datasets)
- [CSV を読み込む](https://qiita.com/implicit_none/items/e108e0b9f30784ec719a)

## Gnuplot
- [日本語マニュアルページ](http://takeno.iee.niit.ac.jp/~foo/gp-jman/)
- [Ver 5.5 目次](http://takeno.iee.niit.ac.jp/~foo/gp-jman/data/current/20200326/gnuplot-ja-div/index.html)

# 生成した数値計算結果ファイルの削除
次のコマンドで良さそう.

```
git clean -dnX ./   # まずこれでチェック
# git clean -dfX ./ # 実行: 本当に消えるので実行するときは要注意
```

# Introduction_to_Lattice_Boltzmann_Equation
## TODO
- https://qiita.com/ebinan92/items/6c61f660092a970bef1a
- https://github.com/ebinan92/Fingering_dynamics/tree/master/lattice_boltzmann

## sample1.py
- [YouTube](https://youtu.be/1mSGfU9hKvc)
- [参考ページ](https://www.hello-python.com/2017/04/30/格子ボルツマン法を用いた流体力学のシミュレー/)
- [オリジナルのページ](http://physics.weber.edu/schroeder/fluids/)

# rust
## ode_dynamical_system001.rs
- [YouTube](https://www.youtube.com/watch?v=4-35rRRQS8I&list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU&index=107)

[このページ](https://qiita.com/akoamay/items/50ecc312cd84596203c1)を参考にした.
可視化は python で gnuplot を生成してその gnuplot から png 生成し,
ffmpeg で mp4 化.
後半にいくにつれて 10MB 程度ある csv を読み込んでは処理することになり,
処理が重くなるので軽量化したい.

## ode_dynamical_system003.rs
- [YouTube](https://www.youtube.com/watch?v=KGs7snpMpRw&feature=youtu.be)

`ode_dynamical_system001.rs` 同様.
これは「回転」.

## ode_dynamical_system004.rs
- [YouTube](https://youtu.be/qe28I-BXsiI)

`ode_dynamical_system001.rs` 同様.
これは「鞍点」.
可視化で新しいファイルを作らず,
`ode_dynamical_system003_visualize.py` を転用している.

# wave_eq
## URL メモ
- 2 次元, 条件いろいろ [Python で波動方程式の数値計算と動画 gif の書き出しをやらせてみよう](http://wakabame.hatenablog.com/entry/2018/03/07/205717)
- FTCS 法 [Pythonによる科学・技術計算 FTCS法(陽解法)による1次元・2次元波動方程式の数値解法，双曲型偏微分方程式](https://qiita.com/sci_Haru/items/8535f435ffa1febcd445)
- [流体力学の方程式をpythonでシミュレーションする 1](https://qiita.com/moootoko/items/3f89efc0d6aff7c1c066)

## ファイルごとの方程式の説明
### 1dim_wave_eq_only_u.rs
- [YouTube へのリンク](https://www.youtube.com/watch?v=spe1Yp2_vVI&feature=youtu.be)

[このページ](http://www.yamamo10.jp/yamamoto/lecture/2004/5E/partial_diff/text_wave/html/node2.html)を参考にして,
素直に波動方程式を解いている.

初期条件は $u = 0, u_t = 0$ で,
原点で波を $(1/2) \sin (2 \pi f t)$ で駆動していて,
離散化については `math_memo.lyx` を見ること.

### 調整中 1dim_wave_eq_only_u_with_src.rs
`1dim_wave_eq_only_u.rs` では `u` を直接動かしているが,
こちらは外力 `f` で駆動しようとしている.
振幅が異様に小さく, なぜうまくいかないかまだわかっていない.

### 1dim_wave_eq_pml_both_side.rs
`1dim_wave_eq_only_u.rs` と同じ条件で波を生成している.
両端に PML をつけていて反射が起きない.

### 1dim_wave_eq_pml_both_side_compare.rs
- [YouTube へのリンク](https://www.youtube.com/watch?v=2lwbDbXS5NE&list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU&index=100)

`1dim_wave_eq_pml_both_side.rs` に対して PML なしの波も追加した.
PML の有無での挙動を比較している.

### 1dim_wave_eq_pml_processing.rs
- [Youtube へのリンク](https://www.youtube.com/watch?v=pwHatoXioR8&feature=youtu.be)

[Qiita: Processingでシミュレーション～境界付近で波を消す](https://qiita.com/tobira-code/items/bd62daa19c42ba169cf2)にある processing のコード,
`processing/sketch_1dim_wave_eq_pml.pde` を直接移植したコード.
左端で駆動していて領域中央の `cnf.nx / 2` から吸収壁が始まる.

元ネタはおそらく次の英語のノート.

- [Steven G. Johnson, Notes on Perfectly Matched Layers (PMLs)](https://math.mit.edu/~stevenj/18.369/pml.pdf)

PML は変則的な 1 階化を使う.
これは `math_memo.lyx` にまとめた.
勘違いしないようにドキュメントをよく読んで注意すること.

### 1dim_wave_eq_pml_right_wall.rs
- [Youtube 動画](https://www.youtube.com/watch?v=qJapxtwSR3k)

`1dim_wave_eq_pml_processing.rs` と原則同じ.
こちらは右端の 5 層の小領域で一気に吸収させている.

### 1dim_wave_eq_u_ut.rs
- [Youtube へのリンク](https://www.youtube.com/watch?v=ILb9vJI6uQg&list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU&index=88&t=0s)

初期条件や波の駆動は `1dim_wave_eq_only_u.rs` と同じ設定にしている.
2 階の常微分方程式でよくやるように, $u, v = u_t$ として,
次の連立の偏微分方程式を解く形にしている.

\begin{align}
u_t = v, \quad v_t = u_{xx}.
\end{align}

### 1dim_wave_eq_u_ut_with_src.rs
- [YouTube へのリンク](https://www.youtube.com/watch?v=G0-1apiHiog&feature=youtu.be)

ふつうの 1 階化方程式で波源を中央につけた.

### 1dim_wave_eq_u_ut_with_src_pml.rs
波源と PML を両方つけた.
比較用に PML なしの近似解も計算している.

`math_memo.lyx` にある離散化の注意を守りつつ,
内部領域と PML 領域で解く方程式を変えている.
定式化については `math_memo.lyx` を見ること.

### 1dim_wave_eq_u_ut_with_src_pml_nonzero_bdval.rs
境界値を 0 からずらしてうまくいくかどうか検証した.

### 2dim_wave_eq_only_u.rs
- [YouTube 動画](https://www.youtube.com/watch?v=Wl5gEDZ-bgU)

中心を強制振動させ, それで波を起こしている.
これだけだとわかりづらいが,
PML つきの動画と比較すると反射が起こっていることがわかる.

### 2dim_wave_eq_pml_both_side.rs
- [YouTube 動画](https://www.youtube.com/watch?v=VUUlgO72v4w)

上の動画と同じく中心を強制振動させ, それで波を起こしている.
こちらは吸収壁をつけた.
上の動画と比較すると境界で反射が起きていないことがわかる.

PML の定式化については `math_memo.lyx` 参照.

### TODO
次のリンク先の Julia コードを実装する.

- <https://twitter.com/genkuroki/status/1245073613973123072>
- <https://twitter.com/genkuroki/status/1246083852251975680>

Rust でもベクトル計算したいのだが, できる?

# TODO リスト・参考文献
Python または Rust でコードを書いて貯めたい.

- http://www.math.sci.hiroshima-u.ac.jp/~awa/SUURI_11/saishuu.html
- http://nalab.mind.meiji.ac.jp/~mk/labo/text/wave.pdf
- https://twitter.com/genkuroki/status/1245079528692535298
- [Haskell 荘園](https://scrapbox.io/haskell-shoen/)
- FDTD、粒子法、格子ボルツマン
    - PySPH https://github.com/pypr/pysph
    - 検討 https://twitter.com/mathsorcerer/status/1227950814410371072?s=21
- https://twitter.com/corymsimon/status/1225178682324475905?s=21
- 制御をJulia+Jupyterでやっているとかいう講義資料がある模様。
- https://twitter.com/kaisekigakumoyo/status/1225742614789533696?s=21
- Computational Linear Algebra for Coders (taught in Python with Jupyter Notebooks) 講義Youtubeつき
- Pythonでできる！ 2次元コロイド結晶の構造解析https://qiita.com/kon2/items/c587fa826bf2134c8e1e
- 装飾パターンの法則 フェドロフ、エッシャー、ペンローズ https://honto.jp/netstore/pd-book_27281704.html
- https://twitter.com/ceptree/status/1238309859981844483?s=21
- CFD Julia: A Learning Module Structuring an Introductory Course on Computational Fluid Dynamics
- https://www.mdpi.com/2311-5521/4/3/159
- http://kamonama.blogspot.com/2009/02/blog-post_23.html
    - http://kamonama.blogspot.com/2009/02/blog-post_23.html
    - http://kamonama.blogspot.com/2009/11/haskellocamlsph.html
    - http://science.cc.kochi-u.ac.jp/scientific_reports/vol02/serfst201901.pdf
- 粒子法 http://www.hirax.net/mobile/content/8754
- 密度汎関数法・分子力場計算
- 有限要素法 アルゴリズムとプログラミング https://www.youtube.com/watch?v=nJqLA4nJosQ
- 分子動力学 https://qiita.com/sci_Haru/items/2b9696911cf0dc29738a
- https://qiita.com/tags/計算物理学
- 数値相対論
- https://twitter.com/doraneko_b1f/status/1241290303690047489
  Linda J.S. Allen「生物数学入門―差分方程式・微分方程式の基礎からのアプローチ」を読んで差分方程式・微分方程式の勉強をしつつ、mathematical_biologyのリポジトリ（もちろんRust）を作っています。
- https://twitter.com/yuruyurau/status/1243147033742938112?s=21
- 高速フーリエ変換 https://caddi.tech/archives/836
- 桂田研卒研ノート
    - http://nalab.mind.meiji.ac.jp/~mk/labo/text/
    - http://nalab.mind.meiji.ac.jp/~mk/labo/text/green.pdf
- [格子ボルツマン法オープンソース Palabosについて その1](https://note.com/mmer547/n/n7ea6ba297237)
- https://takun-physics.net/?p=4195
- https://github.com/termoshtt/eom
- https://twitter.com/genkuroki/status/1233657468409901056?s=21
- https://twitter.com/cometscome_phys/status/1243848172134227970?s=21
- http://www.tsunami.civil.tohoku.ac.jp/hokusai3/J/shibu/19/araki.pdf
- 有限体積法の解説：https://qiita.com/ur_kinsk/items/03e8e20c51a434e50c9c

# YouTube 投稿用メモ
## 追加すべきリスト
- [YouTube: 数学・物理・プログラミング](https://www.youtube.com/watch?v=8RIrq4j8Qg0&list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU&index=1)

## タイトル用サンプル
- Rust 有限体積法 1 次元の線型移流方程式 アニメーションサンプル 数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析

## 動画コメント用サンプル
コードは GitHub に置いてあるので興味があればどうぞ.

- https://github.com/phasetr/mathcodes/

対応するのは次のコードです.

- https://github.com/phasetr/mathcodes/blob/master/ ここを埋める

これ以外に, 数学・物理・プログラミングに関する無料の通信講座を運営しています.
もしあなたがご興味あるなら,
ぜひ次の通信講座ページ一覧を眺めてみてください.

- https://phasetr.com/blog/2014/06/09/トップ固定記事：メルマガ・数学カフェ・その他/
- https://phasetr.com/mthlp1/

他にも YouTube で数値実験の動画を投稿しています.
リストにしているのでこちらもぜひどうぞ.

- https://www.youtube.com/playlist?list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU

GitHub でもいくつかコードを公開しています.

- https://github.com/phasetr/OpenFOAM
- https://github.com/phasetr/mathcodes

ぜひチャンネル登録もしてください。

- https://www.youtube.com/channel/UCZ0p3rtw65Kw7BeR-hdndMw?sub_confirmation=1
