# URL メモ
- 2 次元, 条件いろいろ [Python で波動方程式の数値計算と動画 gif の書き出しをやらせてみよう](http://wakabame.hatenablog.com/entry/2018/03/07/205717)
- FTCS 法 [Pythonによる科学・技術計算 FTCS法(陽解法)による1次元・2次元波動方程式の数値解法，双曲型偏微分方程式](https://qiita.com/sci_Haru/items/8535f435ffa1febcd445)
- [流体力学の方程式をpythonでシミュレーションする 1](https://qiita.com/moootoko/items/3f89efc0d6aff7c1c066)

# ファイルごとの方程式の説明
## 1dim_wave_eq_only_u.rs
- [YouTube へのリンク](https://www.youtube.com/watch?v=spe1Yp2_vVI&feature=youtu.be)

[このページ](http://www.yamamo10.jp/yamamoto/lecture/2004/5E/partial_diff/text_wave/html/node2.html)を参考にして,
素直に波動方程式を解いている.

初期条件は $u = 0, u_t = 0$ で,
原点で波を $(1/2) \sin (2 \pi f t)$ で駆動していて,
次のように離散化している.

\begin{align}
u(t + \Delta t, x) =
2 u(t,x) - u(t - \Delta t, x)
+ \left( \frac{\Delta t}{\Delta x} \right)^2 (u(t,x + \Delta x) - 2 u(t,x) + u(t,x)).
\end{align}

## 1dim_wave_eq_u_ut.rs
- [Youtube へのリンク](https://www.youtube.com/watch?v=ILb9vJI6uQg&list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU&index=88&t=0s)

初期条件や波の駆動は `1dim_wave_eq_only_u.rs` と同じ設定にしている.
2 階の常微分方程式でよくやるように, $u, v = u_t$ として,
次の連立の偏微分方程式を解く形にしている.

\begin{align}
u_t = v, \quad v_t = u_{xx}.
\end{align}

[黒木さんからの指摘](https://twitter.com/genkuroki/status/1245037226284552199)を受け,
次のように離散化する.

まず大元の波動方程式を次のように離散化する.

\begin{align}
\frac{\frac{u(t+\Delta t,x)-u(t,x)}{\Delta t}-\frac{u(t,x)-u(t-\Delta t,x)}{\Delta t}}{\Delta t} &=
\frac{\frac{u(t,x+\Delta x)-u(t,x)}{\Delta x}-\frac{u(t,x)-u(t,x-\Delta x)}{\Delta x}}{\Delta x}.
\end{align}

ここで $v(t,x)=\frac{u(t,x)-u(t-\text{\ensuremath{\Delta t,x)}}}{\Delta t}$ と書くことにすると,
上の差分化は次のように書き直せる.

\begin{align}
\frac{v(t+\Delta t,x)-v(t,x)}{\Delta t} & =
\frac{u(t,x+\Delta x)-2u(t,x)+u(t,x-\Delta x)}{(\Delta x)^{2}},\\
u(t,x) & =
u(t-\Delta t,x)+v(t,x)\Delta t.
\end{align}

これをさらに次のように書き直し,
最終的に次の離散化で計算する.

\begin{align}
v(t+\Delta t,x) &=
v(t,x)+\frac{u(t,x+\Delta x)-2u(t,x)+u(t,x-\Delta x)}{(\Delta x)^{2}}\Delta t,\\
u(t+\Delta t,x) &=
u(t,x)+v(t+\Delta t,x)\Delta t.
\end{align}

### まずい離散化メモ
はじめに実装していたバージョン.
記録のために残しておく.

> これを次のように離散化した.
>
> \begin{align}
> u(t + \Delta t, x) &=
> u(t, x) + v(t, x) \Delta t, \\
> v(t + \Delta t, x) &=
> v(t, x) + \frac{u(t, x - \Delta x) - 2 u(t,x) + u(t, x + \Delta x)}{(\Delta x)^2} \Delta t.
> \end{align}

## 1dim_wave_eq_pml_processing.rs
- [Youtube へのリンク](https://www.youtube.com/watch?v=pwHatoXioR8&feature=youtu.be)

[このページ](https://qiita.com/tobira-code/items/bd62daa19c42ba169cf2)にある processing のコード,
`processing/sketch_1dim_wave_eq_pml.pde` を直接移植したコード.
左端で振動を駆動させていて,
領域中央の `cnf.nx / 2` から吸収壁が始まる.

PML は変則的な 1 階化を使う.
これは `wave_eq.lyx` にまとめた.
勘違いしないようにドキュメントをよく読んで注意すること.

## 1dim_wave_eq_pml_right_wall.rs
- [Youtube 動画](https://www.youtube.com/watch?v=qJapxtwSR3k)

`1dim_wave_eq_pml_processing.rs` では吸収壁は領域中央からはじまっていたのを右端
5 層に押し込めた.

## 1dim_wave_eq_u_ut_pml.rs
- [波動方程式の PML 参考](https://qiita.com/tobira-code/items/bd62daa19c42ba169cf2)

## 2dim_wave_eq_only_u.rs
- [YouTube 動画](https://www.youtube.com/watch?v=Wl5gEDZ-bgU)

中心を強制振動させ, それで波を起こしている.
これだけだとわかりづらいが,
PML つきの動画と比較すると反射が起こっていることがわかる.

## 2dim_wave_eq_pml_both_side.rs
- [YouTube 動画](https://www.youtube.com/watch?v=VUUlgO72v4w)

上の動画と同じく中心を強制振動させ, それで波を起こしている.
こちらは吸収壁をつけた.
上の動画と比較すると境界で反射が起きていないことがわかる.

PML の定式化については `wave_eq.lyx` 参照.

## TODO
次のリンク先の Julia コードを実装する.

- <https://twitter.com/genkuroki/status/1245073613973123072>
- <https://twitter.com/genkuroki/status/1246083852251975680>

Rust でもベクトル計算したいのだが, できる?
