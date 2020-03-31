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
初期条件や波の駆動は `1dim_wave_eq_only_u.rs` と同じ設定にしている.
2 階の常微分方程式でよくやるように, $u, v = u_t$ として,
次の連立の偏微分方程式を解く形にしている.

\begin{align}
u_t = v, \quad v_t = u_{xx}.
\end{align}

これを次のように離散化した.

\begin{align}
u(t + \Delta t, x) &=
u(t, x) + v(t, x) * \Delta t, \\
v(t + \Delta t, x) &=
v(t, x) + \frac{u(t, x - \Delta x) - 2 u(t,x) + u(t, x + \Delta x)}{2 \Delta x} \Delta t.
\end{align}
