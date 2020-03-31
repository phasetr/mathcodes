# 数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析
## 書籍情報
- [コロナ社のサイト](https://www.coronasha.co.jp/np/isbn/9784339046649/)
- [Amazon へのリンク](https://www.amazon.co.jp/dp/4339046647)

## このリポジトリ (ディレクトリ) に関する注意
[Amazon のレビュー](https://www.amazon.co.jp/gp/customer-reviews/R33RHW7EY4FF24/ref=cm_cr_dp_d_rvw_ttl?ie=UTF8&ASIN=4339046647)にも書いたのだが,
オリジナルの C のコードがあまりにひどいのです.
そもそも書名に「C++」とあるから C++ の本だと思ったのですが,
実際には C のコードでした.
描画部分も, 太古のアニメーションが書けなかった頃の Gnuplot の流儀で書かれていて,
計算しながら Gnuplot を起動して計算しています.
「計算結果の可視化グラフをファイルに保存」という部分では postscript を使っているようで,
完全に Gnuplot だけで閉じているわけでもないようです
(postscript をよく知らないので何も言えない).

こんな感じであまりにひどかったので,
自分で書き直すことにしました.
まずは使い慣れている Python で書くことにし,
そのあと, 勉強も兼ねて Rust で書き直す予定です.
数値計算の習作用なので, コードの構造などにはあまり気を使わない予定です.

## YouTube へのリンク
どのプログラムでどんな動画ができるのか,
いちいち動かさないとわからないのでは面倒なので,
作った動画ファイルを YouTube にアップすることにしています.

- [動画リスト: 数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析](https://www.youtube.com/playlist?list=PLSBzltjFopragPoCA2WAfkYzJkoNq-4Ms)

次のリストは私が作った各種数値計算系の動画をまとめたリスト.

- [YouTube: 数学・物理・プログラミング](https://www.youtube.com/watch?v=8RIrq4j8Qg0&list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU&index=1)

## YouTube 投稿用メモ
### 追加すべきリスト
- [YouTube: 数学・物理・プログラミング](https://www.youtube.com/watch?v=8RIrq4j8Qg0&list=PLSBzltjFopraTJUYDMXnj1GdYCdR0QyzU&index=1)
- [数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析](https://www.youtube.com/playlist?list=PLSBzltjFopragPoCA2WAfkYzJkoNq-4Ms)

### タイトル用サンプル
- Rust 有限体積法 1 次元の線型移流方程式 アニメーションサンプル 数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析
- Python 有限体積法 1 次元の線型移流方程式 アニメーションサンプル 数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析

### 動画コメント用サンプル
「数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析」を Rust/Python で書き直し,
その結果を Python で動画にしたアニメーションサンプルです.
コードは Rust/Python 版を GitHub に置いてあるので興味があればどうぞ.

- https://github.com/phasetr/mathcodes/fundamental_for_CFD/

対応するのは次のコードです.

- https://github.com/phasetr/mathcodes/blob/master/fundamental_for_CFD/ ここを埋める

これ以外に, 数学・物理・プログラミングに関する無料の通信講座を運営しています.
もしあなたがご興味あるなら,
ぜひ次の通信講座ページ一覧を眺めてみてください.

- https://phasetr.com/blog/2014/06/09/トップ固定記事：メルマガ・数学カフェ・その他/
- https://phasetr.com/mthlp1/

他にも YouTube で数値実験の動画を投稿しています.
リストにしているのでこちらもぜひどうぞ.

- https://www.youtube.com/watch?v=HKnaEqpSJvk&list=PLSBzltjFoprYN0Rh3tLmoHWqjPRYwJukv

GitHub でもいくつかコードを公開しています.

- https://github.com/phasetr/OpenFOAM
- https://github.com/phasetr/mathcodes

ぜひチャンネル登録もしてください。

- https://www.youtube.com/channel/UCZ0p3rtw65Kw7BeR-hdndMw?sub_confirmation=1
