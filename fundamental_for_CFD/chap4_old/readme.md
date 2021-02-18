# 第 4 章のコードまとめ: 数値流体解析の基礎 Visual C++とgnuplotによる圧縮性・非圧縮性流体解析
オリジナルが個別の c 言語ファイルだったのを,
重複が鬱陶しいので Rust のプロジェクトとしてまとめました.
入り組んだ構造をしていないシンプルな個別ファイルも大事だろうと思い,
それはそれとして作り続ける予定です.

## 実行法
`Makefile` を参考にしてください.
もう少し具体的に書くと, `cnf.orig.yaml` をもとに設定ファイルを作り,
`cargo run cnf.yaml` などとすれば実行できます.

## 可視化
`cargo run cnf.yaml` で `workspace` に CSV が生成されます.
ここで `python3 1d_mp4.py` を実行すれば `workspace` にある自動で最新のディレクトリを読み取り,
この CSV 群から mp4 を作ってくれます.

## TODO
- エラー処理: yaml の値が不正なときにそこで処理を止めるようにしたい.
  具体的には `lib.rs` の `init` 関数冒頭部.
- ファイルの分割, 特に `lib.rs`.
- 動画作成の高速化: python では `png` だけ作って動画作成は `ffmpeg` に直接投げた方が速かったりする?
  要検証.