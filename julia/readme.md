# Julia

## 参考

- [チートシート](https://juliadocs.github.io/Julia-Cheat-Sheet/ja)

自分用のまとめとして特に統計系の資料・教材・コードもここにまとめる.
統計については特に黒木さんによる資料を体系的にまとめることも目的にする.

- [GitHub, genkuroki/Statistics](https://github.com/genkuroki/Statistics)

黒木さんの他の数学系コードも自分なりにまとめたい.
[プログラミングで数学を 中高数学虎の穴](https://phasetr.com/mthlp1/)の Julia 版も作りたい.

### Julia メモ

- [黒木ツイート](https://twitter.com/genkuroki/status/1318631440129560576)

### rc ファイル

`.juliarc.jl` が初期化のファイルになる? [参考](https://stackoverflow.com/questions/42104130/module-aliasing-in-julia).

## REPL

### 開始

```sh
julia

1+2
ans # 直近の評価結果
```

### 雑多なモード

```sh
# help
julia> ?
# バックスペースで抜ける

# パッケージモード: パッケージをインストールする
julia> ] # "]" を入力する
pkg> add Example # Example.jl をインストールできる
```

## IJulia

IJulia.jl をインストールする必要がある.

```sh
julia
julia> ]
pkg> add IJulia
```

立ち上げは次の通り.

```sh
julia
julia> using IJulia
julia> notebook()
```
