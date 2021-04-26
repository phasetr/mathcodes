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
    display_name: .NET (F#)
    language: F#
    name: .net-fsharp
---

# Multiples of 3 and 5
- [Project Euler](https://projecteuler.net/problem=1)


## TODO バージョン情報
Juliaの`versioninfo()`のような関数を探している。


## Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.

> 1000 未満の 3 と 5 の倍数のすべての和を計算せよ。


## 方針またはプログラミング基礎
凝ったことをしていないのでPythonとほとんど同じように考えられる。
もちろん構文は大きく変わる。

F#をはじめとした、いわゆる関数型言語でははじめから高階関数をバリバリ使う。
PythonやJuliaでも既に紹介しているから、ここではその前提で進めよう。
ついでにパイプラインもバリバリ使う。
（私には）楽しい。

### 方針1
まずは1000未満の3の倍数と5の倍数を全部作ってみよう。ただ、1000だと結果が見づらいので、`n=30`くらいにして見やすくする。

```fsharp
// 本来の値
let N = 999
```

まず配列を`[| 1 .. n |]`で作っておき、それに`filter`をかけるのがF#での普通の作り方だろう。

`filter`の中であまりを見るとき、PythonやJuliaのように`==`でチェックするのではなく、`= 0`でチェックしているのに注意する。

```fsharp
let n = 30
let threes = [| 1 .. n |] |> Array.filter (fun x -> x % 3 = 0)
threes |> printfn "%A"
```

```fsharp
let fives = [| 1 .. n |] |> Array.filter (fun x -> x % 5 = 0)
fives |> printfn "%A"
```

このふたつのリストには重複がある。
具体的には15の倍数が重複する。
何らかの方法でこの重複を処理したい。

方法はいくつかある。
最終的には効率・速度も考えないといけないが、ここではそこまでは要求しない。

一番簡単なのはリストを一気に作る方法か？

```fsharp
let numbers = [|1..n|] |> Array.filter (fun x -> x % 3 = 0 || x % 5 = 0)
numbers |> printfn "%A"
```

あとは和を取る。
Juliaで`|>`を使ったときと同じように書ける。

```fsharp
[| 1 .. n |] 
|> Array.filter (fun x -> x % 3 = 0 || x % 5 = 0)
|> Array.sum
|> printfn "%d"
```

`n`を`N`に変えて計算してみよう。

```fsharp
[| 1 .. N |]
|> Array.filter (fun x -> x % 3 = 0 || x % 5 = 0)
|> Array.sum
|> printfn "%d"
```

これで確かに解答が得られた。
[解答1](#解答1)として別途まとめておこう。


## 他の方針
F#のコードは既にかなり単純なので、単純化という意味ではあまりやりようがない。
ここでは単純化に限らずいくつか参考情報を書いておく。


### リスト内包表記
Pythonの時と同じような、配列に対する内包表記でフィルタリングする方法が分からない&忘れた。
代わりに`seq`版を紹介する。

まずは配列の場合の内包表記。

```fsharp
[| for x in 1 .. n -> x |] |> printfn "%A"
```

### `Seq`の利用
F#の`Seq`では遅延評価が使える。
場合によっては必要不可欠。
よくわからなければ配列と同じだと思っておけばいい。
きちんと理解するにはそれなりにいろいろなことを知らなければいけない。

`seq`のままだと途中で切れてしまったので`Seq.toArray`を挟んである。

```fsharp
seq {for x in 1..n do if x % 3 = 0 then yield x} |> Seq.toArray |> printfn "%A"
```

`seq`できちんと計算してみよう。

```fsharp
seq { for x in 1..N do if x % 3 = 0 || x % 5 = 0 then yield x}
|> Seq.sum
|> printfn "%d"
```

## 解答


### 解答1

```fsharp
[| 1 .. N |]
|> Array.filter (fun x -> x % 3 = 0 || x % 5 = 0)
|> Array.sum
|> printfn "%d"
```

### 解答2

```fsharp
seq { for x in 1..N do if x % 3 = 0 || x % 5 = 0 then yield x}
|> Seq.sum
|> printfn "%d"
```
