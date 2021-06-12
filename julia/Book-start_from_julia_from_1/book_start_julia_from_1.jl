# # 1 から始める Julia

# ## 数学系プログラムに関する注意
# - [Unicode の変数入力](https://docs.julialang.org/en/v1/manual/unicode-input/)

# %% とりあえず println
println("TEST1")
# %%

## 2. Julia の言語機能 P.17

##+
x = 1
y = 2.0
# REPL 上 ans は直前に評価した値を格納している変数
2x + 1 # 数値と文字の積で記号がなくても大丈夫
##+

# 次のように一部の掛け算記号が省略できます。
# Python だとこうはいかなかったことを思い出してください。

# %%
x = 7
3x + 7
2(x-3)^3 - 3(x-2) + 1
# 2 (x-3)^2 # 積の記号を省略するときは数と変数の間にスペースを入れてはいけない
# %%

# 変数名に Unicode が使えます。
# 物理・数学系のプログラムを書くことを考えるならかなり便利です。

# %%
θ = π/4
sin(θ)
cos(θ)
sin(π/2)
cos(π/2)
@show θ
# %%

# ## P.18 2.1.2 プリミティブ型
# Julia では変数宣言に型をつける必要こそないものの、型はあります。
# typeof 関数で確認できます。
# 符号つき整数もあります。

# %%
println(typeof(0)) # Int64
println(typeof(1.5)) # Float64
println(typeof("TEST")) # String

println(typeof(10000000000)) # Int64
println(typeof(10000000000000000000)) # Int128
println(typeof(1000000000000000000000000000000000000000)) # BigInt

println(typeof(0x1)) # UInt8
println(typeof(0x111111)) # UInt32
println(typeof(0x1111111111111)) # UInt64
println(typeof(0x11111111111111111)) # UInt128
# %%

# ### P.19 システムデフォルトの整数型

# %%
println(Sys.WORD_SIZE)
println(Int)
println(UInt)
# %%

# ### P.19 符号なし整数型

# %%
0x1
println(typeof(0x1))

0x123
println(typeof(0x123))

0x1234567
println(typeof(0x1234567))

0x123456789abcdef
println(typeof(0x123456789abcdef))
# %%

# ### P.20 浮動小数点数

# %%
println(typeof(1.0))
println(typeof(0.5f0))
println(2.5f-4)
println(typeof(2.5f-4))

a = 2.5f-4 # 0.00025f0
println(typeof(a)) # Float32
println(a) # 0.00025
# %%

# ### 無限大の値
# Inf16 などで無限大を表せます。
# Inf128 は未定義でエラーになります。

# %%
println(Inf16) # Int16 での無限大
println(Inf32) # Int32 での無限大
println(Inf64) # Int64 での無限大
# println(Inf128) # Int128 での無限大は未定義
# %%

# #### P.21 2.1.3 任意精度演算

# %%
x = 1234567890123456789012345678901234567890
println(x)
println(x + x)
typeof(x + x)
# %%

# #### 2.1.4 定数

# %%
const pt_const = 1.0
# pt_const = 2 # エラーになる

# はじめから入っている定数
println(pi)
println(VERSION)
# %%

# #### 2.1.5 基本的な演算子
println(4 / 2)
println(2 \ 4)
println(5 % 2)
println(4^2)

# #### P.22 2.1.6 更新演算子

# %%
x = 1
println(x)
println(x += 1)
println(x -= 1)
println(x *= 3)
println(x /= 3)
println(x \= 3)
x = 3
println(x ÷= 3)
println(x %= 3)
# %%

# #### P.22 2.1.7 複素数
# 虚数単位は `im` で表します。

# %%
println(1 + 2im)
println((1 + 2im) * (2 - 3im))
println(real(1 + 2im)) # 実部
println(imag(1 + 2im)) # 虚部
println(conj(1 + 2im)) # 複素共役
println(abs(1 + 2im))  # 絶対値
# %%

# #### P.23 2.1.8 文字列
# - 文字列: String 型
# - 文字列: Char 型
# %%
s = "Hello, World!"
println(typeof(s))
println(s[begin]) # begin で配列の最初の位置を表す：Julia だとふつう 1 はじまり
println(s[end])   # end で配列の最後の位置を表す
println(typeof(s[begin]))
println(s[begin:begin+4])
# %%

# ##### P.24 文字列連結
# Julia が数学を重視していることを表す特徴的な表記があるので、まずはそれを紹介します。

# %%
s1 = "Hello, "
s2 = "World!"
println(s1 * s2)
# %%

# 文字列の結合は非可換な演算であり、一般に非可換群の演算には積の記号を使います。
# 文字列の連結を「和」と捉えて `+` を使う言語も多い中、
# 積の記号を使うのは特徴的と言えるでしょう。
# 他の原語と足並みが揃っていないとも言え、
# 一般のプログラマーにとって直観的かどうかは微妙なところではあります。

# その他の結合の仕方や文字列の補間コードも紹介しておきます。

# %%
s1 = "Hello, "
s2 = "World!"

println(string(s1, s2))
println("$s1$s2") # 文字列補間
println("1 + 2 = $(1 + 2)") # コードの評価結果を文字列に埋め込める
# %%

# #### P.24 2.1.9 Unicode 文字列
# UTF-8 は文字を可変長で符号化するので全ての文字が同じバイト数で表現されるわけではなく、
# 文字列の配列のインデックス操作は難しいです。
# 対処法はすぐ後で紹介することにして、まずはバイト数の挙動を確認しましょう。

# %%
s = "test 楕円型非線型偏微分方程式"
println(s[begin]) # ここでは begin = 1
println(s[begin+5])
# println(s[begin+6]) # エラー
println(s[begin+8])

# nextind を使う
println(nextind(s, 1))
println(nextind(s, 2))
println(nextind(s, 3))
println(nextind(s, 4))
println(nextind(s, 5))
println(nextind(s, 6))
println(nextind(s, 7))

# Vector を使う
chars = Vector{Char}(s)
println(chars)
for i in 1:length(chars)
  println(string(i) * ": " * chars[i])
end
# %%

# #### P.26 2.1.10 文字列の関数
# ここでは一部しか紹介しません。
# 詳しくは公式ドキュメントやリファレンスを確認してください。

# %%
println(length("Julia"))    # 文字列の長さ
println(repeat("Julia", 2)) # 文字列の繰り返し
println(replace("Python is useful.", "Python" => "Julia")) # 文字列の置換
println(split("Julia-lang", "-"))  # 文字列の分割
println(startswith("JuliaLang", "Julia")) # 文字列の先頭が特定の文字列かどうか
println(startswith("JuliaLang", "julia")) # 大文字小文字も区別する
println(endswith("JuliaLang", "Lang"))    # 文字列の末尾が特定の文字列かどうか
println(endswith("JuliaLang", "lang"))    # 大文字小文字も区別する
println(join(["Julia", "Lang"], "-"))     # 指定の文字で文字列の配列を結合する
println(findfirst("Julia", "JuliaLang"))  # ある文字列を全体の文字列の中から探して見つかったら文字インデックスを返し、見つからなかったら nothing を返す
# %%

# #### P.27 2.1.11 正規表現

# %%
regex = r"J.*g"
println(typeof(regex))
m = match(regex, "JuliaLang is the best.")
println(m)
println(typeof(m))
println(m.match)
println(m.offset)
# %%

# ### P.28 2.2 制御構文

# #### P.28 2.2.1 条件評価

# %%
function ifcheck(x, y)
  if x < y "x is less than y"
  elseif x > y "x is greater than y"
  else "x is equal to y"
  end
end
ifcheck(3, 2)
ifcheck(2, 3)
ifcheck(3, 3)

x = 1
print(x > 100 ? true : false) # 三項演算子利用
print(x < 100 ? true : false)
# %%

# #### P.29 2.2.2 短絡評価
# %%
function if_and_error(n)
  n <= 0 && error("n must be be positive")
end
println(if_and_error(1))
# println(if_and_error(-1)) # （正しく）エラーが出る
# %%

# #### P.30 2.2.3 ループ

# %%
# while ループ
i = 1
while i <= 5 println(i); global i += 1; end

# for ループ
for i = 1:5 println(i^2) end
println(map(x -> x^2, 1:5))
# %%

# #### P.31 2.2.4 try/catch/finally

# %%
function try_check(str)
  i = 0
  try
    i = parse(Int, str) # 文字列を Int に変換
  catch
    println("ERROR!")
  end
  return i
end
try_check("11")
try_check("test")
try_check(2)
# %%

# %%
# ファイル出力
open("file.tmp.txt", "w") do out
  println(out, "test")
end
# ファイル入力
open("file.tmp.txt", "r") do f
  a = readline(f)
  println(a)
end
# %%

# ### P.32 2.3 関数

# %%
function add(x, y)
  return x + y
end
println(add(1, 2))

function add(x, y)
  x + y # 最後の式が自動で return される
end
println(add(1, 3))

add(x, y) = x + y # 1 行で書いてもいい
println(add(1, 5))

add_typed(x :: Int, y :: Int) = x + y # 型をつけてもいい
println(add_typed(1, 6))

add_typed(x :: Float64, y :: Float64) :: Int = x + y
println(add_typed(1.0, 7.0))
# println(add_typed((1.2, 7.2))) # 型変換できないのでエラー

sum_diff(x, y) = (x+y, x-y)
println(sum_diff(3.2, 4.3))
println(typeof(sum_diff(3.2, 4.3)))
# %%

# #### P.34 2.3.1 可変長引数

# %%
function f(x...)
  reduce(+, x)
end
println(f(3))
println(f(3, 4))
println(f(3, 4, 5))
# %%

# 可変長引数は最後の引数にだけ設定できます。

# %%
g(x, y...) = (x, y...)
println(g(3, 4, 5))
# g(x..., y) = (x..., y) # エラー
# %%

# `rand()` 関数は引数に合わせた次数の高次元配列を返します。

# %%
println(typeof(rand(4)))
println(typeof(rand(4, 3)))
println(typeof(rand(4, 3, 7)))
# %%

# #### P.36 オプショナル引数

# %%
f(x, y = 1) = x + y
println(f(3))    # y は省略できて初期値が自動的に入る
println(f(3, 3)) # y を入力すると初期値が上書きされる

f(x, y = 1, z = 2) = x + y + z # 後半の引数には設定できる
# f(x = 2, y, z) = x + y + z # 先頭だけに指定するとエラー
# %%

# #### P.37 2.3.3 キーワード引数
# 名前指定で呼び出せる引数です.
# オプショナル引数との違い・使い分けについて簡単に書いておきます.
# 引数の数が多い場合はキーワード引数を使う方が可読性が高くお勧めです.
# 特に引数の順序に明確な意味がない場合はキーワード引数を使いましょう.
# 逆に引数が少なく順序・意味が明確ならオプショナル引数の方が楽です.

# %%
function add(x, y; z = 3, w = 4)
  x + y + z + w
end
println(add(1, 2))
println(add(1, 2, z = -3))
println(add(1, 2, z = -3, w = 0))
# %%

# #### P.37 匿名関数
# ほかの言語ではラムダと呼ばれたり書かれることもあります.

# %%
square = x -> x * x
println(square(2))

# 複数行のラムダも定義可能
square = x -> begin
  x * x
end
println(square(2))

println(map(x -> x * x, 1:5)) # 高階関数と組み合わせる
println(filter(x -> x % 2 == 1, 1:5))
# %%

# ### P.38 2.4 型
# 次のような特徴があります.
# - 動的型付けである.
# - 言語標準機能として型注釈可能. (optional typing.)
# - 多重ディスパッチで動的にメソッド呼び出し.
# - 実行時にコンパイルされる. (JIT コンパイル.)
# 他に一般的なオブジェクト指向言語とは次のような違いがあります.
# - オブジェクトにメソッドを追加できない.
# - 型の階層関係はあっても継承はない.
# Julia の型注釈には 2 つのメリットがあります.
# - JIT コンパイルによるコード最適化で実行速度向上.
# - 多重ディスパッチが使える
# 実行速度に関しては型注釈したからと言って速くなるわけではなく,
# Julia の事情をきちんと理解した高速化対応が必要です.
# 多重ディスパッチについては実行時に型に応じた適切な関数が呼ばれる仕組みです.
# オーバーロードと似ていますが, 動的に型に応じた関数を呼んでくれる点に違いがあります.

# #### P.40 2.4.1 型の宣言
# `x::Int` のように `::` 演算子を使います.

# %%
function add_typed(x::Int, y::Int)
  x + y
end
println(add_typed(1, 2))
# println(add_typed(1.0, 2)) # x の型エラー
# %%

# ##### P.41 プリミティブ型のカスタム定義
# `base/boot.jl` も参考になるでしょう.

# %%
primitive type Float16 <: AbstractFloat 16 end
# %%

# #### P.41 2.4.2 型の階層関係
# 型に親子関係があり, 型をノードとするグラフが構成されています.
# 具体型 (concrete type) と抽象型 (abstract type) があります.
#
# 抽象型は次のように宣言します.
# ```julia
# abstract type <name> end
# abstract type <name> <: <supertype> end
# ````
#
# 親の型は `supertype` 関数で確認できます.

# %%
println(supertype(Float32))
println(supertype(Real))
println(supertype(Number))
# %%

# 子の型も確認できます.

# %%
println(subtypes(Number))
println(subtypes(Real))
println(subtypes(AbstractFloat))
println(subtypes(Integer))
# %%

# #### 2.4.3 `Nothing` 型
# `nothing` というシングルトンオブジェクトを持ちます.

# %%
x = nothing
println(typeof(x))
println(typeof(nothing))
println(isnothing(x))

function ret_nothing() return end # 関数の戻り値を return だけにすると nothing を返す
println(ret_nothing())
function ret_nothing2() end # 関数の戻り値を指定しないと nothing を返す
println(ret_nothing2())
# %%

# 他にも関数の返り値に指定されていることがあります.

# %%
println(findfirst("e", "Hello, World!"))
println(findfirst("j", "Hello, World!"))
# %%

# #### P.44 2.4.4 複合型 (composite types)
# 他の言語で構造体・辞書と呼ばれる型と思えばいいでしょう.
# `struct` キーワードで宣言し, 具体型として定義されます.
# 具体型を親とする型を定義できないので複合型は型の継承の親になれません.
# 説明のために 1 つ具体例を定義しておきます.

# %%
module P044
  struct Point
    x::Int
    y::Int
  end
end
p = P044.Point(2, 3)
println(p)
println(typeof(p))
println(p.x)
println(typeof(p.x))
println(p.y)
# %%

# 上の `Point` での `x`, `y` をフィールドと呼び,
# 必要に応じて型注釈をつけられます.
# フィールドはドット演算子 `p.x` のように書けばアクセスできます.
# 型宣言するとコンストラクターが自動で作られます.
# コンストラクターは別途定義できます.
# 多重ディスパッチでいい具合に多重定義できるので名前をかぶせても構いません.

# %%
module P046
  struct Point
    x::Int
    y::Int
  end
  Point(x) = Point(x, 0)
end
p = P046.Point(1)
println(p)
println(typeof(p))
# %%

# `struct` 宣言された型は immutable です.

# %%
module P046_1
  struct Point
    x::Int
    y::Int
  end
end
p = P046_1.Point(1, 2)
# p.x = 2 # エラーになる
# %%

# mutable な複合型も定義できます.

# %%
module P046_2
  mutable struct MutablePoint
    x::Int
    y::Int
  end
end
mp = P046_2.MutablePoint(1, 2)
mp.x = 3
println(mp)
# %%

# #### P.47 2.4.5 Union 型
# 特殊な抽象型で, 複数の型の和集合を表します.
# 例えば Haskell で `type A = Int | String` などと書ける型のことです.
# よく使われるのは `Union{T, Nothing}` という型です:
# ここの `T` は `Int` や `Float32` などの具体的な型が入ります.
# いわゆる `Option` 型にあたります.

# %%
IntOrString = Union{Int, String}
union_type(x::IntOrString) = x
println(union_type(1))
println(union_type("TEST"))
# %%

# #### P.47 2.4.6 パラメトリック型
# 他の言語ではジェネリクスやテンプレートと呼ばれている機能の類似です.
# 先ほどの `Point` 型を変えてみましょう.

# %%
module P047
  struct Point{T}
    x::T
    y::T
  end
end
println(P047.Point(2, 3))
println(P047.Point(2.0, 3.0))
# %%

# 関数にも型パラメータを持たせられます.

# %%
module P048
  struct Point{T}
    x::T
    y::T
  end
  function distance(p::Point{T}) where T
    println(T) # 実行時には T が指定されているので具体的な型が出力できる
    sqrt(p.x^2 + p.y^2)
  end
end
p = P048.Point(3, 4)
println(P048.distance(p))
# %%

# 型が違う値に対して違う動作を実装したいときは多重ディスパッチを使います.

# %%
module P049
  struct Point{T}
    x::T
    y::T
  end
  function distance(p::Point{Float64})
    println("Float64")
    sqrt(p.x^2 + p.y^2)
  end
  function distance(p::Point)
    println("NOT Float64")
    sqrt(p.x^2 + p.y^2)
  end
end
println(P049.distance(P049.Point(3, 4)))
println(P049.distance(P049.Point(3.0, 4.0)))
# %%

# 型パラメータを複数つけることもできます.

# %%
module P050
  struct Point2{T1,T2}
    x::T1
    y::T2
  end
  function distance(p::Point2{T1,T2}) where {T1,T2}
    sqrt(p.x^2 + p.y^2)
  end
end
println(P050.distance(P050.Point2(2, 3.0)))
# %%

# #### P.50 2.4.7 パラメトリック型の階層関係
# 型の階層関係は `<:` 演算子で確認できました.
# 改めて確認してみましょう.

# %%
println(Int <: Number)
println(Int >: Number)
println(Int <: Float64)
# %%

# パラメトリック型の階層関係を確認しましょう.

# %%
module P052_1
  struct Point
    x::Int
    y::Int
  end
end
module P052_2
  struct Point{T}
    x::T
    y::T
  end
end
println(P052_2.Point{Int} <: P052_1.Point)          # false # 本と違うがどう理解すればいいか？
println(P052_2.Point{Int} <: P052_2.Point{Number})  # false
println(P052_2.Point{Int} <: P052_2.Point{Float64}) # false
# %%

# Julia のパラメトリック型は不変なので、それに合わせて関数の引数の型定義も工夫が必要です.

# %%
module P052
  struct Point{T}
    x::T
    y::T
  end
  # エラーになる
  function distance(p::Point{Number})
    sqrt(p.x^2 + p.y^2)
  end
  # 型パラメータに制約をつける
  function distance(p::Point{<:Number})
    sqrt(p.x^2 + p.y^2)
  end
  # 上と同じ
  function distance(p::Point{T}) where T <:Number
    sqrt(p.x^2 + p.y^2)
  end
end
p = P052.Point(1, 2)
println(P052.distance(p))
# %%

# #### P.52 2.4.8 抽象型のパラメトリック型

# %%
module P052_2
  abstract type AbstractPoint{T} end;
  struct Point2D{T} <: AbstractPoint{T}
    x::T
    y::T
  end
end
println(P052_2.Point2D{Int} <: P052_2.AbstractPoint)         # true
println(P052_2.Point2D{Int} <: P052_2.AbstractPoint{Int})    # true
println(P052_2.Point2D{Int} <: P052_2.AbstractPoint{Number}) # false
# %%

# ### P.53 2.5 コレクション
# 配列・タプル・リスト・辞書・集合などを指します.
# DataStructures.jl にはヒープや優先度つきキューのような標準ライブラリにはないデータ構造があります.
# アルゴリズムの勉強をするときには参考になるはずです.

# #### P.53 関数名についた `!`
# Julia の慣例として引数として与えた変数を変更する関数には `!` をつける慣習があります.
# 特に mutable なコレクションには要素の追加・削除機能があり,
# そうした機能に対応する関数には `push!` や `insert!` のように `!` がついています.

# #### P.54 2.5.1 タプル
# タプル (Tuple) は immutable です.
# ちょっとしたプログラムで関数の返り値として多値を返したいときに便利です.
# 少し凝ったプログラムを書くときは struct や配列を使うことを勧めます.
# 状況によっては次に紹介する名前付きタプルでもいいでしょう.

# %%
t = (1,2,3)
print(typeof(t))
print(t)
print(t[1])
# t[1] = 4 # エラー
# %%

# タプルは `Array` 型のサイズを表すのに使われています.

# %%
a = rand(4, 3)
println(typeof(a))
println(size(a))
# %%

# 関数の可変長引数は `Tuple` 型のオブジェクトです.

# %%
f(x...) = x
y = f(1,2,3)
println(typeof(y))
# %%

# #### P.55 2.5.2 名前付きタプル (named tuple)
# 命名通り、各要素に名前を付けられるタプルです.
# 簡単なスクリプトを使うときのようにあくまで一時的に使うだけにして,
# 本格的に使う場合は `struct` を使うのが無難です.

# %%
module P055
  nt = (a = 1, b = 2, c = 3)
end
println(typeof(P055.nt))
println(P055.nt.a)
println(P055.nt[begin+1]) # P055.nt[2]
println(P055.nt[:c]) # シンボルによる呼び出し
println(keys(P055.nt))   # キーを取得
println(values(P055.nt)) # 値を取得
# %%

# #### P.56 2.5.3 リスト
# Julia では 1 次元の配列をリストとみなします.
# 値の追加・削除には `push!``, `pop!`, `insert!`, `deleteat!` を使います.
# Julia では `!` があるメソッドは元のオブジェクトを破壊的に変更します.
# 先頭に要素を追加したときは `pushfirst!`,
# 先頭の要素を取り出すときは `popfirst!` を使います.
# DataStructures.jl にはスタック・キュー・両端キュー (deque) などがあるので,
# 必要に応じて眺めてみましょう.

# %%`
module P057
  list = []
  list2 = [1, 2]
end
println(typeof(P057.list))
println(typeof(P057.list2))
println(push!(P057.list2, 3)) # list2 に破壊的に要素を追加
println(P057.list2) # 破壊的変更を確認
println(pop!(P057.list2))
println(P057.list2) # 破壊的変更を確認
println(insert!(P057.list2, 2, 4))
println(P057.list2) # 破壊的変更を確認
println(deleteat!(P057.list2, 2))
println(P057.list2) # 破壊的変更を確認
# %%

# #### P.58 2.5.4 辞書
# キーから値を検索したいときに使います.
# `IdDict` や `WeakKeyDict` のような型もあるので必要に応じて調べて使ってみてください.

# %%
module P058
  d = Dict{String, Int}() # 初期化
end
println(typeof(P058.d))
println(P058.d)
P058.d["a"] = 1; P058.d["b"] = 2
println(P058.d)
println(P058.d["a"])
# println(P058.d["c"]) # エラー
P058.d["a"] = 3 # 破壊的に変更される
println(P058.d)

println(haskey(P058.d, "a")) # キーの存在確認
println(haskey(P058.d, "c"))
# %%

# #### P.59 2.5.5 辞書
# キーだけを格納する辞書と思えばいいでしょう.
# 重複なしで保存できます.
# 包含の記号 ⊆ をコードで使えます.
# REPL で \subseteq と入力すれば使えます.
# 記号については[レファレンス](https://docs.julialang.org/en/v1/manual/unicode-input/)を見てください.

# %%
module P059
  s = Set([1, 2])
end
println(typeof(P059.s))
println(P059.s)
println(push!(P059.s, 3))
println(union(P059.s, [3, 4])) # ! がないので s への破壊的変更ではない
println(P059.s)                # 破壊的変更ではないことを確認
println(intersect(P059.s, [3, 4])) # ! がないので s への破壊的変更ではない
println(P059.s)                    # 破壊的変更ではないことを確認

println(issubset([1, 2], [1, 2, 3]))
println([1,2] ⊆ [1,2,3]) # REPL で \subseteq と入力すると ⊆ が出せ, 包含を表すのに使える
# %%

# #### P.60 2.5.6 コレクション共通の関数
# 以下で紹介した以外にもいろいろな関数があります.
# 一方で特定のコレクションにしか当てられない関数もあります.
# 詳しくは公式ドキュメント・リファレンスを確認してください.

# %%
println(isempty([]))
println(isempty(1:3))
a = [1,2,3]
println(empty!(a))
println(length(1:3))
println(eltype(1:3))
# %%

# #### P.60 2.5.7 コレクションのイテレーション
# `for` 文による処理が一般的です.
# `map` などの高階関数を使った処理もあります.
# 自作のコレクション型をイテレーションするには `iterate` 関数を実装する必要があります.

# %%
d = Dict("a" => 1, "b" => 2, "c" => 3)
for (k, v) in d println(k, v) end
# map(x -> println(x), d) # エラー
# map(println, keys(d)) # エラー
map(println, values(d))
# %%

# ### P.62 2.6 多次元配列
# Julia では `Array{T,N}` 型で高次元の配列が標準で用意されています.
# `T` は要素の型で, `N` は次元です.
# 高次元配列は内部的には 1 次元の配列を使っています.

# Julia の `Array` 型は MATLAB の高次元配列を参考にしていて,
# 関数名も MATLAB と同じ名前が多く,
# Python の NumPy とは違う部分が多くなっています.
# NumPy の高次元配列はインデックスが 0 はじまりで row-major order である一方,
# Julia は 1 はじまりで column-major order です.
# 同じ列の要素を列挙するのは速い一方,
# 同じ行の要素の列挙は遅いです.

# Julia には `Vector{T}` と `Matrix{T}` 型はあるものの,
# それぞれ `Array{T,1}`, `Array{T,2}` のエイリアスでしかありません.

# #### P.63 2.6.1 初期化
# 例えば次のような関数があります.
#
# - `Array`, `zeros`, `ones`, `rand`, `randn`, `fill`, `fill!`, `similar`, `reshape`, `copy`, `deepcopy`
#
# 実際に使ってみましょう.

# %%
# 値が初期化されていない配列
show(stdout, "text/plain", Array{Float32}(undef, 3, 2))
# 0 で初期化
show(stdout, "text/plain", zeros(Float32, 3, 2))
# 1 で初期化
show(stdout, "text/plain", ones(Float32, 3, 2))
# 一様分布で初期化
show(stdout, "text/plain", rand(Float32, 3, 2))
# 正規分布で初期化
show(stdout, "text/plain", randn(Float32, 3, 2))
# 値を 1.1 で初期化
show(stdout, "text/plain", fill(1.1, 3, 2))
# ある配列と類似の配列：値は初期化されない
A = rand(Float32, 3, 2)
show(stdout, "text/plain", A)
show(stdout, "text/plain", similar(A))
show(stdout, "text/plain", similar(A, Float64))
@show A
# 配列のサイズを変える
A = rand(Float32, 3, 2)
show(stdout, "text/plain", A)
show(stdout, "text/plain", reshape(A, 2, 3))
# %%

# #### P.66 2.6.2 基本的な操作
# 配列（`Array` 型）に関する情報を調べる主な関数を紹介し,
# 実際に使ってみます.
# 完全な情報は公式のリファレンス・ドキュメントを参照してください.

# - `eltype(A)`: `A` の要素の型
# - `length(A`: `A` の要素数
# - `ndims(A)`: `A` の次元
# - `size(A)`: `A` のサイズ
# - `size(A, n)`: `n` 番目の次元での `A` のサイズ（行列でいう何行何列か）
# - `strides(A)`: `A` のストライド（要素同士が 1 次元配列上でいくつ離れているか)
# - `stride(A, n)`: `n` 番目の次元での `A` のストライド

# %%
A = rand(Float32, 3, 2)
show(stdout, "text/plain", A)
println("配列の要素の型: $(eltype(A))")
println("配列の要素数: $(length(A))")
println("配列の次元: $(ndims(A))")
println("配列のサイズ（何行何列か）: $(size(A))")
println("配列のサイズ: $(size(A, 1))")
println("配列のサイズ: $(size(A, 2))")
println("配列のストライド: $(strides(A))")
println("配列のストライド: $(stride(A, 1))")
println("配列のストライド: $(stride(A, 2))")
# %%

# #### P.67 2.6.3 インデクシング
# 配列の値を取ったり代入したりする操作のことです.

# %%
A = collect(reshape(1:9, 3, 3)) # 1:3 を (3,3) 行列に変換している
B = collect(reshape(1:8, 4, 2))
A[3, 3] = -9
show(stdout, "text/plain", A)
A[1:2, 1:2] = [-1 -4; -2 -5]
show(stdout, "text/plain", A)
# %%

# #### P.68 2.6.4 多次元配列の演算
# 基本的な四則演算があります.
# 特にいろいろな行列演算が定義されています.

# %%
A = [
  1 0
  0 3
]
Ainv = inv(A)
B = [
  1 2
  0 3
]
Binv = inv(B)
c = 3

show(stdout, "text/plain", A + B)
show(stdout, "text/plain", A - B)
show(stdout, "text/plain", A * B)
show(stdout, "text/plain", A * c)
show(stdout, "text/plain", c * A)
show(stdout, "text/plain", A / B)
show(stdout, "text/plain", A * Binv)
show(stdout, "text/plain", A \ B)
show(stdout, "text/plain", Ainv * B)
# %%

# アダマール積のように配列の要素レベルで演算したい場合はドット演算子を使います.

# %%
A = [
  1 2
  3 4
]
B = [
  5 6
  7 8
]
c = 10
show(stdout, "text/plain", A .+ B)
show(stdout, "text/plain", A .+ c)
show(stdout, "text/plain", A .- B)
show(stdout, "text/plain", A .- c)
show(stdout, "text/plain", A .* B)
show(stdout, "text/plain", A .* c)
show(stdout, "text/plain", A ./ B)
show(stdout, "text/plain", A .\ B)
show(stdout, "text/plain", A ./ c)
# %%

# #### P.68 2.6.5 ブロードキャスティング
# 効率的にサイズが違う配列を演算する仕組みです.
# 具体的には (100, 100) 行列 `A` のすべての要素に 1 を足したいとき,
# いちいち (100, 100) 行列 `B` を作るのでは効率がよくありません.
# これに対処するのがブロードキャスティングです.
# もちろん全ての別サイズの配列同士に対してブロードキャストが発動するわけではありません.
# 次のどちらかの条件が必要です.
#
# - 配列同士の各次元の大きさが同じ（これが成り立つなら当然できる）
# - 次元の大きさが違う場合、片方のサイズが 1

# %%
# 表示がうっとうしくなるので行列のサイズを小さくしておく
A = rand(Float32, 2, 2)
# 次数が大きくなるほど効率が悪くなる
B = ones(Float32, 2, 2)
show(stdout, "text/plain", A + B)

# ブロードキャストを使う例
show(stdout, "text/plain", A .+ 1)
# %%

# (2,2) 行列 `A` に (1,2) の行ベクトルを足すときにもブロードキャストが使えます.

# %%
A = rand(Float32, 2, 2)
B = ones(Float32, 1, 2)
show(stdout, "text/plain", A .+ B)

# 足され方を調べるために他の行列を定義してみる
A = ones(Float32, 2, 2)
B = [
  1 2
]
show(stdout, "text/plain", A .+ B) # きちんと各行に足されている
# %%

# ブロードキャストする関数 `broadcast` `broadcast!` もあります.
# 一般に `broadcast(f, A, B, ...)` で `f` 以降は可変長引数です.
# さらに `f.(A, B)` としてもブロードキャストできます.

# %%
A = ones(Float32, 2, 2)
B = [
  1 2
]
show(stdout, "text/plain", A .+ B)
show(stdout, "text/plain", broadcast(+, A, B))
# %%

# `f.(args)` 型のブロードキャストの実例を見ましょう.
# `map` を使っても同じ結果になりますが, ドット演算の方が楽に書けます.

# %%
sigmoid(x) = 1.0 / (1.0 + exp(-x))
println(sigmoid(0.0))
println(sigmoid(1.0))
A = [
  0 1
  1 0
]
show(stdout, "text/plain", sigmoid.(A))
show(stdout, "text/plain", map(sigmoid, A)) # map を使っても同じことはできる
# %%

# #### P.71 2.6.6 map, reduce, filter
# よく高階関数と呼ばれます.
# 慣れると便利です.
# パイプライン `|>` もうまく使うと楽しくなります.

# %%
A = rand(3, 2)
show(stdout, "text/plain", A)
show(stdout, "text/plain", map(x -> x + 1, A)) # 各要素に +1
show(stdout, "text/plain", reduce(*, A)) # 全ての要素の積
show(stdout, "text/plain", filter(x -> x < 0.5, A))
# %%

# #### P.72 2.6.7 サブ配列
# `view` 関数を使って作ります.
# サブ配列は元の配列への参照とインデックスの情報を持っていて,
# 値を直接持っているわけではありません.
# もとの配列の中身が変わるとサブ配列の値も変わります.
# 最近は配列のコピー操作は速くなってきていて,
# ふつうのインデクシングで新しい配列を作る方が速いことも多いので,
# 実測してパフォーマンスをきちんと調べるのが大事です.

# %%
A = rand(3, 3)
show(stdout, "text/plain", A)
show(stdout, "text/plain", view(A, 1, 2:3)) # どこを抜かれているかは比較して見てみよう
# %%

# ### P.73 2.7 モジュール
# 実際に大きなプログラムを書くときに使います.
# 名前空間という概念が大事で名前の衝突回避のためにも使えるので,
# 本やコンテンツの写経をするときにも便利です.

# #### P.73 2.7.1 モジュールの機能
# 他の言語と同じように思えばいいでしょう.
# いまのところモジュール内の大域的な名前をモジュール外から参照できなくする方法がありません.

# Julia のコードはいつもモジュールに関連づいた名前空間の中で実行されます.
# 有効になっているモジュールは `@__MODULE__` マクロで確認できます.

# %%
println(@__MODULE__)
println(gcd(12, 8))
# %%

# #### P.74 2.7.2 既存モジュールの利用
# `using` 文を使います.

# %%
# mean([1, 2, 3]) # そんなものはないと怒られる
module P075
using Statistics: mean, std
function meansample()
  mean([1, 2, 3])
end
function stdsample()
  std([1, 2, 3])
end
end
println(P075.meansample())
println(P075.stdsample())

# モジュール自身を取り込む
using Statistics: Statistics
println(Statistics.mean([1, 2, 3]))
# mean([1, 2, 3]) # mean は取り込んでいないので怒られる
# %%

# #### P.76 2.7.3 using 文の注意
# よくある import の注意と同じです.
# 一般には余計な関数を取り込むのは行儀が悪いとされているので,
# 取り込む関数はきちんと限定するべきです.
# モジュール自身を取り込む形にするのも一手です.
# Julia では `using` と `import` があり,
# 適切な使い分けが必要です.
# 多重ディスパッチに関しては 2.7.7 項に記述があります.

# #### P.76 2.7.4 新しいモジュールの定義
# モジュール名は慣習として先頭の文字は大文字です.
# 同じく慣習として `module` と `end` の間でインデントはしません.

# %%
module P077
hello(name) = println("Hello, $(name)")
end
P077.hello("Julia")
# %%

# 以前 `using` した `Statistics` では `export` で `mean`, `std` をエクスポートしています.
# わからない場合は例を作ってみましょう.

# %%
module P078
export hello
hello(name) = println("Hello, $(name)")
goodbye(name) = println("Goodbye, $(name)")
end
using P078
# %%

# #### P.79 2.7.5 moduleの相対パス指定
# `using` や `import` 文で指定したモジュール名は基本的に変数 `LOAD_PATH` 内のプロジェクトやパスから拾います.
# 詳しくは 2.11 節で議論されるので,
# ここでは相対パスによるモジュールの指定を説明します.

# `module` 文を使って定義した直後のモジュールは `using` 文を使って読み込もうとすると失敗します.
# 以下の `using P079` が `LOAD_PATH` から `P079` というモジュール名を探すからです.

# %%
module P079
export hello
hello(name) = println("Hello, $(name).")
end
# using P079 # エラー
# 次のように . を使って相対パスで読み込む
using .P079
hello("Julia")
# %%

# `.ModuleName` は自分の子モジュールを指定する相対パスです.
# 適切にドットをつければ適切な位置のモジュールが指定できます.

# #### P.80 2.7.6 ファイルの分割
# ソースコードが長くなると管理しづらくなるので適当な大きさで分割しましょう.
# include でモジュールを複数のファイルに分けて定義できます.
# ふつうは相対パスでファイルを指定します.

# %%
module P080
include("P080_1.jl")
include("P080_2.jl")
# include("file1.jl") # 存在しないファイルを読み込むと怒られる
end
P080.P080_1.hello()
P080.P080_2.hello()
# %%

# #### P.81 2.7.7 他のモジュールで定義された関数の拡張
# 多重ディスパッチをうまく使うと関数を外から拡張できます.
# `using` 文で取り込んだ関数は拡張できない一方,
# `import` 文で取り込んだ関数は拡張できます.

# %%
# 長さを 3 に固定したベクトルの定義
struct MyVec3{T} <: AbstractVector{T}
  x::T
  y::T
  z::T
end
# length を拡張する
import Base: length
length(v::MyVec3) = 3
# length の拡張その 2
Base.length(v::MyVec3) = 3 # この書き方を使うことも多い
# %%

# ### P.82 2.8 メタプログラミング
# プログラムを使ってプログラミングする手法です.
# 特にプログラムによるコードの生成や書き換えを指します.
# これだけ聞いて意味がわかるわけもないので実例を見るのが大事です.
# Julia はメタプログラミングを比較的よく使う言語です.
# よくあるのはマクロで `@assert` のような `@` ではじまるのはマクロ呼び出しです.

# #### P.83 2.8.1 シンボル
# シンボルはデータ型の一種で,
# 処理系の内部で使われる名前 (識別子) に対応します.
# メタプログラミングではシンボルをよく使います.
# シンボルの型名は `Symbol` です.
# `:foo` のように名前の前にコロンをつければ作れます.

# %%
:foo
typeof(ans)
typeof(:foo)
Symbol("foo") # これは :foo と同じ
Symbol("foo", :bar, 9) # 結合したシンボルができる
# %%

# シンボルには同じ名前を示すオブジェクトは 1 つしか作られます.
# オブジェクトの同一性は `===` で確認できます.

# %%
println(:foo === Symbol("foo"))
# %%

# #### P.83 2.8.2 構文木の表現
# コードは Julia の処理系に読み込まれると構文解析されて抽象構文木
# (abstract syntax tree, AST) に変換されます.

# 構文木は Julia のオブジェクトとして扱えます.
# `:()` で Julia のコードを囲むと Julia の処理系はコードを実行せずに構文をオブジェクトとして取り出します.
# この操作をクオートと呼びます.
# 1 行の単純なコードのクオートは `:()` である一方,
# 複数行の場合は `quote` キーワードを使います.
# `quote` と `end` で囲むとクオートできます.
# `dump` を使うと内部構造が見られます.

# %%
ex = :(2x + 1)
println(ex)
ex = quote 2x+1 end
dump(ex)
# %%

# %%
dump(:(x = 10))
# %%

# Julia のコードと構文木の対応を見るには `dump` 関数の結果を観察するといいでしょう.
# いろいろなコードで構文木を `dump` で見てみましょう.
# 公式マニュアルにも構文木の解説があるので必要に応じて公式を参照してください.

# #### P.86 2.8.3 構文木の補間
# クォートで作った構文木には別の構文木やリテラルを埋め込めます.
# これを補間と呼びます.

# %%
ex = 1
println(:(2x + $(ex))) # 整数リテラルの補間
ex = :(3y + 1)
println(:(2x + $(ex))) # 構文木の補間
# %%

# `$(func(ex))` のように関数を適用した結果で補間できます.

# 補間するものがシンボルの場合には注意が必要です.
# 変数 `ex` が `:y` のようなシンボルがあるとき,
# `$(ex)` の結果は `:y` ではなく `y` になります.
# メタプログラミングではシンボルは変数などの識別子として補間される方が利便性が高いようです.
# シンボルのリテラルとして補間したい場合は `QuoteNode` でクォートします.

# %%
ex = :y
println(:(2x + $(ex)))
# %%

# #### P.86 2.8.4 構文木の評価
# 構文木は `eval` 関数で評価できます.

# %%
x = 10
println(:(2x + 1))
eval(:(2x + 1))
dump(:(2x + 1))
# %%

# いつでもモジュールのグローバルスコープで評価されます.
# 例えば関数の中で `eval` を呼び出してから構文木を評価しても,
# その構文木はローカルスコープの変数とは干渉しません.

# %%
function test()
  x = "local"
  eval(:(x = "global")) # Main モジュールで実行されてグローバル変数 `x` を定義する
  println(x)
end
test()
println(x) # 関数の外からでも参照できてしまう
# %%

# `eval` での簡単なメタプログラミング例を見てみましょう.

# %%
for (i, name) in enumerate([:A, :B, :C])
  eval(:(const $(Symbol(:FLAG_, name)) = $(UInt16(1) << (i-1))))
end
println([FLAG_A, FLAG_B, FLAG_C])
# %%

# `@eval` マクロを見てみましょう.
# これは与えられたコードをクォートしてその結果を `eval` 関数で評価するので,
# `eval` 関数で必要なクォートが省略できます.
# 上の例を書き換えてみます.

# %%
for (i, name) in enumerate([:A, :B, :C])
  @eval const $(Symbol(:FLAG_, name)) = $(UInt16(1) << (i-1))
end
println([FLAG_A, FLAG_B, FLAG_C])
# %%

# #### P.88 2.8.5 マクロの機能
# マクロは与えられたコードを別のコードに変換してから実行する仕組みで,
# Julia ではよく使われます.
# `@` を使って呼び出す点が関数呼び出しと違います.

# 展開の結果は `@macroexpand` マクロで確認できます.
# これは与えられた式にあるマクロ呼び出しを展開した構文木を返します.
# `@assert` マクロの呼出を展開した結果を見てみましょう.

#%%
@macroexpand @assert x > 0
#%%

# コンパイルのかなり早い段階でマクロ呼出が展開されます.
# 2.8.7, 2.8.8 項でもう少し触れます.

# マクロの呼び出しには次の 2 つの方法があります.
#
# - `@macro(ex1, ex2, ex3)`
# - `@macro ex1 ex2 ex3`
#
# 要は括弧の有無です.
# 両者は構文解析の方法が違うだけで機能は同じです.
# 必要がなければ括弧は省略します.
# 括弧を省略したときの挙動は具体的に確認してみましょう.

# %%
x = 1
y = 2
println(:(@macro x + y) == :(@macro(x + y)))
println(:(@macro x +) == :(@macro(x, +)))
dump(:@macro x + y)
dump(:(@macro(x + y)))
dump(:(@macro x +))
dump(:(@macro(x, +)))
# %%

# 文字列だけを受け取るマクロもあり, 非標準文字列リテラル (non-standard string literal) と呼ばれます.
# このマクロはマクロ名が `_str` で終わり,
# `@macro_str` を `macro"..."` のようにふつうの文字列の前に `_str` を除いたマクロをつけて呼び出せます.
# この種のマクロは特定のオブジェクトのリテラル定義で使われていて,
# 正規表現のリテラルがこの 1 例です.

# #### P.90 2.8.6 標準ライブラリにあるマクロ
# 標準ライブラリのマクロでも既にいろいろな機能があります.
# まずは次の 4 種類にわけて認識するといいでしょう.
#
# - コンパイラへのヒント: `@inbounds`, `@inline`, `@fastmath` など
# - 構文の拡張: `@assert`, `@enum`, `@view` など
# - 開発の補助: `@less`, `@time`, `@code_typed` など
# - 特殊なリテラル: `@r_str`, `@big_str` など

# ##### コンパイラヒントを出すマクロ
# 構文木に最適化のヒントなどを差し込むマクロです.
# 例えば `@inbounds` は実行時の配列要素の参照チェックを省かせるマクロで,
# `@inline` は関数を積極的にインライン化させるマクロで,
# `@fastmath` は浮動小数の計算で IEEE754 の制約を超えて最適化させるマクロです.

# ##### 構文拡張マクロ
# 処理の自動化マクロです.
# 例えば `@assert` は与えられた式の条件が成立するか実行時にチェックし,
# 条件が成り立たなければ `AssertionError` 例外を出し,
# `@enum` マクロは C 言語の `enum` 構文にあたる機能を提供し,
# `@view` マクロは `X[i:]` などの配列の一部をコピーする構文で,
# コピーではなくその部分への参照を作るようにします.
# これは `view` 関数のマクロ版です.

# ##### 開発補助のマクロ
# おもに REPL で使うマクロです.
# 例えば `@less` マクロは関数呼出の式を受け取って呼び出されるメソッドのソースコードを表示し,
# `@time` マクロは処理を受け取って実行にかかた時間やメモリ使用量を表示し,
# `@code_typed` マクロは関数呼出の式を受け取って型推論の結果を表示します.

# ##### 特殊なリテラルを定義するマクロ
# 非標準文字列リテラルとして機能し, 特定のオブジェクトを作ります.
# 例えば `@r_str` は正規表現のオブジェクトを作り,
# `@big_str` は `BigInt` 型や `BigFLoat` などの可変長サイズの整数や浮動小数点数を作るのに使われます..
# この特殊なリテラルはコンパイル時に文字列を解釈して特殊なオブジェクトが作れる利点があります.

# %%
println(typeof(42))      # Int64
println(typeof(big"42")) # BigInt

println(typeof(3.14))      # Float64
println(typeof(big"3.14")) # BigFloat
# %%

# #### P.91 2.8.7 マクロの定義
# ユーザー自身でマクロを定義できます.
# 定義には `macro` キーワードを使います.
# マクロは展開時に名前を置き換えます.
# 簡単なマクロで確認してみましょう.

# %%
macro plus1(ex)
      :($(ex) + 1)
end
@macroexpand @plus1 2x
# %%

# 特に REPL で実行するとわかるように `x` が `Main` モジュールのグローバル変数 `x` を参照しています.
# これは `test(x) = @plus1 2x` と書いたとき,
# 引数の `x` とマクロの `2x` の `x` が別の変数だという意味です.

# Julia はマクロの展開時にデフォルトで式おん中の識別子をマクロが定義されたモジュールのグローバル変数に置き換えます.
# これを防ぐには `esc` 関数で識別がグローバル変数に変換されないようにします.
# 次のコードで動作の違いを確認してください.

# %%
macro plus1(ex)
    :($(esc(ex)) + 1)
end
@macroexpand @plus1 2x
# %%

# #### P.93 2.8.8 識別子の変換規則
# マクロ展開で識別子はグローバル変数に変換されるのがデフォルトです.
# しかしローカル変数に変換される例外もあります.
# これは次の場合です.
#
# - `global` 宣言なしで代入されたとき.
# - `local` 宣言があるとき.
# - 関数定義の引数であるとき.

# 1 つ目の規則が実際のコードでは 1 番よくあります.
# ローカル変数と解釈された識別子はマクロ展開時に新しい変数に置き換えられる.
# これはマクロ呼び出し側にある別の識別子との衝突を避けるためです.
# このようなマクロ展開の仕方は一般に衛生的 (hygienic) マクロと呼ばれます.
# これは実戦的な機能です.
# 特にコードの実行時間をナノ秒単位で計測する `@time_ns` マクロを定義して確認してみましょう.

# %%
macro time_ns(ex)
  quote
    t1 = time_ns()
    val = $(esc(ex))
    t2 = time_ns()
    val, Int(t2 - t1)
  end
end
@time_ns sum(randn(1000)) # 計算結果と実行時間のタプルを返す
@time_ns sum(randn(1000)) # 2 回目以降は計算が高速になる
# %%

# 時間を計測したいコード `ex` に `t1 = ...` のような代入があり,
# 衛生的マクロではなくローカル変数をそのまま維持するマクロだとすると,
# マクロ定義内のローカル変数 `t1 = time_ns()` の結果を上書きしてしまい,
# 目的の時間計測ができなくなります.

# マクロ定義中の `t1, t2, val` は `global` 宣言なしで代入されているので,
# 前述の規則からローカル変数と解釈されます.
# これがどういう置き換えられるか確認しましょう.

# %%
@macroexpand @time_ns sum(randn(1000))
# %%

# ついでに `time_ns` や `Int` は `Main` モジュールの `Main.time_ns` や
# `Main.Int` にそれぞれ変換されています.

# ### P.95 2.9 C 言語の呼び出し

# #### P.95 2.9.1 `ccall` 構文
# C の関数を呼び出すのに使う `ccall` という特別な構文が用意されている.
#
# ```
# ccall(C の関数, 戻り値の型, 引数の型, 引数1, 引数2)
# ```
#
# 例として C の標準ライブラリで定義されている `sin` 関数を呼び出してみましょう.

# %%
ccall(:sin, Cdouble, (Cdouble,), 1.0)
# %%

# 詳しくはドキュメントを見てもらうことにして,
# ここでは簡単な事情だけコメントします.
# `Cdouble` は Julia の `Float64` で,
# C の型名に対応する `C` ではじまる別名が用意されていることがあります.
# 関数はライブラリ名まで渡さないといけない場合もあります.
# データ変換の関数 `Base.cconvert` もあります.

# %%
println(Base.cconvert(Cdouble, 1))
println(Base.cconvert(Cdouble, 1) isa Cdouble)
# %%

# `ccall` 構文はふつうの関数呼び出しと違って特殊な構文です.
# これ自体を引数として別の関数にわたしたり, 値としては使えません.

# #### P.97 2.9.2 ポインタの受け渡し
# C には引数や戻り値としてポインタを使う関数もあります.
# まず文字列で確認してみましょう.

# %%
home = ccall(:getenv, Cstring, (Cstring,), "HOME") # 環境変数 HOME を取得
println(unsafe_string(home))

not_exist = ccall(:getenv, Cstring, (Cstring,), "NOTEXIST") # 環境変数 HOME を取得
# println(unsafe_string(not_exist)) # 存在しない環境変数を得ようと思うとエラー
# %%

# 文字列以外のデータ型でも Julia と C の間で値へのポインタをやり取りできます.

# #### P.100 2.9.3 構造体の受け渡し
# C の `struct` で定義された構造体と Julia の `struct` や
# `mutable struct` で定義された複合型のメモリレイアウトには互換性があります.

# ### P.101 2.10 外部プログラムの呼び出し
# ここではコマンドの作成・実行方法からパイプライン処理と発展的なコマンド作成法を見てみます.

# #### P.101 2.10.1 コマンの作成・実行
# Perl や Ruby と同じくバッククォートで外部プログラムを実行できます.
# Julia ではバッククォートで囲むだけでコマンドは実行されず,
# `run` 関数に渡してはじめて実行されます.
# `run` 関数は実行したコマンドのプロセスを `Process` 型のオブジェクトとして返します.

# %%
println(typeof(`ls`)) # `Cmd` 型のコマンドオブジェクト
println(run(`ls`))
println(typeof(run(`ls`)))
# %%

# コマンドを `run` 関数で実行するとコマンドの標準出力はそのまま Julia の標準出力に出力されます.
# コマンドの標準出力について一番簡単なのは `read` 関数を使う方法でしょう.

# %%
println(read(`ls`, String)) # 標準出力を文字列として取得
# %%

# `eachline` 関数で標準出力を行ごとに処理できます.

# %%
for line in eachline("junomemo.jl")
  @show line
end
# %%

# コマンドのプロセスに対して Julia からデータに渡すこともできます.
# 次のように `open` 関数を使うと書き込めます.

# %%
open(`wc -l`, "w", stdout) do output
  for _ in 1:10
    println(output, "hi!")
  end
end
# %%

# コマンドはデフォルトで同期筆耕されるので,
# 長時間かかるコマンドでも終わるまで待ちます.

# %%
@time run(`sleep 1`)
# %%

# 呼び出したプロセスが正常終了しない場合は例外が送出されます.

# %%
# run(`ls --option-not-exist`)
# %%

# #### P.103 2.10.2 コマンド実行の注意点
# Julia のコマンドは一般的なシェルを経由せずに実行されるので,
# POSIX で定義されている一般的なシェルの機能はバッククォート内に書いても使えない.
# 例を挙げるとパターンマッチに使われるアスタリスクやパイプラインのバーはそのままコマンドの引数として扱われるため,
# `ls *.txt` や `find . -type f | wc -l` などは bash などのシェルとは違う動作をします.

# シェルの機能が必要な場合, 明示的にシェルを実行できます.
# 例えば `-c` オプションがあり, これに実行したいコマンドの文字列を渡すとシェルを経由してコマンド実行できます.

# %%
println(run(`bash -c 'ls *.txt'`))
println(run(`bash -c 'find . -type f | wc -l'`))
# %%

# #### P.104 2.10.3 パイプライン処理
# バッククォート内でシェルのパイプラインは使えません.
# しかし `pipeline` 関数を使うとシェルに頼らなくてもコマンドのパイプラインを構築できます.

# ```
# pipeline(cmd1, cmd2, cmd3) # cmd1 | cmd2 | cmd3
# ```

# %%
println(run(pipeline(`cat file.tmp.txt`, `wc -l`)))
# %%

# #### P.104 2.10.4 より発展的なコマンドの作成方法
# 動的にコマンドを作ることもできます.

# %%
msg = "hello"
cmd = "echo"
println(run(`echo $(msg)`))
println(run(`$(cmd) $(msg)`))
# %%

# 空白を含む文字列に関して注意すべき挙動があります.

# %%
run(`touch $("foo bar.tmp.txt")`)
run(`touch $(["foo.tmp.txt", "bar.tmp.txt"])`)
run(`touch $(("foo.tmp.txt", "bar.tmp.txt"))`)

println(run(`locale`))
println(run(Cmd(`locale`, env = Dict("LANG" => "ja_JP.UTF-8"))))
println(run(`pwd`))
println(run(Cmd(`pwd`, dir = "./")))
# %%

# 実行されるコマンドが存在するかどうかは `Sys.which` 関数を使います.
# コマンド名を渡すとその絶対パスを返します.
# 同名のコマンドが存在しない場合は `nothing` を返します.

# %%
println(Sys.which("ls"))
println(Sys.which("command-not-exists"))
println(Sys.which("command-not-exists") === nothing)
# %%

# ### P.106 2.11 パッケージ
# Julia のパッケージは特定の構造を持つディレクトリで,
# ソースコードやドキュメントのファイルが入っています.
# Julia 本体と一緒に配布されているパッケージと別途インストールするべきパッケージがあります.

# #### P.107 2.11.1 パッケージ管理の基本
# パッケージ管理ツールは標準でインストールされています.
# ふつうは REPL で管理します.
# REPL を起動して `]` と入力するとプロンプトが `julia>` から `(v1.2) pkg>` のように変わります.
# この状態で `help` を実行すると使えるコマンドの一覧が出てきます.
# 元のモードに戻るには Delete または Backspace を押します.
#
# - `status` (`st`): インストールしている外部パッケージの状態を表示する
# - `add`: パッケージを追加する. 例: `add Distributions`
# - `remove` (`rm`): インストールしたパッケージを削除する
# - `update` (`up`): インストール済みパッケージをアップデートする
# - `pin`: インストールしたパッケージのバージョンを固定する
# - `free`: `pin` で固定したバージョンを解放できる
# - `gc`: 不要なファイル・パッケージを自動で削除できる
# - `activate`: プロジェクトごとのパッケージ管理を有効にする

# #### P.109 2.11.2 プロジェクトのパッケージ管理
# `Project.toml` と `Manifest.toml` でパッケージを管理します.
# `Project.toml` はいまのプロジェクトが依存しているパッケージを管理するファイルです.
# `Manifest.toml` は実際に使われるパッケージの正確なバージョンやインストール場所を管理します.
# `Manifest.toml` は `Project.toml` をもとに依存を解決して生成します.

# #### P.111 2.11.3 プロジェクトの有効化
# REPL 以外では `activate` コマンドは使えます.
# `julia` コマンドを実行するとオプションとして
# `--project=@.` を指定するといまのディレクトリにあるプロジェクトを有効化します.

# #### P.112 2.11.4 パッケージの作成
# [Distributions.jl](https://github.com/JuliaStats/Distributions.jl) のパッケージ構成を参考にするといいでしょう.

# ## P.117 3 Julia ライブラリの使い方

# ### P.117 3.1 線型代数
# `Array` 型を使った線型代数の演算を見てみましょう.
# 線型代数の関数は `LinearAlgebra` モジュールに含まれています.

# #### P.117 ベクトルの演算

# %%
using LinearAlgebra
@show dot([1, 2, 3], [4, 5, 6]) # 内積
@show cross([0, 1, 0], [0, 0, 1]) # ベクトル積

v = [-1, 2, 3]
@show norm(v, 1) # L^1 ノルム
@show norm(v, 2) # L^2 ノルム
@show norm(v, Inf) # # L^∞ ノルム
@show normalize(v, 1) # L^1 ノルムで正規化
@show normalize(v, 2) # L^2 ノルムで正規化
# %%

# #### P.118 3.1.2 行列の演算
# 以下で書いた関数以外にも行列式の対数を求める `logdet` や,
# 擬似逆行列を求める `pinv` などがあります.
# 詳しくは公式のドキュメントを見てください.

# %%
using LinearAlgebra
A = [1 2 3; 4 1 6; 7 8 1]
A = [
  1 2 3
  4 1 6
  7 8 1
  ]
@show tr(A) # トレース
@show det(A) # 行列式
@show inv(A) # 逆行列
@show A * inv(A) # 単位行列になってほしい
# %%

# #### P.119 3.1.3 行列の種類
# 対称行列やエルミート行列に対して,
# `Symmetric` や `Hermitian` といった型があります.
# ほか にも UpperTriangular, LowerTriangular,
# Tridiagonal, SymTridiagonal, Bidiagonal, Diagonal などがあります
# 実際に作ってみましょう.

# %%
using LinearAlgebra
A = rand(3, 3)
@show A
@show Symmetric(A)
@show Symmetric(A, :U) # 上三角部分をもとにして対称行列を作る
@show Symmetric(A, :L) # 下三角部分をもとにして対称行列を作る

A = [1 2 3; 4 5 6; 7 8 9]
@show A
@show UpperTriangular(A)
@show LowerTriangular(A)
@show Tridiagonal(A)
@show Diagonal(A)

@show SymTridiagonal(Symmetric(A))

# https://docs.julialang.org/en/v1/stdlib/LinearAlgebra/#LinearAlgebra.Bidiagonal
dv = [1, 2, 3, 4]
ev = [7, 8, 9]
@show Bidiagonal(dv, ev, :U) # ev is on the first superdiagonal
@show Bidiagonal(dv, ev, :L) # ev is on the first subdiagonal
# %%

# #### P.120 3.1.4 行列分解
# ある行列を行列の積に分解することを指します.
# Julia でもいろいろな分解がサポートされています.
# 詳しくはドキュメントを参照してください.
#
# - `cholesky`: コレスキー分解
# - `lu`: LU 分解
# - `qr`: QR 分解
# - `hessenberg`: ヘッセンベルク分解
# - `eigen`: スペクトル分解
# - `svd`: 特異値分解
#
# それぞれの関数は `cholesky`に対する `cholesky!` のように,
# 入力を書き換える関数も定義されています.

# %%
using LinearAlgebra
A = rand(Float32, 4, 3)
@show A
F = svd(A)
@show typeof(F)
@show F
@show F.S # 特異値
@show F.U
@show F.V
@show F.Vt
# %%

# #### P.121 3.1.5 BLAS
# BLAS (basic linear algebra subprograms) は線型代数演算に関する標準的な
# API 規格です.
# Julia では `LinearAlgebra.BLAS` で BLAS のラッパーを提供しています.

# `gemv` は行列とベクトルの積を計算します.
# 行列 `A`, 2 つのベクトル `x`, `y` について,
# `gemv` は `α * A * x` を計算し,
# `gemv!` は `y` を `α * A * x + β * y` に更新します.
# ここで `α` と `β` は `A` と `y` に対する重み係数です.

# %%
using LinearAlgebra.BLAS
A = [
  1.0 4.0
  2.0 5.0
  3.0 6.0
  ]
@show A
x1 = [1.0, 2.0]
x2 = [1.0, 2.0, 3.0]
y1 = [1.0, 1.0, 1.0]
y2 = [1.0, 1.0]
@show gemv('N', 0.0, A, x1)
@show gemv('N', 1.0, A, x1)
@show gemv('T', 1.0, A, x2)
@show gemv!('N', 0.0, A, x1, 1.0, y1) # y1 を更新する
@show gemv!('N', 1.0, A, x1, 0.0, y1) # y1 を更新する
@show gemv!('T', 0.0, A, x2, 0.0, y2) # y2 を更新する
# %%

# ### P.123 3.2 ファイル入出力
# ファイルの入出力, シリアライズ・デシリアライズ,
# XML・JSON ファイルの扱いを見てみましょう.

# #### P.123 ファイルとストリーム
# ファイル処理には `open` 関数を使います.
# これは次の構文で使います.
#
# ```
# open(filename::String, [mode::String]) -> IOStream
# ```
#
# `mode` とその対応は次のようになっています.
#
# - `r`: read
# - `w`: write
# - `a`: append
# - `r+`: read+write, 指定されたファイルがないとエラー
# - `w+`: read+write, 指定されたファイルがないと新規作成
# - `a+`: read+append

# %%
# ファイルを作っておく
fname = "p124.tmp.txt"
# 読み込みその 1
open(fname, "w") do f println(f, "Hello Julia!") end
# 読み込みその 2
open(fname) do f
  @show readlines(f)
end
# 読み込みその 3
open(fname) do f
  for line in eachline(f)
    @show line
  end
end
# close なしで読み込む
lines = open(readlines, fname)
@show lines
# %%

# 改めてファイルに書き込む方法を見てみましょう.

# %%
fname = "p125.tmp.txt"
open(fname, "w") do f
  println(f, "Line 1")
  println(f, "Line 2")
end
lines = open(readlines, fname)
@show lines
# %%

# `readline(stdin)` と書くと標準入力から 1 行読み込めます.

# #### P.125 3.2.2 シリアライズとデシリアライズ
# これでわかるかはともかく一言でいうと,
# シリアライズはオブジェクトをバイトストリームまたは他のフォーマットに変換する処理のことです.
# バイトストリームをオブジェクトに復元する処理をデシリアライズと呼びます.
# これを使うと Julia のオブジェクトをファイルとして保存したり,
# ファイルからオブジェクトが復元できます.
# Python では pickle が有名です.
# Julia では標準で `Serialization` モジュールがあり,
# `using Serialization` でモジュールを読み込みます.

# %%
using Serialization
fname = "dict.tmp.dat"
dict = Dict("a" => 1, "b" => 2)
serialize(fname, dict) # 必要ならファイルを開いてみてみよう (バイナリなのでふつう読めない)
@show deserialize(fname)
# %%

# 関数の中身や型定義は保存されないので,
# デシリアライズするときそれらの関数や型が読み込まれた状態でなければいけません.
# バージョンをまたぐほどの長期間データを保存するときは,
# 次の JLD2.jl を使う方がいいでしょう.

# #### P.127 3.2.3 JLD2
# JLD2.jl は Julia のオブジェクトを保存するためのパッケージで,
# JLD.jl の改善版です.
# 特に HDF5 というフォーマットのサブセットで,
# これは科学技術分野でよく使われています.
# 詳しくはパッケージの GitHub ページを見てください.

# まずインストールが必要です.
# REPLから `add JLD2 FileIO` でインストールしておいてください.

# %%
using JLD2, FileIO
data = rand(3, 2)
@show data
fname = "out.tmp.jld2"
save(fname, "data", data)
load_data = load(fname)
@show load_data
# %%
