3章 最も単純なコンピュータ
=================================

oO(正規表現みたいな話かな？)

3.1 決定性有限オートマトン
---------------------------------

非常に単純なコンピュータとして、有限状態機械(finite state machine)もしくは有限オートマトン(finite automaton)と呼ばれるものを考える。

1. FSMには入力として文字のストリームが渡される。
2. FSMは入力を受けて状態を遷移させてゆく。(状態遷移図っぽい設計図で書ける)
3. 入力ストリームが終わったとき行き着いた状態が「受理」だったらtrue、「拒否」だったらfalseを返却する。

文字のマッチングとかに使うイメージだ。

まずは、入力ストリームに対して決定的である機械を考える。これを決定性有限オートマトンと呼ぶ。DFA: Deterministic Finite Automaton

常に次に進むべき情報は一つに定まるようになっているのが特徴。(どっちの状態にも遷移できるから迷う、みたいなことが起きえないように作ってある）

DFAをrubyで再現するために、いくつかのクラスを定義する。

- FARule ...一つ一つの規則を表す。
  - applies_to? 規則を適用できるか返す
  - follow 規則を適用するときに機械をどのように変更するか返す
- DFARulebook ...FARuleを束ねた規則集。
  - next_state ...現在の状態を引数に取って次の状態を返す。
  - rule_for ...次の文字から、適用できる規則を探して返す。(内部用)
- DFA ...DFARulebookから受理状態かどうかを返す。受理状態はコンストラクタで複数渡すことができる。
  - accepting? ...受理状態かどうか。
  - read_character ...入力から一文字読んで、規則集を調べて、現在の状態を変更する。
  - read_string ...文字列を一気に読み込んで、状態を変更する便利メソッド


実行途中の様子は[run.rb](run.rb)にまとめた。(長いので。。)


DFAはそれ自体が状態を持っているので、一度処理させると初期状態から離れてしまう。
いつでも初期状態のDFAが得られるように、DFADesignという設計書のようなクラスを用意し、.to_dfaでDFAを作れるようにする。(ファクトリ的なもの)

最終的にこんな使い勝手に。

```ruby
dfa_design = DFADesign.new(1, [3], rulebook)
puts dfa_design.accepts?('a')
puts dfa_design.accepts?('baa')
puts dfa_design.accepts?('baba')
```

正規表現っぽくなってきた！

3.2 非決定性有限オートマトン
---------------------------

DFAは決定的である。ある状態から次の状態に移る方法は一つに限られていて、とても単純だ。つまり「次に来る文字はa, b, cどれでもOK」みたいな文字列マッチは表現できない。

次の状態はa,b,cいずれでもよい、そういう可能性を許容するような規則が作れると便利だ。
これを非決定性有限オートマトン(NFA: Nondeterministic Finite Automaton)と呼ぶ。

DFAは機械を稼働させて、最終的に受理状態に行き着けばOKとみなすが、NFAは受理状態に行き着く可能性が一つでもあれば、その文字列は受理される、と考える。

物理的な動きが想像できないので、DFAよりわかりにくい。バーチャルっぽくなった。

RubyでNFAをシミュレーションするには、ひたすら可能性のあるパスをしらみつぶしに試し、受理状態を探すような動きをさせればよい。

DFAクラスは現在の状態current_stateを持っていたが、NFAクラスは取りうる状態の集合体current_states(複数系)を持つようになる。

詳細は[NFA.rb](NFA.rb)と[NFA_run.rb](NFA_run.rb)にまとめた。。(これも長い)


なお、特定の機械が受理する文字列の集合を言語(language)と呼ぶ。有限オートマトンが認識できる言語は正規言語(regular language)と呼ばれる。


### 3.2.2 自由移動

文字を読まなくても勝手に移動できる次の状態「自由移動」(free move)を取り入れると、更に表現できるもの(言語)が増える。

文字のところをnilしたFARuleは自由移動とみなすようにして、機能を拡張してみる。

NFAのcurrent_statesメソッドを上書きし、free_moveを辿る処理を挟むことで実現する。

なお、自由移動のあるNFAはNFA-εと呼ばれる。


3.3 正規表現
---------------------

NFAを使うと、正規表現機能を実装することができる。

単純な正規表現のルール：

- 空
- 一つのリテラル文字
  - 'a' ... 文字列aにマッチ
  - 'b' ... 文字列bにマッチ
- 2つのパターンの連結。aとbを連結して正規表現abを作る
- 2つのパターンの選択。a|bはaまたはbにマッチする。
- パターンの繰り返し。`a*`は'', 'a', 'aaa'などにマッチする。

これ以外の構文は以上の構文で再現できるので、これだけを実現しよう。
まずは構文要素を作る。

これも詳細は[REGEXP.rb](REGEXP.rb)と[REGEXP_run.rb](REGEXP_run.rb)に書いた。

```ruby
pattern = Repeat.new(
  Choose.new(
    Concatenate.new(Literal.new('a'), Literal.new('b')),
    Literal.new('a')
  )
)
```

構文要素は定義できた。次は意味論になる。
正規表現をNFA-εへ変換(コンパイル)することができれば、実装できたとみなせるはずだ。

これもREGEXP.rbに書いたので詳細はそちらへ。


3.4 等価性
--------------------

DFAからNFA、自由移動を加えてついには正規表現が実装できるまでになった。
現実的に役立つものができたが、実は今まで実装していた全ては、DFAだけでも実装できることが知られている。

？！

- DFAはある状態から別の状態へ遷移してゆく。
- NFAはある状態の集合から、別の状態の集合へ遷移してゆく。

こう考えると、両者が似たようなことをしていることがわかる。つまり、NFAはDFAを使ってシミュレーションすることができるので、DFAだけでNFAと同等のことが実現できてしまう、ということになる。

実際にシミュレーションを実装してみる。

詳しくは[Simulation.rb](Simulation.rb)と[Simulation_run.rb](Simulation_run.rb)に書いた。


### コラム DFAの最小化

ある文字列を受理するのに一番少ない状態数で構成されたDFAを最小であると言う。NFAからDFAを作ると、冗長な状態を含んだ最小でないDFAが生成されることがあるが、最小化するためのアルゴリズムがある。

Brzozowskiのアルゴリズムと言う。

DFAを逆にする。全ての`FARule.new(state, character, next_state)`を`FARule.new(next_state, character, state)`に修正する。こうすると決定性が壊れるのでDFAはNFAになる。
開始状態と受理状態を入れ替える。
反転させたNFAをDFAに変換する。

すると、マッチする文字列も逆転するが、最小のDFAになるらしい。元の文字列を作りたいならもう一度同じことを繰り返すと、DFAを最小化することができる。

正規表現を最小のDFAに変換すると、それらが同じことをしているか判定することができる。