I collected my scientific codes, especially math and physics.

# python

Jupyter がすごいのでそれを使ったコードも書いてみたい.
インストールについての注意を書いておく.
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
