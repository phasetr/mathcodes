
===============================================
付属データをお使いになる前にお読みください
===============================================

●付属データについて
付属データには『Pythonで動かして学ぶ！あたらしい数学の教科書』（ISBN978-4-7981-6117-4）の本文で解説したコードと演習問題および解答例のサンプルファイル（第2章から第7章）を用意しています。

□サンプルファイルの形式
サンプルファイルはJupyter Notebook形式です。
AnacondaのインストールやJupyter Notebookの起動方法などは本書の第1章をご覧ください。

□Jupyter Notebookへのサンプルファイルの追加方法
Jupyter Notebookを起動すると、［Files］タブの画面（Windows のエクスプローラーが起動）が開きます。
［Files］タブの画面にサンプルファイルをドラッグ＆ドロップして追加できます。
追加したら［Upload］ボタンをクリックします。
アップロードしたファイルをクリックすると編集画面が開きます。
［In］のボックスからコードをコピーできます。またファイル上で、実行結果も確認できます。

●本書のサンプルの動作環境
本書のサンプルは以下の環境で、問題なく動作することを確認しています。

・OS
Windows 10/macOS Mojave 10.14.5

・Anaconda3
Anaconda3-2019.03

・Python
Python 3.7

・ライブラリとバージョン
NumPy 1.15.4
matplotlib 3.0.2

□Anacondaの最新版
Anacondaの最新版は、AnacondaのWebサイトからダウンロードできます。

・AnacondaのWebサイト
https://www.anaconda.com/


□Anaconda3-2019.03
本書執筆時点で利用した「Anaconda3-2019.03」は、Anaconda installer archiveよりダウンロードできます。

・Anaconda installer archive
https://repo.anaconda.com/archive/

ご利用のOS環境に合わせてダウンロードしてください。

・Windows
Anaconda3-2019.03-Windows-x86.exe
Anaconda3-2019.03-Windows-x86_64.exe

・macOS
Anaconda3-2019.03-MacOSX-x86_64.pkg
Anaconda3-2019.03-MacOSX-x86_64.sh

・Linux
Anaconda3-2019.03-Linux-ppc64le.sh
Anaconda3-2019.03-Linux-x86_64.sh

□ライブラリのインストール方法
　本書では、Anaconda Navigator上からNumPyやmatplotlibのライブラリをインストールしています（第1章を参照）。

□ライブラリのバージョンを指定したインストール方法
　ライブラリのバージョンを指定してインストールする際は、Anaconda Navigatorの左カラムから「Environments」を選択します。次に右のカラムで「base（root)」の右向き三角形をクリックして、「Open Terminal」を選択します。コマンドプロンプト（macOSではターミナル、Linuxでは端末）が起動しますので、以下のようにpipコマンドで、バージョンを指定し、インストールしてください。

------------------------
pip install numpy==1.15.4
pip install matplotlib==3.0.2
------------------------

●サンプルデータの一覧
　サンプルデータのフォルダとファイル構成は次の通りです。zipファイルを解凍して利用してください。サンプルコードがない節は、収録していません。

python_atarasi_sugaku_sample.zip

    +-- Chapter2_sample【第2章のサンプルフォルダ】
        +-- 2_01_python_basic-sample.ipynb
        +-- 2_02_numpy_basic-sample.ipynb
        +-- 2_03_matplotlib_basic-sanple.ipynb

    +-- Chapter3_sample【第3章のサンプルフォルダ】
        +-- 3_01_variable_constant-sample.ipynb
        +-- 3_02_function-sample.ipynb
        +-- 3_03_power-sample.ipynb
        +-- 3_04_poly-nomial-sample.ipynb
        +-- 3_05_trigonometric-sample.ipynb
        +-- 3_06_sum-sample.ipynb
        +-- 3_07_random-sample.ipynb
        +-- 3_08_absolute_value-sample.ipynb

    +-- Chapter4_sample【第4章のサンプルフォルダ】
        +-- 4_01_tensor-sample.ipynb
        +-- 4_02_inner_product_sample.ipynb
        +-- 4_03_matrix_product-sample.ipynb
        +-- 4_04_transpose-sample.ipynb
        +-- 4_05_determinant-sample.ipynb
        +-- 4_06_linear_transformation.ipynb
        +-- 4_07_eigenvalue-sample.ipynb
        +-- 4_08_cos_similarity-sample.ipynb

    +-- Chapter5_sample【第5章のサンプルフォルダ】
        +-- 5_01_limit-sample.ipynb
        +-- 5_06_napier-sample.ipynb
        +-- 5_07_gradient_decent-sample.ipynb

    +-- Chapter6_sample【第6章のサンプルフォルダ】
        +-- 6_01_probability-sample.ipynb
        +-- 6_02_average-sample.ipynb
        +-- 6_03_variance-sample.ipynb
        +-- 6_04_normal-sample.ipynb
        +-- 6_05_covariance-sample.ipynb
        +-- 6_06_correlation-sample.ipynb
        +-- 6_08_likelihood-sample.ipynb
        +-- 6_09_entropy-sample.ipynb

    +-- Chapter7_sample【第7章のサンプルフォルダ】
        +-- 7_01_regression-sample.ipynb
        +-- 7_02_classification-sample.ipynb
        +-- 7_04_learn_mechanism-sample.ipynb
        +-- 7_05_learn-sample.ipynb

    +-- README.txt【本ファイル】

●免責事項について
・本書に記載されたURLなどは予告なく変更される場合があります。
・本書の出版にあたっては正確な記述につとめましたが、著者や出版社などのいずれも、本書の内容に対して何らかの保証をするものではなく、内容やサンプルにもとづくいかなる運用結果に関してもいっさいの責任を負いません。
・本書に記載されている会社名、製品名はそれぞれ各社の商標および登録商標です。
・本書の内容は、2019年8月執筆時点のものです。。

2019年8月　株式会社翔泳社 編集部