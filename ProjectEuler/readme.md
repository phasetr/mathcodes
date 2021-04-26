# Project Euler

## サイト
- [Project Euler](https://projecteuler.net/)

## Docker
### メモ
#### コンテナのシェルに入る
```sh
docker-compose up -d
docker exec -it jmc bash
docker stop jmc
```

### MIT-Scheme
- <https://github.com/joeltg/mit-scheme-kernel>
- 自力でDockerインストールしたいができていない
- とりあえず次のコマンドでMIT-SchemeだけのJupyterが起動する

```sh
docker run -it --rm --name ms -p 10000:8888 --mount type=bind,source=$(pwd),destination=/work kkwok/jupyter-mit-scheme jupyter notebook --ip=0.0.0.0 --allow-root --NotebookApp.token=''
```

### TODO Common LISP
- <https://github.com/yitzchak/common-lisp-jupyter>

## TODO
- Python, Julia, F#のJupyter導入

## 参考
- [17言語をぶち込んだJupyter LabのDockerイメージを作ってみた](https://qiita.com/HeRo/items/61e7f45a5dbb5fd0e4a7)
- [Jupyter kernels](http://pythonic.zoomquiet.top/data/20170425211145/index.html)
