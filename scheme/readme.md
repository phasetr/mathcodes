# Scheme

## MIT-SchemeをJupyterで
```sh
docker run -it --rm -p 8888:8888 kkwok/jupyter-mit-scheme
docker run -it --rm --name ms -p 10000:8888 --mount type=bind,source=$(pwd),destination=/work kkwok/jupyter-mit-scheme jupyter notebook --ip=0.0.0.0 --allow-root --NotebookApp.token=''
```

## Dockerメモ
`kkwok/jupyter-mit-scheme`は古いので作り直したい.

```sh
docker run -it -d --rm -p 8888:8888 jupyter/datascience-notebook
docker stop hoge
```
