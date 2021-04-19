# Scheme

## MIT-SchemeをJupyterで
```sh
docker run -it --rm --name ms -p 10000:8888 --mount type=bind,source=$(pwd),destination=/work kkwok/jupyter-mit-scheme jupyter notebook --ip=0.0.0.0 --allow-root --NotebookApp.token=''
```
