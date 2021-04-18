FROM jupyter/datascience-notebook
USER root

RUN apt-get update -y && \
    apt-get -y install git libzmq3-dev jupyter-notebook mit-scheme sbcl && \
    pip install jupytext && \
    apt-get -y remove git

#RUN git clone https://github.com/joeltg/mit-scheme-kernel && \
#    cd mit-scheme-kernel && \
#    make && \
#    sudo make install && \
#    cd && \
#    rm -rf mit-scheme-kernel && \

# RUN julia -e 'using Pkg; Pkg.add("Plots"); using Plots' && \
#     julia -e 'using Pkg; Pkg.add("DifferentialEquations"); using DifferentialEquations'

RUN mkdir -p /home/jovyan/.jupyter/lab/user-settings/@jupyterlab/apputils-extension && \
    jupconfig="/home/jovyan/.jupyter/jupyter_notebook_config.py" && \
    echo '{"theme":"JupyterLab Dark"}' > \
      /home/jovyan/.jupyter/lab/user-settings/@jupyterlab/apputils-extension/themes.jupyterlab-settings && \
    echo 'c.NotebookApp.contents_manager_class = "jupytext.TextFileContentsManager"' >> ${jupconfig} && \
    echo 'c.ContentsManager.default_jupytext_formats = "ipynb,md"' >> ${jupconfig}

ENV LANG ja_JP.UTF-8
ENV LANGUAGE ja_JP:ja
ENV LC_ALL ja_JP.UTF-8
ENV TZ JST-9

#CMD ["jupyter-notebook", "--port=8000", "--ip=0.0.0.0", "--allow-root", "--NotebookApp.token=''"]
