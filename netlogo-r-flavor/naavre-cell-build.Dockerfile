FROM mambaorg/micromamba:1.5.6
RUN micromamba install -y -n base -c conda-forge conda-pack
ARG CONDA_ENV_FILE
COPY ${CONDA_ENV_FILE} environment.yaml
RUN micromamba create -y -n venv -f environment.yaml && \
    micromamba clean --all --yes

RUN micromamba run -n venv Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org/'); remotes::install_version('rdwd', version = '1.8.29', repos = 'https://cloud.r-project.org/')"
RUN micromamba run -n venv Rscript -e "install.packages('stats', repos='https://cloud.r-project.org/')"
