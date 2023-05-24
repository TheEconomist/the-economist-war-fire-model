FROM condaforge/mambaforge

RUN apt-get update
RUN apt-get install -y git git-lfs libcurl4-openssl-dev 

COPY requirements.txt .

RUN mamba install -y --file requirements.txt