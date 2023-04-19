FROM condaforge/mambaforge

RUN apt-get update
RUN apt-get install -y git git-lfs libcurl4-openssl-dev 

RUN mamba install -y awscli gh r-base r-tidyverse r-devtools r-anytime r-rgdal r-countrycode r-data.table r-readxl r-googlesheets4 r-lubridate r-maps r-progress r-WDI r-zoo r-Rcpp r-geosphere r-gganimate r-gifski r-agtboost r-sf
#RUN Rscript -e 'devtools::install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")'