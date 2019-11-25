FROM rocker/tidyverse:3.6.1

COPY . /home/rstudio/fco

RUN Rscript /home/rstudio/fco/R/required_packages.R

EXPOSE 8787
