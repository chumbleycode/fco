FROM rocker/tidyverse:3.6.1

## Copy requirements.R to container directory /tmp
COPY . /home/rstudio/fco

## install required libs on container
RUN Rscript /home/rstudio/fco/R/required_packages.R

EXPOSE 8787
