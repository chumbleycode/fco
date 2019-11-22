# A simple example analysis using Bayesian parameter ranking

* The docker image is hosted [here](https://hub.docker.com/repository/docker/chumbleycode/fco).
* The submitted manuscript is [here](https://chumbleycode.github.io/docs/papers_reports/fcr_apa.pdf). 
* If you publish with this method, please reference the paper as: Chumbley JR (2019) A Bayesian credible set for ranking parameters by relative magnitude. Manuscript submitted for publication. https://chumbleycode.github.io/docs/papers_reports/fcr_apa.pdf
* The simple example, in "/home/rstudio/fco/R" does linear regression then asks for the most detailed but plausible ordering of parameters (by magnitude).
* For related work, see [my website](https://chumbleycode.github.io).

# 1. Pull docker image 

Run the following from the shell:

```
docker pull chumbleycode/fco:latest
```

# 2a. To open container with rstudio in your browser

```
docker run --rm -p 2222:8787 -e USER=guest -e PASSWORD=secret chumbleycode/fco:latest
```

Then go to http://localhost:2222 in your favorite browser.

You can change USER and PASSWORD, in the above. 
USER=guest
PASSWORD=secret

# 2b. Alternatively, to use R in the terminal

```
docker run --rm -it chumbleycode/fco:latest R
````

# 3. Play with the example analysis script

A simple example script is in fco/R/example_analysis.R


# Alternatives to 2a/2b: Running container with local volume and root access

If you want to use the functionality on own data, you must additionally bind mount your local folder. First use the shell to navigate to your data folder then - for shell or browser access - run either 

```
docker run --rm -p 2222:8787 -e PASSWORD=secret -v $(pwd):/home/rstudio/fco/host -e ROOT=TRUE chumbleycode/fco:latest
```

or

```
docker run -it -v $(pwd):/home/rstudio/fco/host chumbleycode/fco:latest R
````
