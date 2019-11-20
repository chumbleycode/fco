# An example analysis using Bayesian parameter ranking

* For the working paper see [here](https://chumbleycode.github.io/docs/papers_reports/fcr_apa.pdf).
* The simple example, does linear regression then asks for the most detailed but plausible ordering of parameters (by magnitude).
* For related work, see [my website](https://chumbleycode.github.io).
* If you are new to docker see [here](https://chumbleycode.github.io/fco_docker.html).

# 0. Install docker (if you don't yet have it)

Open an account at [docker hub](https://hub.docker.com/). Then follow installation instructions.

# 1. Get docker image 

```
docker pull chumbleycode/fco:latest
```

For more information see: https://hub.docker.com/repository/docker/chumbleycode/fco

# 2. Open container 


You can change USER and PASSWORD.

```
docker run --rm -v $(pwd):/home/rstudio/fco/ -p 2222:8787 -e USER=guest -e PASSWORD=secret chumbleycode/fco:latest
```

# 3a. To use rstudio in your browser

Go to http://localhost:2222.

USER=guest
PASSWORD=secret

# 3b. Alternatively, to use R in the terminal

```
docker run --rm -it -v $(pwd):/home/rstudio/fco/ chumbleycode/fco:latest R
````

# 4. Play with the example analysis script

A simple example script is in fco/R/example_analysis.R


