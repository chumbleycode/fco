setwd("/home/rstudio/fco")
library(tidyverse)
library(lmhyp)
library(ggformula)
library(stringi)
library(combinat)
source("R/pkg.R") # a minimal "package"

# data simulated to have rank: w1|w4|w3|w5|w2
dat    = readRDS("data/example_dataset.rds")

# estimate linear model
object = list(model = lm(y~ . - 1, data = select(dat, matches("y|w"))))

# preparation
nme  = map(object, coef) %>% map(names)
prep = get_hyp(nme, only_order_hyps = T)

# estimate posterior for each full rank
post  = pmap(list(object, prep$hyp, prep$prior), test_hyp)
post  = tidy_det(prior = prep, post = post) # TIDY output

# get max posterior full rank and (generally partial) FCO
max_rnk = get_fco_det(post)$H # extract max full rank
fco     = find_local_fco(partial_rank = max_rnk,
                         dat          = post[[1]])
fco

# Example using MCMC
ok for the last time i am inside
