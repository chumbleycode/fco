library(tidyverse)
library(lmhyp)
library(ggformula)
library(stringi)
source("R/pkg.R")
set.seed(1000)

if(from_disk <- TRUE){
  
  out_mcmc <- readRDS("data/out_mcmc_obeseColorectal_ud.rds")

} else {
  
  library(rstan)
  source("R/range_util_server.R")
  
  # IN THIS SIMULATED DATASET THE TRUE UNDERLYING ORDER IS "1|2|3|4|5"
  # CHANGE ME (ALONE?)
  .x = readRDS("R/range_real_data/toy_example.rds")
  
  # ORGANIZE INPUTS/DATA
  X = select(.x, matches("w"))
  C = select(.x, matches("c"))
  y = .x$y
  
  # SAMPLE POSTERIOR 
  ones = rep(1, dim(X)[2])
  out_mcmc = infer(X = X, C = C, y = y, Dalp  = 1.0 * ones, n_iter = 20000, n_chain = 5) 
  
} 

# EXTRACT VARIABLES
n_samp     = dim(out_mcmc)[1]
post_delta = select(out_mcmc, matches("delta"))
post_w     = select(out_mcmc, matches("w"))

# TIDY OBJECT
aug  =
  bind_cols(post_delta, post_w) %>% 
  mutate(post_range = apply(post_w, 1, max) - apply(post_w, 1, min))

# MODEL COMPARISON THRESHOLD
u   = 0.85
l   = 1-u
aug = mutate(aug, class = cut(post_range, c(0, l, u, 1), labels = c("a", "s", "c")))

# "MODEL PROBABILITY": CREDIBLE IF EXCEEDS SAY 0.9
(model_probability = summary(aug$class)/dim(aug)[1])

# SPECIFY NAMES OF VARIABLES
names(post_w)
vars = quos(c(`W[1]`, `W[2]`, `W[3]`, `W[4]`)) # only 4 waves for crp
vars = quos(c(`W[1]`, `W[2]`, `W[3]`, `W[4]`, `W[5]`)) # else 5 waves

# SENSITIVE MODELS
post = 
  post_w %>%
  rowwise() %>% 
  transmute(H = str_c(order(!!! vars), collapse = "<")) %>% 
  ungroup %>% group_by(H) %>% count %>% mutate(Hp = n/n_samp, M = "M0") %>% select(-n, Hp, H, M) %>% 
  ungroup %>% 
  list

# ALGORITHM 1
max_ranking = get_fco_det(post) # initialize at max probability full ranking
fco_recurse = find_local_fco(partial_rank = max_ranking$H,
                             dat          = post[[1]])
(CDF  = tidy_output(max_ranking, fco_recurse))

# ALGORITHM 2 (more complex: to be depricated?)
source("R/rank_util.R")
recursion = map(list(post_w), get_discrete_posterior, type = "greedy") 
(CDF = 
    recursion %>% 
    map(pluck, "out_wux") %>% # over all params
    map(filter, what == "max"))

