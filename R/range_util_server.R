
# GENERATE DATA AFTER MADATHIL PAPER  
generate = function(w, n, delta = 2) {
  
  if(length(w) ==3 ) {
    # For agreement with Madathil 3d simulations
    (Sigma = matrix(c(1,.79, .49, .79,1,.79, .49, .79, 1), nrow = 3))
  } else  { 
    (Sigma = diag(length(w)))
  }
  
  exposure = 
    (MASS::mvrnorm(n = n, 
                   mu = delta * w, 
                   Sigma = Sigma))
  colnames(exposure) = str_c("w", 1:length(w))
  
  confounds = 
    (MASS::mvrnorm(n = n, 
                   mu = delta * w, 
                   Sigma = Sigma))
  colnames(confounds) = str_c("c", 1:length(w))
  
  # add a random error col
  as_tibble(cbind(exposure, confounds)) %>% 
    mutate(err_term = rnorm(n), 
           y        = as.vector(delta * (exposure %*% w)  + err_term))
}

# CALL STAN CODE FROM MADATHIL PAPER
infer = function(X, C, y, n_iter = 20000, n_chain = 5, Dalp){
  # infer = function(X, C, y, n_iter = 20000, n_chain = 5, Dalp){
  
  dat <- list(N       = length(y), 
              K       = dim(X)[2], 
              ncov    = dim(C)[2],
              exp_Mat = X,
              cov_Mat = C,
              Y       = y,
              Dalp    = Dalp) 
  
  rstan_options(auto_write = TRUE) 
  fit <-stan('R/simplesimul.stan', 
             data    = dat, 
             chains  = n_chain, 
             iter    = n_iter,
             control = list(stepsize = 0.66, adapt_delta = 0.99, max_treedepth = 20)
  )
  
  fit = as.data.frame(fit) %>% select(matches("w|delta")) # much smaller
}

########################################################
# FUNCTIONS ON SIMPLEX: ALTERNATIVES TO MODEL SELECTION (ACCUMULATION, CRITICAL, SENSITIVE)
########################################################

get_range = 
  function(V, perty = T){ 
    if(perty) par(mfrow = c(1,1))
    y = apply(V,1,max) - apply(V,1,min)
    # posterior range max-min
    hist(y, breaks = 50, main = "", xlim = c(0,1))
    y
  }

get_min_comp = 
  function(V, perty = T){ 
    if(perty) par(mfrow = c(1,1))
    y = apply(V,1,min)
    # posterior y
    hist(y, breaks = 50, main = "", xlim = c(0,1))
    y
  }

get_max_comp = 
  function(V, perty = T){ 
    if(perty) par(mfrow = c(1,1))
    y = apply(V,1,max)  
    # posterior y
    hist(y, breaks = 50, main = "", xlim = c(0,1))
    y
  }

########################################################
# EQUALITY HYPOTHESES
########################################################

critic = 
  function(V, perty = T){
    if(perty) par(mfrow = c(1,dim(V)[2]))
    II = 1:dim(V)[2]
    for(ii in II){
      z = V[, ii]
      y = V[, setdiff(II, ii)]
      x = apply(abs(y - z), 1, min) # min dist between focal wave ii and others
      hcum <- h <- hist(x, plot=FALSE)
      hcum$counts <- cumsum(hcum$counts)
      # plot(hcum, main=as.character(ii), xlim = c(0,1))
      # plot(h, add=T, col="grey")
      d <- density(x)
      if(ii == 1) {
        plot(x = d$x, y = d$y * length(x) * diff(h$breaks)[1], xlim = c(0,1))
      } else {
        lines(x = d$x, y = d$y * length(x) * diff(h$breaks)[1], lwd = 2)
      }
    }
  }


########################################################
# GIVEN A CHARACTER VECTOR OF VARNAMES, RETURN COMPONENTS OF CRITICAL, ACCUMULATION, SENSITIVE
########################################################

get_hyp = 
  function(my_var_names, only_order_hyps = F){
    
    # my_var_names = a list of character vectors for params of interest
    # only_order_hyps = shall I return only order (sensitive, rank) hypotheses, or also accumulation/critical
    
    ########################################################
    # utils
    ########################################################
    shifter <- function(x, n = 1) {
      if (n == 0) x else c(tail(x, -n), head(x, n))
    }
    get_all_perms = function(var_names){
      combinat::permn(var_names) %>% map(str_c, collapse = "<")  
    }
    
    zz = NULL
    for(ii in 1:length(my_var_names)){
      x = my_var_names[[ii]]
      zz[[ii]] = map(0:(length(x)-1), shifter, x = x ) %>% map(str_c, collapse = "=") 
    }
    
    ########################################################
    # character vector of hypotheses
    ########################################################
    
    # accumulation
    accum    = map(zz, 1)
    
    # sensitive 
    sens = map(my_var_names, get_all_perms)  # full orders
    
    # critical 
    crit_pos = map_depth(zz, 2, stri_replace_last_fixed, "=", "<")
    crit_neg = map_depth(zz, 2, ~str_replace(.x, "=","<"))  
    
    if(otherwise <- F){
      crit_pos = map_depth(zz, 2, stri_replace_last_fixed, "=", "<") %>% map_depth(2, ~str_c("0=",.x))
      crit_neg = map_depth(zz, 2, ~str_replace(.x, "=","<")) %>% map_depth(2, ~str_c(.x,"=0"))
    }
    
    # prior probabilities
    n_crit  = map(crit_neg, ~2 * length(.x)) %>% map(~rep(1/.x,.x)/3)
    # if(otherwise) n_crit  = map(crit_pos, ~2 * length(.x)) %>% map(~rep(1/.x,.x)/3)
    n_sens  = map(sens, length) %>% map(~rep(1/.x,.x)/3)
    n_accum = map(accum, length) %>% map(~rep(1/.x,.x)/3)
    prior   = map2(n_accum, n_crit, c) %>% map2(n_sens, c )
    
    hyp = 
      list(accum, crit_neg, crit_pos, sens) %>%
      map_depth(2, str_c, collapse = ";") %>% reduce(str_c, sep = ";")
    
    if(only_order_hyps) {
      prior   = map(n_sens, ~.x/sum(.x)) 
      hyp     = sens %>% map(str_c, collapse = ";") 
    }
    
    return(out = list(prior = prior, hyp = hyp))
  }


get_fco_det = function(ret){
  # heuristic: aggregate non-negligble components (hypotheses greater than some epsilon = .1) 
  
  maxH_in_maxM = 
    ret %>% 
    map(group_by, M) %>% 
    map(add_tally, Hp, name = "Mp") %>% 
    map(~mutate(.x, maxMp = max(.$Mp))) %>% # PICK THE MAX PROB MODEL
    map(filter, maxMp == Mp) %>% 
    map(arrange, -Hp) %>% 
    map_df(filter,  Hp == max(Hp)) %>%  # JUST PICK THE MAX PROB HYPOTHESIS
    mutate(H = str_replace_all(H, "<", "|"))
  
  return(maxH_in_maxM = maxH_in_maxM)
}


########################################################
# TIDY OUTPUT FROM lmhyp OBJECT (OUTPUT FROM test_hyp())
########################################################

tidy_det = function(prior, post){
  
  col1 = map(post, "post_prob")  %>% map(enframe, name = "name", value = "Hp")
  col2 = map(post, "hypotheses") %>% map(enframe, name = "name", value = "H")
  post = map2(col1, col2, left_join, by = "name")
  
  ret = 
    map(post, mutate, M = str_count(H, "=")) %>% 
    map(~mutate(.x, M = ifelse(M != 0, M - max(M) + 2, M))) %>% 
    map(~mutate(.x, M = str_c("M", M)))
  
  return(ret = ret)
}


########################################################
# CENTRAL FUNCTION (SEE EPI)
########################################################

infer_det = function(object, args, only_order_hyps = F){ 
  
  ########################################################
  # Parameteric
  ########################################################
  
  # infer
  nme   = map(object, coef) %>% map(names) 
  prior = get_hyp(nme, only_order_hyps)
  if(from_scratch <- F){
    if(slower <- T) post = pmap(list(object, prior$hyp, prior$prior), test_hyp)
    if(faster <- F) post = parallel::mcmapply(safely(test_hyp),  object = object, hyp = prior$hyp, priorprob = prior$prior, mc.cores = parallel::detectCores() - 2, SIMPLIFY = F)
    saveRDS(post, "r/output_rank/post.rds")
  } else {
    post = readRDS("r/output_rank/post.rds")
  }
  
  post_tidy = tidy_det(prior = prior, post = post)
  fco_det   = get_fco_det(post_tidy)
  final     = bind_cols(args, fco_det)
  
  # calculate fco, if sensitive "M0" is the max posterior model
  final = 
    mutate(final, dat = post_tidy) %>% 
    filter(M == "M0") %>% 
    mutate(local_fco    = pmap(list(partial_rank = H, dat), find_local_fco),
           local_fco_mu = pmap(list(partial_rank = H, dat), find_local_fco_mean)) %>% 
    select(-dat) %>%
    mutate(local_fco_ci = map(local_fco, 1), 
           local_fco_prob = map(local_fco, 2),
           local_fco_mu_ci = map(local_fco_mu, 1),
           local_fco_mu_prob = map(local_fco_mu, 2),
           n_par = dim,
           # Violations?
           tru = map(trueOrd, str_replace_all, "=", ","),
           viol_local_fco    =  map(list(fco = local_fco_ci,    tru = tru), str_remove_all, "w") %>% pmap_lgl(violated),
           viol_local_mu_fco =  map(list(fco = local_fco_mu_ci, tru = tru), str_remove_all, "w") %>% pmap_lgl(violated),
           # Info preservation?
           n_local_fco_distinctions    = map(local_fco_ci,    str_count, "\\|"),
           n_local_fco_mu_distinctions = map(local_fco_mu_ci, str_count, "\\|"),
           info_preservation_local_fco    = pmap_dbl(list(x = n_local_fco_distinctions, y = n_par), function(x,y) x/(y-1)),
           info_preservation_local_mu_fco = pmap_dbl(list(x = n_local_fco_mu_distinctions, y = n_par), function(x,y) x/(y-1))
    ) 
  
  if(0){
    # print
    select(final_sensitive, true_model, trueOrd, local_fco, local_fco_mu) %>% bind_cols(tibble(viol, info_preservation))
    
    # Notes: local_fco < Mp
    
    # For small delta (delta = 1) in lower dimensions a critical model is chosen
    # over the (correct) sensitive model. But this is the closest critical model to
    # a "quite critical" sensitive ground truth. The method appropriately (?) favors the
    # accumulation model in all situations when delta is very small (0.1), even over
    # the correct sensitive and critical models.
    #
    
    # MAP probability
    nme   = object %>% map(coef) %>% map(names) %>% map(~.x[-1])
    hyp   = object %>% map(coef) %>%  map(~.x[-1]) %>% map(order) %>% map(~str_c("w", .x, collapse = "<"))
    prior = map(nme,
                ~{
                  p = 1/(factorial(length(.x)) + length(.x) + 1) # order, critical and accumulation
                  c(p, 1-p)
                })
    post = pmap(list(object = object, hyp = hyp, prior = prior), test_hyp )
    summ = tidy_det(prior = prior, post = post) %>% map_df(~.x) %>% filter(complete.cases(.)) %>% mutate(H = str_replace_all(H, "<", "|"))
    tot = bind_cols(args, summ)
    
    # print
    tot
    tot %>% gf_point(Hp~delta) %>% gf_facet_wrap(~trueM)
    tot %>% gf_point(Hp~trueM) %>% gf_facet_wrap(~delta)
    
    # so far so, good but...
    tot = tot %>% mutate(obj = object) 
    tot = tot %>% mutate(aue = pmap(list(partial_rank = H, Hp = Hp, obj = obj), dysfunction)) %>% select(-obj)
    ex = 1
    dysfunction(tot$H[ex], tot$Hp[ex], obj = object[[ex]])
    
  }
  
  return(final)
}

########################################################
# SAME BUT FOR MCMC
########################################################

get_out_mcmc =  function(.x, .y) {
  
  # extract data
  X = select(.x, matches("w"))
  C = select(.x, matches("c"))
  y = .x$y
  
  # specify prior
  ones = rep(1, dim(X)[2])
  priors = list(Dalp = list(prior_uniform = 1.0 * ones,
                            prior_corners = 0.1 * ones,
                            prior_central = 10  * ones))  # the inner list could take other prior hyperparams
  
  # derive posterior 
  post_par = pmap(priors, infer, X = X, C = C, y = y) # posterior over model-conditional parameters
  
  saveRDS(post_par, str_c("R/output/raw_server_more_delta/", "case", str_pad(.y, 3, pad = "0")))
  return(out = NULL)
}
