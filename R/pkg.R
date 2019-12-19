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


find_local_fco = 
  function(partial_rank, dat, thresh = .9){
    
    # Check if the most probable full order passes critereon 
    # This condition applies only iteration 1 of recursive call 
    # (which starts at the max probable full order)
    init  = str_replace_all(partial_rank,"\\|", "<")
    init_is_full_order = str_count(init,"w") == (str_count(init, "<") + 1)
    if(init_is_full_order){
      initp = filter(dat, H == init)$Hp
      if(initp > thresh) return(c(partial_rank, initp))
    }
    
    coarser    = 
      get_coarse_wux(partial_rank) %>%
      map_depth(2, combinat::permn) %>%
      map_depth(3, str_c, collapse = "<") %>% 
      map(cross) %>% 
      map_depth(2, str_c, collapse = "<") %>% 
      map(unlist)
    probs      = map(coarser, ~filter(dat, H %in% .x)) %>% map(summarise, sum(Hp)) 
    max_prob   = max(unlist(probs))
    max_event  = probs %>% keep(~.x == max_prob) %>% names %>% `[[`(1) # the last step in case of multiple max
    
    if(max_prob > thresh | !str_detect(max_event, "\\|")){ 
      return(c(max_event, max_prob)) 
    } else {  
      rbind(c(max_event, max_prob),
            find_local_fco(max_event, dat))
    } 
  }

get_coarse_wux = function(obj){
  # gets each element of the 1 step coarser/weaker order
  
  for(ii in str_locate_all(obj, "\\|")[[1]][,1]){
    
    init = str_split(obj, "\\|")[[1]]
    aa = head(unlist(str_remove(init,"\\|") %>% map(.,~c(.x, "|"))), -1)
    ll = 1
    qqq = NULL
    naaa = NULL
    for(ii in  which(aa == "|")){
      aa = head(unlist(str_remove(init,"\\|") %>% map(.,~c(.x, "|"))), -1)
      aa[ii] = ","
      naaa[ll] = str_c(aa, collapse = "")
      qqq[ll] = aa %>% str_c(collapse  = "") %>% str_split("\\|") %>% map(as.list) %>% map(~str_split(.x, ","))
      ll = ll + 1
    }
  }
  
  qqq = set_names(qqq, naaa)
  return(qqq = qqq)
}


tidy_output = function(max_rnk, fco){
  # eats the max probability full rank, and the output from find_local_fco()
  # returns a tidy tibble
  
  rbind(c(V1 = max_rnk$H,
          V2 = max_rnk$Hp),
        fco) %>% 
    as_tibble %>% 
    transmute(H = V1, prob = signif(as.numeric(V2), 3))
}
