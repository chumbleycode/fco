

if(0){
  # COMPARE ADJUSTED AND UNADJUSTED FCO
  # UNADJUSTED DOES BETTER!!!
  
  # displays the relationship between the  
  final %>% 
    lm((n_local_fco_distinctions)~sd_theta + (dim)+(n), data =.) %>% 
    broom::tidy()
  
  final %>% 
    select(delta, dim, n, trueOrd, local_fco_ci, 
           local_fco_prob, n_local_fco_distinctions, 
           info_preservation_local_fco, viol_local_fco) %>% 
    mutate(local_fco_prob = map_dbl(local_fco_prob, ~ signif(as.numeric(.x), 3)))
  
  # compares the size-adjusted and unadjusted local_fco
  final %>% 
    filter(sd_theta !=0) %>%
    select(sd_theta, trueOrd, local_fco_ci, local_fco_mu_ci, local_fco_prob, local_fco_mu_prob) #%>% gf_point(local_fco_prob~local_fco_mu_prob)
  
  # size-adjusted is more conservative (more often trivial ). On 83 non-null
  # simulations for which unadjusted ci included one distinction, adjustment
  # resulted in 0 distinctions. Adjusted and unadjusted agreed on the majority of
  # simulations though.
  final %>% 
    filter(sd_theta !=0) %>% 
    with(table(n_local_fco_distinctions == 0,n_local_fco_mu_distinctions == 0))
  
  final %>% 
    filter(sd_theta !=0) %>% 
    with(table(n_local_fco_distinctions ,n_local_fco_mu_distinctions))
  # local_fco without size-adjustment (row variable) preserves more information
  final %>% filter(sd_theta !=0) %>% with(table(signif(info_preservation_local_fco,3), signif(info_preservation_local_mu_fco,3)))
  # the difference between size-adjusted and unadjusted results diminishes as sample size increases.
  final %>% filter(sd_theta !=0) %>% split(.$n) %>% map(~.x %>% with(table(n_local_fco_distinctions, n_local_fco_mu_distinctions)))
  # break up by dimensinality
  final %>% filter(sd_theta !=0) %>% split(.$dim) %>% map(~.x %>% with(table(n_local_fco_distinctions, n_local_fco_mu_distinctions)))
  
  
  final %>% filter(sd_theta !=0) %>% select(n_local_fco_distinctions, n_local_fco_mu_distinctions, info_preservation_local_fco, info_preservation_local_mu_fco)
}