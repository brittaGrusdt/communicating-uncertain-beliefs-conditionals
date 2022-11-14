library(here)
library(ExpDataWrangling)
source(here("R", "model-tables.R"))

# Utterances --------------------------------------------------------------
generate_utts <- function(params, path_to_target){
  utterances <- run_webppl(here("model", "webppl-model", "default-model",
                                "utterances.wppl"), params)
  utts <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  save_data(utts, path_to_target)
  return(utts)
}


# Acceptability/Assertability conditions ----------------------------------
# p_rooij: (P(e|i) - P(e|¬i)) / (1-P(e|¬i))
# p_delta: P(e|i) - P(e|¬i)
acceptability_conditions <- function(data_wide){
  df <- data_wide %>% compute_cond_prob("P(C|A)") %>% rename(p_c_given_a=p) %>% 
    compute_cond_prob("P(C|-A)") %>% rename(p_c_given_na=p) %>%
    mutate(p_delta=round(p_c_given_a - p_c_given_na, 5),
           p_nc_given_na=round(1-p_c_given_na, 5),
           p_rooij=case_when(p_nc_given_na == 0 ~ round(p_delta/0.00001, 5),
                             TRUE ~ round(p_delta/p_nc_given_na, 5)),
           pc=`AC` + `-AC`,
           p_diff=round(p_c_given_a - pc, 5)) %>%
    dplyr::select(-p_nc_given_na, -p_c_given_a, -p_c_given_na, -pc)
  return(df)
}
