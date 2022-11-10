library(here)
save_data <- function(data, target_path){
  target_dir = paste(head(str_split(target_path, .Platform$file.sep)[[1]],
                          -1), collapse="/")
  if(!dir.exists(target_dir)){dir.create(target_dir, recursive=TRUE)
  }
  data %>% write_rds(target_path)
  message(paste("saved to:", target_path))
}

filter_vars <- function(df_long, vars){
  df <- df_long %>% mutate(keep=TRUE)
  for(var in vars){
    if(str_detect(var, "^-")){
      # negative variable, starts with -
      df <- df %>% mutate(keep=case_when(!keep ~ keep, TRUE ~ str_detect(cell, var)))
    }
    else {
      token <- paste("-", var, sep="")
      df <- df %>% mutate(keep=case_when(!keep ~ keep, TRUE ~ !str_detect(cell, token)))
    }
  }
  # %>% filter(keep) %>% dplyr::select(-keep)
  return(df)
}


# Utterances --------------------------------------------------------------
generate_utts <- function(params){
  utterances <- run_webppl(here("model", "webppl-model", "default-model",
                                "utterances.wppl"), params)
  utts <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  utts %>% save_data(params$utts_path)
  return(utts)
}

# instead of all different utterances, chunk them into categories (for plotting)
# @arg data: with column 'utterance'
chunk_utterances <- function(data, utts_kept=c()){
  levels = c("might + literal", "conditional", "literal", "conjunction");
  s = paste(utts_kept, collapse="");
  if(str_detect(s, ">") || str_detect(s, "if")){
    levels = c("might + literal", "other conditional", "literal", "conjunction",
               utts_kept);
  }
  data = data %>% mutate(
    utterance = case_when(
      utterance %in% utts_kept ~ utterance,
      str_detect(utterance, "might") ~ "might + literal",
      (str_detect(utterance, "if") | str_detect(utterance, ">")) ~ levels[[2]],
      (str_detect(utterance, "and") | str_detect(utterance, "but") |
         str_detect(utterance, "neither") | str_detect(utterance, "both")) ~ "conjunction",
      TRUE ~ "literal"
    ),
    utterance = factor(utterance, levels=
                         c(map(utts_kept, function(s){
                           s <- str_replace_all(s, "-", "¬")
                           return(str_replace(s, ">", "->"))
                         }),
                         levels)
    )
  );
  return(data)
}


# Probabilities -----------------------------------------------------------
compute_cond_prob <- function(distr_wide, prob){
  if(prob=="P(C|A)"){
    distr <- distr_wide %>% mutate(p=`AC`/(`AC`+`A-C`))
  }else if(prob=="P(-C|-A)"){
    distr <- distr_wide %>% mutate(p=`-A-C`/(`-AC`+`-A-C`))
  }else if(prob=="P(A|C)"){
    distr <- distr_wide %>% mutate(p=`AC`/(`-AC`+`AC`))
  }else if(prob=="P(A|-C)"){
    distr <- distr_wide %>% mutate(p=`A-C`/(`A-C`+`-A-C`))
  }else if(prob=="P(-A|-C)"){
    distr <- distr_wide %>% mutate(p=`-A-C`/(`A-C`+`-A-C`))
  } else if(prob=="P(C|-A)"){
    distr <- distr_wide %>% mutate(p=`-AC`/(`-AC`+`-A-C`))
  } else if(prob == "P(-C|-A)"){
    distr <- distr_wide %>% mutate(p=`-A-C`/(`-AC`+`-A-C`))
  }else if(prob == "P(-C|A)"){
    distr <- distr_wide %>% mutate(p=`A-C`/(`AC`+`A-C`))
  }else if(prob == "P(-A|C)"){
    distr <- distr_wide %>% mutate(p=`-AC`/(`AC`+`-AC`))
  }
  else{
    stop("not implemented.")
  }
  return(distr)
}


# model ------------------------------------------------------------------
# ** computes log likelihood for independent net and  A->C / C->A for dep nets**
likelihood <- function(df_wide, sigma_indep, a=10, b=1){
  # prepare
  df <- df_wide %>%
    compute_cond_prob("P(C|A)") %>% rename(p_c_given_a=p) %>% 
    compute_cond_prob("P(C|-A)") %>% rename(p_c_given_na=p) %>% 
    compute_cond_prob("P(A|C)") %>% rename(p_a_given_c=p) %>% 
    compute_cond_prob("P(A|-C)") %>% rename(p_a_given_nc=p) %>%
    mutate(pa=AC+`A-C`, pc=AC+`-AC`,
           ind.lower=case_when(1-(pa+pc) < 0 ~ abs(1-(pa+pc)),
                               TRUE ~ 0),
           ind.upper=pmin(pa, pc))
  
  df <- df %>% 
    mutate(
      p_nc_given_a = 1 - p_c_given_a,
      p_na_given_c = 1 - p_a_given_c,
      p_nc_given_na = 1 - p_c_given_na,
      p_na_given_nc = 1 - p_a_given_nc,
      
      logL_ind=log(dtruncnorm(x=`AC`, a=ind.lower, b=ind.upper, mean=pa*pc, sd=sigma_indep)),
      logL_if_ac = log(dbeta(p_c_given_a, a, b))+log(dbeta(p_c_given_na, b, a)),
      logL_if_anc = log(dbeta(p_nc_given_a, a, b)) + log(dbeta(p_nc_given_na, b, a)),
      logL_if_ca = log(dbeta(p_a_given_c, a, b)) + log(dbeta(p_a_given_nc, b, a)),
      logL_if_cna = log(dbeta(p_na_given_c, a, b)) + log(dbeta(p_na_given_nc, b, a))
    ) %>% 
    dplyr::select(-p_c_given_na, -p_c_given_a, -p_a_given_c, -p_a_given_nc, -pa, -pc,
                  -p_nc_given_a, -p_na_given_c, -p_nc_given_na, -p_na_given_nc)
  return(df)
}

# other functions ---------------------------------------------------------

#@arg config_keys: order in config_keys is important since same key values
# are overwritten!
configure <- function(config_keys) {
  config_file = yaml::yaml.load_file(here("model", "config.yml"), eval.expr=TRUE)
  params = c() 
  # latest read config key is added first, as the first added key does not 
  # get overwritten by later ones, therefore default in the end
  for(key in rev(config_keys)){
    params = c(params, config_file[[key]])
  }
  params = c(params, config_file$default)
  return(params)
}

# plotting functions ------------------------------------------------------

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
