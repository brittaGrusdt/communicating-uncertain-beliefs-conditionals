library(here)
save_data <- function(data, target_path){
  target_dir = paste(head(str_split(target_path, .Platform$file.sep)[[1]],
                          -1), collapse="/")
  if(!dir.exists(target_dir)){dir.create(target_dir, recursive=TRUE)
  }
  data %>% write_rds(target_path)
  print(paste("saved to:", target_path))
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
sort_utterances <- function(utterances){
  literals <- c("A", "C", "-A", "-C")
  conjs <- c("C and A", "-C and A", "C and -A", "-C and -A")
  likely <- c("likely A", "likely C", "likely -A", "likely -C")
  ifs <- c("A > C", "A > -C", "-A > C", "-A > -C",
           "C > A", "C > -A", "-C > A", "-C > -A")
  return(utterances[order(match(utterances, c(conjs, literals, ifs, likely)))])
}

add_pspeaker_max_conj_lit <- function(df){
  df <- df %>% mutate(pmax_conj_lit =
                        max(A, `-A`, C, `-C`,
                            `C and A`, `C and -A`, `-C and A`,`-C and -A`))
  return(df)
}

generate_utts <- function(params){
  utterances <- run_webppl(here("model", "webppl-model", "default-model",
                                "utterances.wppl"), params)
  utts <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  utts %>% save_data(params$utts_path)
  return(utts)
}

# columns: bn_id, probs, utterance
best_utterance = function(data.speaker){
  data.speaker.best <- data.speaker %>% group_by(bn_id) %>%
    mutate(p_best=max(probs), u_best=list(utterance[probs == max(probs)])) %>%
    unnest(u_best) %>% dplyr::select(-p_best)
  return(data.speaker.best)
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
chunk_cns <- function(data) {
  data = data %>% mutate(cn = case_when(cn == "A || C" ~ "A,C independent",
                                        TRUE ~ "A,C dependent"),
                         cn = factor(cn))
  return(data)
}


# Probabilities -----------------------------------------------------------
#@arg vars: list of variables, if more than one, only states where all hold
# are retained!
# data must be in long format, such that cell is one column and marginals can
# be computed for any cell entries, returned object is in wide format
marginalize <- function(data, vars){
  df <- data %>% filter_vars(vars)
  df <- df %>%  mutate(p=case_when(keep ~ val, TRUE ~ 0)) %>%
    group_by(bn_id, level) %>% mutate(p=sum(p))  %>%
    dplyr::select(-keep) %>% spread(key=cell, val=val, fill = 0)
  
  return(df)
}

# takes the expected value of column *p* with probability in column *prob*
# args: df_wide; tibble with one bn per row, at least columns *p*, *prob*, *level*
#       value_str: str describing value, e.g. *P(A)* for expected val of P(A)
expected_val <- function(df_wide, value_str){
  evs <- df_wide %>% mutate(ev_prod=p * prob)
  evs <- evs %>% group_by(level)
  evs <- evs %>% summarise(ev=sum(ev_prod), .groups="drop") %>% add_column(p=value_str) %>% ungroup()
  
  # fill non-existent levels for plotting
  levels <- evs$level 
  if(is.na(match("prior", levels))){
    evs <- evs %>% add_row(level="prior", ev=0, p=value_str)}
  if(is.na(match("LL", levels))){
    evs <- evs %>% add_row(level="LL", ev=0, p=value_str)}
  if(is.na(match("PL", levels))){
    evs <- evs %>% add_row(level="PL", ev=0, p=value_str)}
  return(evs)
}

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
add_model_params <- function(df, params){
  df <- df %>% mutate(cost=params$cost_conditional,
                      alpha=params$alpha,
                      bias=params$bias,
                      value=as.character(value))
  return(df)
}

filter_by_model_params <- function(df, params){
  df <- df %>% filter(cost==params$cost_conditional &
                        alpha==params$alpha)
  return(df)
}

# computes log_likelihood only for given cn
get_likelihood = function(df_wide, par){
  data = group_map(df_wide %>% group_by(cn), function(df, grp){
    cn=grp$cn
    if(cn=="A implies C"){
      vals=c("P(C|A)", "P(C|-A)")
      df = df %>% mutate(p_marg=AC + `A-C`)
    } else if(cn == "A implies -C"){
      vals=c("P(-C|A)", "P(-C|-A)")
      df = df %>% mutate(p_marg=AC + `A-C`)
    } else if(cn == "C implies A"){
      vals=c("P(A|C)", "P(A|-C)")
      df = df %>% mutate(p_marg=AC + `-AC`)
    } else if(cn == "C implies -A"){
      vals = c("P(-A|C)", "P(-A|-C)")
      df = df %>% mutate(p_marg=AC + `-AC`)
    }
    if(cn == "A || C"){
      df = df %>% 
        mutate(pa=AC+`A-C`, pc=AC+`-AC`,
               ind.lower=case_when(1-(pa+pc) < 0 ~ abs(1-(pa+pc)),
                                 TRUE ~ 0),
               ind.upper=pmin(pa, pc))
      df.cn = df %>%
        mutate(ll=dtruncnorm(x=`AC`, a=ind.lower, b=ind.upper,
                             mean=pa*pc, sd=par$indep_sigma)) %>%
        dplyr::select(-pa, -pc, -ind.lower, -ind.upper)
      
    }else {
      df.cn <- df %>%
      compute_cond_prob(vals[1]) %>% rename(p_pos=p) %>% 
      compute_cond_prob(vals[2]) %>% rename(p_neg=p) %>% 
      mutate(ll = dbeta(p_pos, par$beta_pos_a, par$beta_pos_b, log=T) +
                  dbeta(p_neg, par$beta_neg_a, par$beta_neg_b, log=T) + 
                  dbeta(p_marg, par$beta_marg_a, par$beta_marg_b, log=T)
      ) %>%
      dplyr::select(-p_marg, -p_pos, -p_neg)
    }
  return(df.cn %>% add_column(cn=(!! cn)))
  });
  return(bind_rows(data))
}


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

add_meaning_probs = function(df.wide){
  return(df.wide %>%
           compute_cond_prob("P(C|A)") %>% rename(pc_given_a=p) %>%
           compute_cond_prob("P(A|C)") %>% rename(pa_given_c=p) %>%
           mutate(pa=`AC`+`A-C`, pc=`AC`+`-AC`));
}

# other functions ---------------------------------------------------------
hellinger <- function(p, q){
  (1/sqrt(2)) * sqrt(sum((sqrt(p)-sqrt(q))^2))
}

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

plot_evs <- function(data){
  p <- data %>% ggplot() +
    geom_bar(mapping = aes(x=level, y=ev, fill=level), stat="identity", position="dodge") +
    labs(x="", y="", title="") +
    coord_flip() +
    theme_classic(base_size = 20) +
    theme(legend.position="none")
  return(p)
}

plot_speaker <- function(data, fn, w, h, legend_pos="none", facets=TRUE,
                         xlab="", ylab=""){
  df <- data %>% mutate(p=round(as.numeric(p), 2))
  if(xlab==""){xlab = TeX("$\\frac{1}{|S|} \\cdot \\sum_{s \\in S} P_S(u|s)$")}
  if(ylab==""){ylab = "utterance"}
  
  if("cn" %in% colnames(df)) {p <- df %>%
    ggplot(aes(y=utterance, x=p, fill=cn)) +
    guides(fill=guide_legend(title="causal net"))
  } else if("speaker_condition" %in% colnames(df)) {
    p <-  df %>% ggplot(aes(y=utterance, x=p, fill=speaker_condition))
  } else {
    p <-  df %>% ggplot(aes(y=utterance, x=p))
  }
  p <- p +
    geom_bar(stat="identity", position=position_dodge(preserve = "single"))  +
    labs(x=xlab, y=ylab) + theme_bw(base_size=25)
  if(facets) p <- p + facet_wrap(~speaker_condition)
  p <- p + theme(axis.text.y=element_text(size=15), legend.position=legend_pos)
  
  ggsave(paste(PLOT_DIR, fn, sep=SEP), p, width=w, height=h)
  return(p)
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
