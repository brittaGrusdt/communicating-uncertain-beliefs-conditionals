# Function Definitions ----------------------------------------------------
#' Computes a conditional probability for prob table AC, A-C, -AC, -A-C.
#'
#' @param distr_wide tibble with columns 'AC', 'A-C', '-AC', '-A-C'
#' @param prob str P(x|y) where x,y are one of: A, -A, C, -C
#' @return tibble with column p added containing computed conditional probability
#' @import dplyr
#' @import tibble
#'
compute_cond_prob <- function(distr_wide, prob){
  if(prob == "P(C|A)"){
    distr <- distr_wide %>% mutate(p =`AC`/(`AC`+`A-C`))
  }else if(prob == "P(-C|-A)"){
    distr <- distr_wide %>% mutate(p =`-A-C`/(`-AC`+`-A-C`))
  }else if(prob == "P(A|C)"){
    distr <- distr_wide %>% mutate(p =`AC`/(`-AC`+`AC`))
  }else if(prob == "P(A|-C)"){
    distr <- distr_wide %>% mutate(p =`A-C`/(`A-C`+`-A-C`))
  }else if(prob == "P(-A|-C)"){
    distr <- distr_wide %>% mutate(p =`-A-C`/(`A-C`+`-A-C`))
  } else if(prob == "P(C|-A)"){
    distr <- distr_wide %>% mutate(p =`-AC`/(`-AC`+`-A-C`))
  } else if(prob == "P(-C|-A)"){
    distr <- distr_wide %>% mutate(p =`-A-C`/(`-AC`+`-A-C`))
  }else if(prob == "P(-C|A)"){
    distr <- distr_wide %>% mutate(p =`A-C`/(`AC`+`A-C`))
  }else if(prob == "P(-A|C)"){
    distr <- distr_wide %>% mutate(p =`-AC`/(`AC`+`-AC`))
  }
  else{
    stop("not implemented.")
  }
  return(distr)
}

#' @import dplyr
#' @import tibble
#' @import readr
join_model_behavioral_averages = function(path_behav.by_context,
                                          path_model.by_context){
  model.by_context = read_csv(path_model.by_context) %>% rename(model = p)
  message(paste('read model results from:', path_model.by_context))

  behav.by_context = read_csv(path_behav.by_context) %>%
    rename(stimulus = id, behavioral = ratio)
  message(paste('read behavioral results from ', path_behav.by_context))

  data.joint = left_join(
    behav.by_context %>% dplyr::select(stimulus, utt.standardized, behavioral),
    model.by_context %>% dplyr::select(-best.utt) %>% rename(utt.standardized = utterance),
    by=c("stimulus", "utt.standardized")
  )
  return(data.joint)
}

#' @import dplyr
#' @import tibble
#' @import ExpDataWrangling
translate_standardized2model = function(df) {
  df <- df %>%
    mutate(utterance =
             case_when(utt.standardized == standardized.sentences$bg ~ "C and A",
                       utt.standardized == standardized.sentences$none ~ "-C and -A" ,
                       utt.standardized == standardized.sentences$g ~ "C and -A",
                       utt.standardized == standardized.sentences$b ~ "-C and A",
                       utt.standardized == standardized.sentences$if_bg ~ "A > C",
                       utt.standardized == standardized.sentences$if_bng ~ "A > -C",
                       utt.standardized == standardized.sentences$if_nbg ~ "-A > C",
                       utt.standardized == standardized.sentences$if_nbng ~ "-A > -C",
                       utt.standardized == standardized.sentences$if_gb ~ "C > A",
                       utt.standardized == standardized.sentences$if_gnb ~ "C > -A",
                       utt.standardized == standardized.sentences$if_ngb ~ "-C > A",
                       utt.standardized == standardized.sentences$if_ngnb ~ "-C > -A",
                       utt.standardized == standardized.sentences$only_g ~ "C",
                       utt.standardized == standardized.sentences$only_ng ~ "-C",
                       utt.standardized == standardized.sentences$only_b ~ "A",
                       utt.standardized == standardized.sentences$only_nb ~ "-A",
                       utt.standardized == standardized.sentences$might_g ~ "might C",
                       utt.standardized == standardized.sentences$might_ng ~ "might -C",
                       utt.standardized == standardized.sentences$might_b ~ "might A",
                       utt.standardized == standardized.sentences$might_nb ~ "might -A"
             )
    )
  return(df)
}

#' 1:1 mapping model predictions with empirically observed tables
#' @import dplyr
#' @import tibble
#' @import readr
join_model_behavioral = function(dat.speaker, bn_ids.mapping, tables.mapping,
                                 path_data, path_results = NA){
  predictions <- dat.speaker %>%
    dplyr::select(bn_id, utterance, probs) %>%
    rename(response = utterance) %>%
    translate_utterances() %>%
    rename(utterance = response) %>% group_by(bn_id)

  # get mean model prediction across causal nets for each data point
  # (the speaker's predictions only differ very little (e-14) for same
  # table but different causal net)
  df = left_join(
    predictions,
    bn_ids.mapping %>% dplyr::select(bn_id, table_id, p_id, cn),
    by="bn_id"
  ) %>% group_by(table_id, p_id, utterance) %>%
    mutate(max_p=pmax(probs), min_p=pmin(probs), mean_p=mean(probs),
           max_diff1=max_p-mean_p, max_diff2=mean_p-min_p)

  df.model = df %>%  group_by(table_id, p_id, utterance) %>%
    summarize(probs=mean(probs), .groups="drop_last") %>%
    separate(p_id, into=c("prolific_id", "rel", "prior"), sep="_") %>%
    unite("stimulus", "rel", "prior", sep="_") %>%
    rename(model.p=probs)

  data.behav = read_csv(path_data)
  tables.empiric = data.behav %>%
    dplyr::select(prolific_id, id, pe_task.smooth, utt.standardized,
                  human_exp2) %>%
    rename(utterance = utt.standardized, human_exp1 = pe_task.smooth)
  message(paste("read empiric data from:", path_data))

  # join behavioral results with ids to be able to merge with model predictions
  res.behavioral = left_join(tables.empiric %>% rename(stimulus=id),
                             tables.mapping %>% dplyr::select(-empirical_id),
                             by=c("prolific_id", "stimulus"))

  # join model and behavioral data
  behav_model = left_join(res.behavioral, df.model) %>%
    mutate(model.p = round(model.p, 3), human_exp1 = round(human_exp1, 3))

  if(!is.na(path_results)) {
    write_csv(behav_model, path_results)
    message(paste("results written to", path_results))
  }
  return(behav_model)
}

#' @import dplyr
#' @import tibble
model_average_per_utterance_type = function(model.avg, path_results=NA){
  df.chunked = model.avg %>%
    mutate(utterance=case_when(
      str_detect(utterance, "likely") ~ str_replace(utterance, "likely", "might"),
      TRUE ~ utterance)
    ) %>% chunk_utterances()

  avg.chunked = df.chunked %>% dplyr::select(-best.utt) %>%
    group_by(stimulus, utterance) %>%
    summarize(p=sum(p), .groups="drop_last") %>%
    mutate(best.utt=p==max(p), p=round(p, 3))
  if(!is.na(path_results)){
    write_csv(avg.chunked, path_results)
    message(paste("wrote avg predictions per stimulus for chunked utterances to",
                  path_results))
  }
  return(avg.chunked)
}


#' considers each empirical data point (#trials x #participants - filtered)
#' and computes the average model predictions across stimuli, taking into
#' account data from all participants, but weighted with respect to
#' prior probability of respective table and causal net
#' @import dplyr
#' @import tibble
#' @import readr
model_prediction_by_context = function(
    dat.speaker, bn_ids.mapping, path_results=NA, path_results_chunked = NA
    ){
  prior_s_given_Di = bn_ids.mapping %>% dplyr::select(-table_id) %>%
    mutate(p_bn=exp(ll)) %>% group_by(stimulus) %>%
    mutate(p.norm = sum(p_bn), p_bn=p_bn/p.norm) %>% dplyr::select(-ll, -p.norm)

  # combines speaker predictions with respective model predictions
  results = left_join(dat.speaker %>% dplyr::select(-rowid),
                      prior_s_given_Di, by=c("bn_id", "cn"))
  predictions.by_context = results %>% group_by(stimulus, utterance) %>%
    summarize(p = sum(probs * p_bn), .groups="drop_last") %>%
    mutate(best.utt=p == max(p)) %>% rename(response = utterance) %>%
    translate_utterances() %>% rename(utterance = response) %>%
    mutate(p = round(p, 3))

  if(!is.na(path_results)) {
    write_csv(predictions.by_context, path_results)
    message(paste("wrote avg predictions per stimulus based on all empirical tables to",
                  path_results))
    avg.chunked = model_average_per_utterance_type(predictions.by_context,
                                                   path_results_chunked)
  }
  return(predictions.by_context)
}

# Acceptability/Assertability conditions ----------------------------------
#' p_rooij: (P(e|i) - P(e|¬i)) / (1-P(e|¬i))
#' p_delta: P(e|i) - P(e|¬i)
#'
#' @import dplyr
#' @import tibble
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

