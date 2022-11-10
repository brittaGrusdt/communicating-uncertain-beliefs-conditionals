FS = .Platform$file.sep
# utterance contains words in w_pos but not words in w_neg are replaced by utt, 
# for example:
# w_pos = c("and"), w_neg = c("does not"), utt = "both blocks fall"
# "the blue block and the green block fall" -> "both blocks fall"
summarize_utts = function(df, w_pos, w_neg, utt){
  for(w in w_pos) {
    df[[paste('word', w, sep='_')]] = with(df, str_detect(utt.standardized, w))
  }
  
  for(w in w_neg) {
    df[[paste('word_not', w, sep='_')]] = with(df, !str_detect(utt.standardized, w))
  }
  
  dat <- df %>% group_by(prolific_id, id) %>% 
    pivot_longer(cols = starts_with('word_'), names_to = 'word',
                 values_to = "has_word") %>% 
    group_by(prolific_id, id) %>%
    # filter(has_word) %>% 
    mutate(has_all = sum(has_word),
           utt.standardized =
             case_when(has_all ==  length(w_pos) + length(w_neg) ~ utt,
                       TRUE ~ utt.standardized)
    ) %>%
    dplyr::select(-has_all, -word, -has_word) %>% ungroup() %>% distinct()
  return(dat)
}

standardized.sentences = list(
  bg = "both blocks fall",
  none = "neither block falls",
  g = "green falls but blue does not fall",
  b = "blue falls but green does not fall",
  
  if_gb = "if green falls blue falls",
  if_gnb = "if green falls blue does not fall",
  if_bg = "if blue falls green falls",
  if_bng = "if blue falls green does not fall",
  
  if_nbng = "if blue does not fall green does not fall",
  if_nbg = "if blue does not fall green falls",
  if_ngb = "if green does not fall blue falls",
  if_ngnb = "if green does not fall blue does not fall",
  
  might_g = "green might fall",
  might_b = "blue might fall",
  might_ng = "green might not fall",
  might_nb = "blue might not fall",
  
  only_g = "green falls",
  only_b = "blue falls",
  only_ng = "green does not fall",
  only_nb = "blue does not fall"
);

# (the green and the blue = the blue and the green, etc.)
standardize_sentences = function(df.test){
  df.test = df.test %>% mutate(utt.standardized = as.character(uc_task))
  
  test.standardized <- df.test %>%
    summarize_utts(c("and"), c("does not"), standardized.sentences$bg) %>%
    mutate(utt.standardized = str_replace(utt.standardized, "and", "but")) %>%
    
    summarize_utts(c("neither"), c("does not"), standardized.sentences$none) %>%
    summarize_utts(c("but", "green falls", "blue does not"), c(), standardized.sentences$g) %>%
    summarize_utts(c("but", "blue falls", "green does not"), c(), standardized.sentences$b) %>% 
    summarize_utts(c("if green falls"), c("does not"), standardized.sentences$if_gb) %>%
    summarize_utts(c("if blue falls"), c("does not"), standardized.sentences$if_bg) %>%
    summarize_utts(c("if green does not fall", "blue does not fall"), c(),
                   standardized.sentences$if_ngnb) %>%
    summarize_utts(c("if blue does not fall", "green does not fall"), c(),
                   standardized.sentences$if_nbng) %>%
    summarize_utts(c("if blue falls", "green does not fall"), c(),
                   standardized.sentences$if_bng) %>%
    summarize_utts(c("if green does not fall", "blue falls"), c(),
                   standardized.sentences$if_ngb) %>% 
    summarize_utts(c("if green falls", "blue does not fall"), c(),
                   standardized.sentences$if_gnb) %>%
    summarize_utts(c("if blue does not fall", "green falls"), c(),
                   standardized.sentences$if_nbg) %>%
    
    summarize_utts(c("but", "the green block falls", "the blue block does not"),
                   c(), standardized.sentences$g) %>%
    summarize_utts(c("but", "the blue block falls", "the green block does not"),
                   c(), standardized.sentences$b) %>%
    summarize_utts(c("if the green block falls"), c("does not"), standardized.sentences$if_gb) %>%
    summarize_utts(c("if the blue block falls"), c("does not"), standardized.sentences$if_bg) %>%
    summarize_utts(c("if the green block does not fall", "the blue block does not fall"), c(),
                   standardized.sentences$if_ngnb) %>%
    summarize_utts(c("if the blue block does not fall", "the green block does not fall"), c(),
                   standardized.sentences$if_nbng) %>%
    
    summarize_utts(c("if the blue block falls", "the green block does not fall"), c(),
                   standardized.sentences$if_bng) %>%
    summarize_utts(c("if the green block does not fall", "the blue block falls"), c(),
                   standardized.sentences$if_ngb) %>% 
    summarize_utts(c("if the green block falls", "the blue block does not fall"), c(),
                   standardized.sentences$if_gnb) %>%
    summarize_utts(c("if the blue block does not fall", "the green block falls"), c(),
                   standardized.sentences$if_nbg) %>%
    
    summarize_utts(c("the green block might fall"), c(), standardized.sentences$might_g) %>%
    summarize_utts(c("the blue block might fall"), c(), standardized.sentences$might_b) %>%
    summarize_utts(c("the green block might not fall"), c(), standardized.sentences$might_ng) %>%
    summarize_utts(c("the blue block might not fall"), c(), standardized.sentences$might_nb) %>%
    summarize_utts(c("the green block falls"), c(), standardized.sentences$only_g) %>%
    summarize_utts(c("the blue block falls"), c(), standardized.sentences$only_b) %>%
    summarize_utts(c("the green block does not fall"), c(), standardized.sentences$only_ng) %>%
    summarize_utts(c("the blue block does not fall"), c(), standardized.sentences$only_nb);
  
  utterances <- test.standardized %>% dplyr::select(utt.standardized) %>% unique()
  print('standardized responses:')
  print(utterances)
  return(test.standardized)
}

# translate A/C-responses to real colors
# @arg speaker.model: long format
translate_utterances = function(speaker.model, group = "bg"){
  mapping = tribble(~group, ~A, ~`-A`, ~C, ~`-C`, 
                    "gb", "green falls", "green does not fall",
                    "blue falls", "blue does not fall",
                    "bg", "blue falls", "blue does not fall",
                    "green falls", "green does not fall"
  ) %>% filter(group == (!! group))
  
  df <- speaker.model %>%
    mutate(response=case_when(
      str_detect(response, "-C") ~ str_replace(response, "-C", mapping$`-C`),
      str_detect(response, "C") ~ str_replace(response, "C", mapping$`C`),
      TRUE ~ response)) %>%
    mutate(response=case_when(
      str_detect(response, "-A") ~ str_replace(response, "-A", mapping$`-A`),
      str_detect(response, "A") ~ str_replace(response, "A", mapping$`A`),
      TRUE ~ response))
  df = df %>% 
    mutate(response=case_when(str_detect(response, " >") ~
                                paste("if", str_replace(response, " >", "")),
                              TRUE ~ response)) %>%
    mutate(response=case_when(str_detect(response, "likely") ~ str_replace(response, "falls", "might fall"),
                              TRUE ~ response)) %>%
    mutate(response=case_when(str_detect(response, "likely") ~ str_replace(response, "does not fall", "might not fall"),
                              TRUE ~ response)) %>% 
    mutate(response=case_when(str_detect(response, "might") ~ str_replace(response, "likely", ""),
                              TRUE ~ response)) %>%
    mutate(response=case_when(response=="green falls and blue falls" ~ standardized.sentences$bg,
                              response=="blue falls and green falls" ~ standardized.sentences$bg,
                              response=="green does not fall and blue does not fall" ~
                                standardized.sentences$none,
                              response=="blue does not fall and green does not fall" ~
                                standardized.sentences$none,
                              TRUE ~ response)) %>%
    mutate(response=str_replace(response, "and", "but")) %>%
    mutate(response=case_when(
      response=="green does not fall but blue falls" ~
        "blue falls but green does not fall",
      response=="blue does not fall but green falls" ~
        "green falls but blue does not fall",
      TRUE ~ response)) %>%
    mutate(response=str_trim(response)) %>%
    ungroup() 
  return(df)
}

#@arg df: with column 'prob'
translate_probs_to_utts = function(df){
  dat = df %>% mutate(utt.standardized =
                        case_when(prob=="b" ~ standardized.sentences$b,
                                  prob=="g" ~ standardized.sentences$g,
                                  prob=="bg" ~ standardized.sentences$bg,
                                  prob=="none" ~ standardized.sentences$none,
                                  prob=="p_a" ~ "blue falls",
                                  prob=="p_c" ~ "green falls",
                                  prob=="p_na" ~ "blue does not fall",
                                  prob=="p_nc" ~ "green does not fall",
                                  prob=="p_c_given_a" ~ standardized.sentences$if_bg,
                                  prob=="p_c_given_na" ~ standardized.sentences$if_nbg,
                                  prob=="p_a_given_c" ~ standardized.sentences$if_gb,
                                  prob=="p_a_given_nc" ~ standardized.sentences$if_ngb,
                                  prob=="p_nc_given_a" ~ standardized.sentences$if_bng,
                                  prob=="p_nc_given_na" ~ standardized.sentences$if_nbng,
                                  prob=="p_na_given_c" ~ standardized.sentences$if_gnb,
                                  prob=="p_na_given_nc" ~ standardized.sentences$if_ngnb,
                                  prob=="p_likely_a" ~ "blue might fall",
                                  prob=="p_likely_na" ~ "blue might not fall",
                                  prob=="p_likely_c" ~ "green might fall",
                                  prob=="p_likely_nc" ~ "green might not fall",
                                  TRUE ~ NA_character_)
  )
  return(dat)
}


# computes counts of utterances per stimulus and corresponding ratio
# input is joint data since we want 0-entries if an utterance is never selected
task2_avg_per_stimulus = function(behavioral){
  behav.n_per_stim = behavioral %>% dplyr::select(prolific_id, id) %>% 
    distinct() %>% group_by(id) %>% summarize(N = n(), .groups="drop_last")
  
  behav.count = behavioral %>%
    mutate(human_exp2 = case_when(is.na(human_exp2) ~ 0, TRUE ~ 1)) %>%
    group_by(id, utt.standardized) %>% 
    summarize(count = sum(human_exp2), .groups="drop_last")
  
  behav.avg = left_join(behav.n_per_stim, behav.count, by="id") %>%
    mutate(ratio = count / N)
  return(behav.avg)
}

model_average_per_utterance_type = function(model.avg, params, target_fn){
  df.chunked = model.avg %>%
    mutate(utterance=case_when(
      str_detect(utterance, "likely") ~ str_replace(utterance, "likely", "might"),
      TRUE ~ utterance)
    ) %>% chunk_utterances()
  
  avg.chunked = df.chunked %>% dplyr::select(-best.utt) %>%
    group_by(stimulus, utterance) %>%
    summarize(p=sum(p), .groups="drop_last") %>%
    mutate(best.utt=p==max(p), p=round(p, 3))
  fn = paste(params$target_dir, FS, target_fn, "-chunked.csv", sep="")
  write_csv(avg.chunked, fn)
  message(paste("wrote avg predictions per stimulus for chunked utterances to", fn))
  return(avg.chunked)
}

# considers each empirical data point (#trials x #participants - filtered) 
# and computes the average model predictions across stimuli, taking into 
# account data from all participants, but weighted with respect to 
# prior probability of respective table and causal net 
average_predictions_empirical = function(dat.speaker, params, target_fn){
  mapping = params$bn_ids_mapping %>% dplyr::select(-table_id) %>%
    mutate(p_bn=exp(ll)) %>% group_by(stimulus) %>%
    mutate(p.norm = sum(p_bn), p_bn=p_bn/p.norm)
  
  res = left_join(dat.speaker %>% dplyr::select(-rowid),
                  mapping %>% dplyr::select(-ll, -p.norm), by=c("bn_id", "cn"))
  res.avg = res %>% group_by(stimulus, utterance) %>% 
    summarize(p=sum(probs*p_bn), .groups="drop_last") %>% 
    mutate(best.utt=p==max(p)) %>% rename(response=utterance) %>% 
    translate_utterances() %>% rename(utterance=response) %>%
    mutate(p=round(p, 3))
  
  if(params$save) {
    fn = paste(params$target_dir, FS, target_fn, ".csv", sep="")
    write_csv(res.avg, fn)
    message(paste("wrote avg predictions per stimulus based on all empirical
                  tables to", fn))
    avg.chunked = model_average_per_utterance_type(res.avg, params, target_fn)
  }
  return(res.avg)
}

# 1:1 mapping model predictions with empirically observed tables
join_model_behavioral = function(dat.speaker, params, path_data){
  predictions <- dat.speaker %>%
    dplyr::select(bn_id, utterance, probs) %>%
    rename(response=utterance) %>% 
    translate_utterances() %>%
    rename(utterance=response) %>% group_by(bn_id)
  
  # get mean model prediction across causal nets for each data point
  # (the speaker's predictions only differ very little (e-14) for same
  # table but different causal net)
  df = left_join(
    predictions, 
    params$bn_ids_mapping %>% dplyr::select(bn_id, table_id, p_id, cn),
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
  mapping.ids = readRDS(params$tables_mapping) %>% dplyr::select(-empirical_id)
  res.behavioral = left_join(tables.empiric %>% rename(stimulus=id),
                             mapping.ids, by=c("prolific_id", "stimulus"))
  
  # join model and behavioral data
  behav_model = left_join(res.behavioral, df.model) %>% 
    mutate(model.p = round(model.p, 3), human_exp1 = round(human_exp1, 3))
  
  if(params$save) {
    fn = paste(params$target_dir, "exact-model-behavioral-predictions.csv", sep=FS)
    write_csv(behav_model, fn)
    message(paste("results written to", fn))
  }
  return(behav_model)
}

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

save_utt_frequencies = function(uc_pe_data, target_dir){
  human.exp2.avg = task2_avg_per_stimulus(uc_pe_data)
  fn_utts = paste(target_dir, "behavioral-avg-task2.csv", sep=FS)
  write_csv(human.exp2.avg, fn_utts)
  message(paste('frequency utterances written to:', fn_utts))
  
  # also save for chunked utterances
  df2.chunked = human.exp2.avg %>% rename(utterance = utt.standardized) %>% 
    chunk_utterances() %>% rename(utt_type = utterance)
  df2.chunked.avg = df2.chunked %>% group_by(id, utt_type) %>% 
    summarize(count = sum(count), ratio = sum(ratio), .groups="drop_last")
  
  fn_chunked = paste(target_dir, "behavioral-avg-task2-chunked.csv", sep = FS)
  write_csv(df2.chunked.avg, fn_chunked)
  message(paste('frequency utterance types written to:', fn_chunked))
}
