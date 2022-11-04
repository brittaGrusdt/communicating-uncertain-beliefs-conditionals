library(tidyverse)
library(here)
library(matrixStats)
source(here("model", "R", "helpers-tables.R"))

FS = .Platform$file.sep
epsilon = 0.000001
seed_fitted_tables = "20202020"


retrieve_test_pe_data <- function(df){
  dat.test <- df %>% filter(str_detect(trial_name, "multiple_slider")) %>%
    dplyr::select(trial_name, trial_number,
                  prolific_id, RT, QUD, id, group,
                  question1, question2, question3, question4,
                  response1, response2, response3, response4) %>%
    pivot_longer(cols=c(contains("response")),
                 names_to = "response_idx", names_prefix = "response",
                 values_to = "response") %>%
    pivot_longer(cols=c(contains("question")),
                 names_to = "question_idx", names_prefix = "question",
                 values_to = "slider") %>%
    filter(response_idx == question_idx) %>%
    dplyr::select(-response_idx, -question_idx)
  
  dat.test <- dat.test %>%
    mutate(response = as.numeric(response),
           response = response / 100,
           prolific_id = factor(prolific_id),
           id = factor(id)) %>%
    add_smoothed_exp1() %>% rename(pe_task = response)
  return(dat.test)
}

retrieve_test_uc_data <- function(df){
  dat.test <- df %>%
    filter(startsWith(trial_name, "fridge_view")) %>%
    dplyr::select(prolific_id, RT, QUD, id, group, response1, response2,
                  trial_name, trial_number, cost.uc) %>%
    rename(custom_response = response2, uc_task = response1)
  
  dat.test <- dat.test %>%
    mutate(prolific_id = factor(prolific_id), id = factor(id))
  return(dat.test)
}

retrieve_train_pe_data <- function(df){
  dat.train <- df %>%
    filter(startsWith(trial_name, "animation") |
           trial_name == "multiple_slider_train") %>%
    dplyr::select(prolific_id, RT, expected, QUD, id, trial_name,
                  trial_number,
                  question1, question2, question3, question4,
                  response1, response2, response3, response4
    ) %>%
    pivot_longer(cols=c(contains("response")),
                 names_to = "response_idx", names_prefix = "response",
                 values_to = "response") %>%
    pivot_longer(cols=c(contains("question")),
                 names_to = "question_idx", names_prefix = "question",
                 values_to = "question") %>%
    filter(response_idx == question_idx) %>%
    dplyr::select(-response_idx, -question_idx) %>%
    mutate(prolific_id = factor(prolific_id), id = factor(id)) %>%
    group_by(prolific_id, id) %>%
    mutate(response = as.numeric(response), response = response/100) %>%
    add_smoothed_exp1()
  return(dat.train)
}

retrieve_data <- function(raw_data, N_trials){
  # 1. dplyr::select only columns relevant for data analysis
  df <- raw_data %>%
    dplyr::select(prolific_id, submission_id,
                  question, question1, question2, question3, question4,
                  QUD, response,
                  expected, response1, response2, response3, response4,
                  id, trial_name, trial_number, group,
                  timeSpent, RT, cost,
                  education, comments, gender, age) %>% 
    rename(cost.uc = cost) %>% 
    mutate(response3 = as.character(response3),
           response4 = as.character(response4))
  dat.color_vision <- tibble();
  if(N_trials$color_vision != 0) {
    dat.color_vision <- df %>%
      filter(startsWith(trial_name, "color-vision")) %>%
      dplyr::select(prolific_id, id, question, response, expected, QUD, trial_number)
    df <- df %>% filter(!startsWith(trial_name, "color-vision"));
  }
  dat.slider_choice = tibble()
  dat.attention_check = tibble()
  if(N_trials$slider_choice != 0){
    cols = c("prolific_id", "id", "question", "response", "expected",
             "trial_name", "trial_number")
    dat.slider_choice = df %>%
      filter(startsWith(trial_name, "slider_choice_training")) %>%
      dplyr::select(one_of(cols))
    dat.attention_check = df %>%
      filter(startsWith(trial_name, "attention_check")) %>%
      dplyr::select(one_of(cols))
  }
  N_participants <- df %>% dplyr::select(prolific_id) %>% unique() %>% nrow()
  stopifnot(nrow(df) == N_participants * (N_trials$test + N_trials$train));
  
  dat.comments <- df %>%
    dplyr::select(prolific_id, comments) %>%
    mutate(comments = as.character(comments),
           comments = if_else(is.na(comments), "", comments)) %>%
    unique()
  dat.info <- df %>% 
    dplyr::select(prolific_id, education, gender, age, timeSpent) %>% 
    distinct() %>% 
    mutate(gender = case_when(is.na(gender) ~ "not specified", 
                                             T ~ gender),
           education = case_when(is.na(education) ~ "not specified",
                           T ~ education))
  dat.train <- retrieve_train_pe_data(df)
  dat.test.pe <- df %>% filter(str_detect(trial_name, "multiple_slider")) %>%
    retrieve_test_pe_data() 
  dat.test.uc <- df %>% filter(str_detect(trial_name, "fridge_view")) %>%
    retrieve_test_uc_data() 
  
  dat.all <- list(test.pe_task = dat.test.pe, 
                  test.uc_task = dat.test.uc,
                  train.pe = dat.train,
                  color = dat.color_vision,
                  train.attention = dat.attention_check,
                  train.slider_choice = dat.slider_choice,
                  comments=dat.comments,
                  info=dat.info) 
  
  return(dat.all)
}

# standardize all to group1!
standardize_color_groups_exp1 <- function(df){
  # ind2 is used as single training example for production task (always group1!)
  df <- df %>%
    mutate(slider =
             case_when((slider == "bg" | slider == "gb" |
                          slider=="ry" | slider == "yr") ~ "ac",
                       slider == "none" ~ "none",
                       group=="group1" & (slider=="b" | slider=="r") ~ "a",
                       group=="group1" & (slider=="g" | slider=="y") ~ "c",
                       group=="group2" & slider=="g"  ~ "a",
                       group=="group2" & slider=="b" ~ "c"
             ),
           group = "group1",
           slider = case_when(slider == "a" ~ "b",
                              slider == "c" ~ "g",
                              slider == "ac" ~ "bg",
                              slider == "none" ~ "none")
    )
  return(df)
}

# standardizes selected responses + custom responses as if all were in group1
standardize_color_groups_exp2 <- function(df){
  df <- df %>%
    mutate(uc_task=
            case_when(group == "group2" ~ str_replace_all(uc_task, "blue", "G"),
                      T ~ str_replace_all(uc_task, "blue", "B")),
           custom_response=
             case_when(group == "group2" ~ str_replace_all(custom_response, "blue", "-G-"),
                       T ~ str_replace_all(custom_response, "blue", "-B-"))) %>%
    
    mutate(uc_task = case_when(group == "group2" ~ str_replace_all(uc_task, "green", "B"),
                               T ~ str_replace_all(uc_task, "green", "G")),
           custom_response =
             case_when(group == "group2" ~ str_replace_all(custom_response, "green", "-B-"),
                       T ~ str_replace_all(custom_response, "green", "-G-"))) %>%
    mutate(uc_task = str_replace_all(uc_task, "G", "green"),
           custom_response = str_replace_all(custom_response, "-G-", "green")) %>%
    mutate(uc_task = str_replace_all(uc_task, "B", "blue"),
           custom_response = str_replace_all(custom_response, "-B-", "blue"));
  df <- df %>% mutate(group = "group1", 
                      uc_task = as.factor(uc_task),
                      custom_response = as.factor(custom_response));
  return(df)
}

# @arg df: data frame containing columns bg, b, g, none
add_probs <- function(df){
  df <- df %>% mutate(p_a=bg+b, p_c=bg+g, p_na=g+none, p_nc=b+none) %>%
    mutate(p_c_given_a = bg / p_a,
           p_c_given_na = g / p_na,
           p_a_given_c = bg / p_c, 
           p_a_given_nc = b / p_nc, 
           p_nc_given_a = b/p_a,
           p_nc_given_na = none/p_na,
           p_na_given_c = g/p_c,
           p_na_given_nc = none/p_nc,
           p_likely_a = p_a,
           p_likely_na=p_na,
           p_likely_c = p_c,
           p_likely_nc=p_nc
    )
  return(df)
}

# @arg df1 in long-format
# smooth slider ratings from prior elicitation experiment (exp1)
add_smoothed_exp1 <- function(df1){
  df = df1 %>% group_by(prolific_id, id) %>% filter(sum(response) != 0)
  # normalize st. slider responses sum up to 1 but also keep original response
  df.with_smoothed = df %>%
    mutate(n = sum(response + epsilon), 
           pe_task.smooth = (response + epsilon) / n) %>%
    dplyr::select(-n)
  return(df.with_smoothed)
}

save_prob_tables <- function(df.utt_probs, result_dir){
  # Save all tables (with smoothed values)
  tables_all <- df.utt_probs %>% dplyr::select(id, prob, prolific_id, response) %>%
    pivot_wider(names_from = prob, values_from = response)
  path_tables_all <- paste(result_dir, "pe_tables_smooth.csv", sep = FS);
  write.table(tables_all, file = path_tables_all, sep = ",", row.names = F)
  message(paste('written smoothed probability tables to:', path_tables_all))
  
  # tables from PE-task rounded to two digits: each receives an unique id
  # (empirical_id); save how many participants chose respective table
  tables_empiric_pids = tables_all %>%
    dplyr::select("bg", "b", "g", "none", "prolific_id", "id") %>% 
    unite("p_id", c(prolific_id, id)) %>% group_by(bg, b, g, none) %>%
    summarize(p_id = list(p_id), .groups = "keep") %>% ungroup() %>% 
    mutate(bg.round = as.integer(round(bg, 2) * 100), 
           b.round = as.integer(round(b, 2) * 100),
           g.round = as.integer(round(g, 2) * 100), 
           none.round = as.integer(round(none, 2) * 100)) %>%
    rowid_to_column("empirical_id")
  save_data(tables_empiric_pids, 
            paste(result_dir, "tables-empiric-pids.rds", sep = FS))
}

process_data <- function(path_to_raw_csv, N_trials){
  raw_data = read_csv(path_to_raw_csv)
  list_data <- retrieve_data(raw_data, N_trials)
  
  # prepare test data
  pe_data <- list_data$test.pe %>% standardize_color_groups_exp1()
  pe.utt_probs.smooth <- add_utt_probs_to_pe_task_data(pe_data, smoothed = T)
  pe.utt_probs.orig <- add_utt_probs_to_pe_task_data(pe_data, smoothed = F)

  uc_data <- list_data$test.uc %>% standardize_color_groups_exp2() %>% 
    standardize_sentences()
  
  list_data$test.pe_task = pe_data
  list_data$test.pe_utt_probs_smooth = pe.utt_probs.smooth
  list_data$test.pe_utt_probs_orig = pe.utt_probs.orig
  list_data$test.uc_task = uc_data
  
  return(list_data)
}

bin_tables = function(tables){
  tbls= tables %>% rowid_to_column() %>% group_by(rowid) %>% 
    pivot_longer(cols=ends_with(".round"),
                 names_to="cell", values_to="p") %>%
    mutate(mod = p %% 10) 
  tables.binned = tbls %>%
    mutate(match_to = case_when(p %in% c(0,1,2) ~ 0,
                                p %in% c(98, 99, 100) ~ 100, 
                                mod %in% c(3,4,5,6,7) ~ p - mod + 5,
                                mod %in% c(8,9) ~ p - mod + 10,
                                mod %in% c(0,1,2) ~ p - mod)) %>%
    mutate(cell=str_replace(cell, ".round", ".binned")) %>% 
    dplyr::select(-p, -mod) %>% 
    pivot_wider(names_from="cell", values_from="match_to") %>% 
    ungroup() %>% dplyr::select(-rowid)
  # add rounded values again
  tables.binned = left_join(tables.binned, tables)
  return(tables.binned)
}

# matches empirical_ids with table_ids from sampled tables and saves mapping
# and brings result into format for webppl model
# @arg tables.generated: tables uniquely grouped by table_id
# (before combining with causal nets)
match_sampled_and_empiric_tables = function(tables.generated, dir_empiric){
  tables.empiric.pids = readRDS(paste(dir_empiric, "tables-empiric-pids.rds", sep=FS)) %>%
    rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none,
           AC.round=`bg.round`, `A-C.round`=`b.round`,
           `-AC.round`=`g.round`, `-A-C.round`=`none.round`) %>% 
    bin_tables()
  tables.emp = tables.empiric.pids %>% unnest(c(p_id)) %>%
    mutate(p_id.copy=p_id) %>%
    separate(p_id.copy, into=c("pid", "rel", "prior"), sep="_") %>%
    unite("stimulus", "rel", "prior", sep="_") %>%
    dplyr::select(ends_with(".binned"), empirical_id, stimulus, p_id) %>%
    group_by(`AC.binned`, `A-C.binned`, `-AC.binned`, `-A-C.binned`)
  
  tables.emp$bin_id = tables.emp %>% group_indices() 
  tables.emp.binned = tables.emp %>%
    mutate(empirical_id=list(empirical_id), stimulus=list(stimulus),
           p_id = list(p_id)) %>% distinct()
  tables.gen.binned = tables.generated %>% bin_tables()
  
  tbls.joint = left_join(
    tables.gen.binned, tables.emp.binned,
    by=c("AC.binned", "A-C.binned", "-AC.binned", "-A-C.binned")
  ) %>% group_by(table_id) %>% 
    mutate(match.empirical=!is.null(empirical_id[[1]])) %>% 
  dplyr::select(-ends_with(".round"), -ends_with("binned"), -bin_id)
  
  return(tbls.joint)
}

# adds empirical tables to set of all generated tables (tables.gen)
add_empirical_tables = function(tables.gen, dir_empiric, save_mapping_to){
  max.table_id = tables.gen$table_id %>% max()
  tbls.emp = readRDS(paste(dir_empiric, "tables-empiric-pids.rds", sep=FS)) %>%
    rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none) %>%
    dplyr::select(-ends_with(".round")) %>% 
    rowid_to_column("table_id") %>% group_by(table_id) %>% 
    mutate(table_id = table_id + max.table_id, added=TRUE) %>%
    group_by(table_id) %>% 
    unnest_longer(c(p_id)) %>% 
    separate("p_id", into=c("prolific_id", "rel", "prior"), sep="_") %>% 
    unite("id", "rel", "prior")
   
  tables.map = tbls.emp %>% ungroup() %>% 
    dplyr::select(prolific_id, id, table_id, empirical_id,
                  AC, `A-C`, `-AC`, `-A-C`) %>%
    rename(stimulus=id)
  if(save_mapping_to != "") save_data(tables.map, save_mapping_to)
  
  tables.empiric = tbls.emp %>% mutate(stimulus=id) %>%
    unite("p_id", "prolific_id", "id", sep="_") %>% 
    mutate(empirical_id=list(empirical_id), p_id=list(p_id), stimulus=list(stimulus)) %>% 
    distinct()
  
  tbls.joint = bind_rows(tables.gen %>% add_column(added=FALSE), tables.empiric) 
  return(tbls.joint)
}

# saves tables to folder @arg dir_empiric
makeAbstractPriorTables = function(dir_empiric) { 
  dat.model = sampleModelTables()
  tables.model = dat.model$tables
  tables.par = dat.model$params
  
  tables.generated = tables.model %>% unnest(c(vs, ps)) %>%
    pivot_wider(names_from="vs", values_from="ps") %>%
    mutate(AC.round=as.integer(round(AC, 2) * 100),
           `A-C.round`=as.integer(round(`A-C`,2) * 100),
           `-AC.round`=as.integer(round(`-AC`, 2) * 100),
           `-A-C.round`=as.integer(round(`-A-C`, 2) * 100))
  tables.generated = tables.generated %>% group_by(table_id)
  # for each sampled table add which empirical tables match
  tables.model = match_sampled_and_empiric_tables(tables.generated, dir_empiric)
  # add all distinct exact empirical tables to generated set of tables
  mapping_fn = tables.par$target_mapping
  tables.with_empirical = add_empirical_tables(tables.model, dir_empiric, 
                                               mapping_fn)
  # combine each generated/empirical table with each causal net
  # filter out those with -Infinity log likelihood
  tables <- tables.with_empirical %>% likelihood(tables.par$indep_sigma) %>%
    rename(cn.orig=cn)
  bns = tables_to_bns(tables, tables.par) %>%
    mutate(bn_id=case_when(
      cn=="A || C" ~ paste(table_id, "independent", sep="_"),
      TRUE ~ paste(table_id, str_replace_all(cn, " ", ""), sep="_")
    )) %>% group_by(bn_id) %>%
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
           ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
    dplyr::select(-`AC`, -`A-C`, -`-AC`, -`-A-C`) %>%
    add_column(indep_sigma=tables.par$indep_sigma)
  
  bns.finite_ll = bns  %>% filter(!is.infinite(ll))
  
  save_data(bns.finite_ll %>% filter(!added),
            paste(tables.par$target_dir, tables.par$tables_path, sep=FS))
  save_data(tables.par,
            paste(tables.par$target_dir, tables.par$target_params, sep=FS))
  
  save_to = paste(str_split(tables.par$tables_path, "\\.")[[1]][[1]], 
                  "with-empirical.rds", sep="-")
  save_data(bns.finite_ll, paste(tables.par$target_dir, save_to, sep=FS))
  
  # unique tables marginal P(tables) computed across cns with logsumexp
  tables = group_map(bns %>% group_by(table_id), function(table, table_id){
    table_id=table_id$table_id
    tbls.dep = table %>% filter(!endsWith(bn_id, "independent"))
    p_dep = (1/8) * exp(logSumExp(tbls.dep$ll))
    p_ind = (1/2) *
      exp(table %>% filter(endsWith(bn_id, "independent")) %>% pull(ll))
    ll = log(p_dep + p_ind)
    # ll=logSumExp(table$ll)
    df = table %>% add_column(table_id=(!! table_id))
    return(df[1,] %>% mutate(ll=(!!ll)))
  }) %>% bind_rows() %>% group_by(table_id) %>% mutate(cn="", best.cn=TRUE)
  save_data(tables, tables.par$target_uniq_tables)
  return(bns)
}


join_model_behavioral_averages = function(tables_fn, dir_empiric,
                                          chunked = FALSE, fn_data="-matched"){
  target_dir = here("model", "results", tables_fn)
  fn.model = paste("model-avg-predictions", fn_data, sep="")
  fn.model = ifelse(chunked, paste(fn.model, "-chunked.csv", sep=""),
                    paste(fn.model, ".csv", sep=""))
  message(paste('read model results from ', target_dir, FS, fn.model, sep=""))
  model.avg = read_csv(paste(target_dir, fn.model, sep=FS)) %>% rename(model=p)
  
  fn.behav = ifelse(chunked, "behavioral-avg-task2-chunked.csv",
                    "behavioral-avg-task2.csv")
  message(paste('read behavioral results from ', dir_empiric, FS, fn.behav, sep=""))
  behav.avg = read_csv(paste(dir_empiric, fn.behav, sep=FS)) %>%
    rename(stimulus = id, behavioral=ratio)
  
  data.joint = left_join(
    behav.avg %>% dplyr::select(stimulus, utt.standardized, behavioral),
    model.avg %>% dplyr::select(-best.utt) %>% rename(utt.standardized = utterance), 
    by=c("stimulus", "utt.standardized")
  )
  return(data.joint)
}

rep_each <- function(x, times) {
  times <- rep(times, length.out = length(x))
  rep(x, times = times)
}

get_controlled_factors = function(df){
  data = df %>% mutate(stimulus = id) %>% 
    separate(stimulus, into=c("relation", "prior"), sep="_") %>%
    mutate(prior_green = case_when(relation == "if1" ~ "impossible",
                                   relation == "if2" ~ "low",
                                   endsWith(prior, "l") ~ "low",
                                   endsWith(prior, "h") ~ "high"),
           prior_blue = case_when(startsWith(prior, "h") ~ "high",
                                  startsWith(prior, "l") ~ "low",
                                  startsWith(prior, "u-L") ~ "uncl",
                                  startsWith(prior, "u") ~ "unc"),
           relation_type = case_when(relation == "independent" ~ relation,
                                     T ~ "dependent")) %>%
    dplyr::select(-prior) %>% 
    mutate(prior_green = as_factor(prior_green),
           prior_blue = as_factor(prior_blue),
           relation = as_factor(relation),
           relation_type = as_factor(relation_type)
    )
  return(data)
}

# Quality of the data -----------------------------------------------------
# for each participant take mean response of all other participants for each
# stimulus and question, then compute squared difference between participant's
# response and mean of all others -> 4 values, one for each question (for each participant)
# to get one value per participant sum these up (for each participant)
distancesResponses = function(df.prior, save_as = NA){
  df = df.prior %>% 
    dplyr::select(prolific_id, id, response, slider) %>%
    unite(col = "id_quest", "id", "slider", sep="__", remove=FALSE)
  
  distances <- tibble()
  for(proband in df.prior$prolific_id %>% unique()) {
    message(proband)
    res = df %>% filter(prolific_id == proband) %>% ungroup()
    for(stimulus in df$id %>% unique()) {
      dat <- df %>% filter(id == (!! stimulus));
      res_proband = res %>%
        filter(str_detect(id_quest, paste(stimulus, ".*", sep=""))) %>%
        dplyr::select(id_quest, response) %>% rename(r_proband = response) %>%
        add_column(comparator = proband)
      dat.others = anti_join(dat, res_proband %>% rename(prolific_id=comparator),
                             by=c("prolific_id", "id_quest"))
      
      means.others = dat.others %>% group_by(id_quest) %>% 
        summarize(mean.others=mean(response), .groups="drop_last")
      
      diffs = left_join(means.others, res_proband, by=c("id_quest")) %>%
        mutate(sq_diff = (r_proband - mean.others)**2)
      distances = bind_rows(distances, diffs)
    }
  }
  dist.sums <- distances %>%
    separate("id_quest", into=c("id", "slider"), sep="__") %>%
    group_by(id, comparator) %>%
    summarize(sum_sq_diffs = sum(sq_diff), .groups="drop_last") %>% 
    mutate(mean.id=mean(sum_sq_diffs), sd.id=sd(sum_sq_diffs)) %>%
    group_by(comparator) %>% 
    mutate(mean.comparator=mean(sum_sq_diffs)) %>%
    ungroup()
  
  if(!is.na(save_as)){
    message(paste('save data to:', save_as))
    saveRDS(dist.sums, save_as)
  }
  return(dist.sums)
}


add_utt_probs_to_pe_task_data = function(test.prior, smoothed=TRUE){
  df.pe_responses = test.prior %>% dplyr::select(-QUD, -trial_name)
  if(smoothed){
    df.pe_responses = df.pe_responses %>% dplyr::select(-pe_task) %>% 
      pivot_wider(names_from = "slider", values_from = "pe_task.smooth")
  } else {
    df.pe_responses = df.pe_responses %>% dplyr::select(-pe_task.smooth) %>%
      pivot_wider(names_from = "slider", values_from = "pe_task")
  }
  df.probabilities = df.pe_responses %>% add_probs() %>% 
    pivot_longer(cols = c("b", "g", "bg", "none", starts_with("p_")),
                 names_to = "prob", values_to = "response") %>%
    translate_probs_to_utts() %>% dplyr::select(-group) %>%
    mutate(slider = case_when(!prob %in% c("bg", "b", "g", "none") ~ NA_character_,
                              TRUE ~ prob))
  return(df.probabilities)
}

join_pe_uc_data = function(pe_data, uc_data){
  df.uc_data = uc_data %>% 
    dplyr::select(prolific_id, id, uc_task, custom_response, 
                  utt.standardized, RT, cost.uc, trial_number) %>%
    add_column(human_exp2 = 1) # mark rows with selected utterance in uc task
  
  joint_data = left_join(
    pe_data %>% rename(RT.pe_task = RT, trial_nb_pe = trial_number, pe_task = response),
    df.uc_data %>% rename(RT.uc_task = RT, trial_nb_uc = trial_number)
  )
  return(joint_data)
}

# compute_utt_probs = function(observations){
#   observations %>%
#     mutate(p_utt_obs = case_when(utt_observed %in% c("A", "might A") ~ AC + `A-C`, 
#                                  utt_observed %in% c("-A", "might -A") ~ `-AC` + `-A-C`,
#                                  utt_observed %in% c("C", "might C") ~ `AC` + `-AC`, 
#                                  utt_observed %in% c("-C", "might -C") ~ `A-C` + `-A-C`,
#                                  utt_observed == "C and A" ~  `AC`,
#                                  utt_observed == "C and -A" ~ `-AC`,
#                                  utt_observed == "-C and A" ~ `A-C`,
#                                  utt_observed == "-C and -A" ~ `-A-C`,
#                                  
#                                  utt_observed == "A > C" ~  AC / (AC + `A-C`),
#                                  utt_observed == "C > A" ~  `AC` / (`AC` + `-AC`),
#                                  utt_observed == "A > -C" ~ `A-C` / (AC + `A-C`),
#                                  utt_observed == "-C > A" ~ `A-C` / (`A-C` + `-A-C`),
#                                  
#                                  utt_observed == "-A > C" ~ `-AC` / (`-AC` + `-A-C`),
#                                  utt_observed == "C > -A" ~  `-AC` / (`AC` + `-AC`),
#                                  utt_observed == "-A > -C" ~ `-A-C` / (`-AC` + `-A-C`),
#                                  utt_observed == "-C > -A" ~ `-A-C` / (`A-C` + `-A-C`)
#     ))
# }


