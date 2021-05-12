library(tidyverse)
library(here)
library(matrixStats)
source(here("model", "R", "helpers-tables.R"))

fs = .Platform$file.sep
epsilon = 0.000001
seed_fitted_tables = "20202020"

test_data <- function(path_to_csv) {
  data <- read_csv(path_to_csv) %>%
    mutate(prolific_id = str_trim(str_to_lower(prolific_id))) %>%
    filter(str_detect(prolific_id, "test-.*") | str_detect(prolific_id, "test "))
  return(data)
}

experimental_data <- function(path_to_csv){
  data <- read_csv(path_to_csv) %>%
    mutate(prolific_id = str_trim(str_to_lower(prolific_id))) %>%
    filter(!str_detect(prolific_id, "test.*") & prolific_id != "" &
             !is.na(prolific_id))
  return(data)
}

save_raw_data <- function(data_dir, data_fn, result_dir, result_fn, debug_run=F){
  path_to_data <- paste(data_dir, data_fn, sep=.Platform$file.sep)
  if(debug_run){
    data <- test_data(path_to_data)
  } else {
    data <- experimental_data(path_to_data)
  }
  
  path_target <- paste(result_dir, paste(result_fn, "raw.csv", sep="_"), sep=fs)
  write_excel_csv(data, path = path_target, delim = ",", append=F, col_names=T)
  print(paste('written raw data to:', path_target))
  return(data)
}

tidy_test_exp1 <- function(df){
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
                 values_to = "question") %>%
    filter(response_idx == question_idx) %>%
    dplyr::select(-response_idx, -question_idx)
  
  dat.test <- dat.test %>%
    mutate(response = as.numeric(response),
           response = response/100,
           prolific_id = factor(prolific_id),
           group=case_when(id=="ind2" ~ "group1",
                           TRUE ~ group),
           id = factor(id)
    )
  return(dat.test)
}

tidy_test_exp2 <- function(df){
  dat.test <- df %>%
    filter(startsWith(trial_name, "fridge_view") |
             trial_name == "fridge_train") %>%
    dplyr::select(prolific_id, RT, QUD, id, group, response1, response2,
                  trial_name, trial_number) %>%
    rename(custom_response=response2, response=response1)
  
  dat.test <- dat.test %>%
    mutate(prolific_id = factor(prolific_id),
           id = factor(id))
  return(dat.test)
}

tidy_test_joint <- function(df){
  data.prior = df %>% filter(str_detect(trial_name, "multiple_slider")) %>%
    tidy_test_exp1() %>%
    add_column(custom_response="", utterance="")
  data.production = df %>% filter(str_detect(trial_name, "fridge_")) %>%
    tidy_test_exp2() %>%
    rename(utterance=response) %>%
    add_column(question="")
  dat.test = bind_rows(data.prior, data.production)
  return(dat.test)
}

tidy_train <- function(df){
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
  
  dat.train.smooth = dat.train %>% rename(response=r_smooth) %>%
    dplyr::select(-r_orig, -n, -trial_name)
  dat.train.orig = dat.train %>% rename(response=r_orig) %>%
    dplyr::select(-r_smooth, -n, -trial_name)
  return(list(smooth=dat.train.smooth, orig=dat.train.orig))
}

tidy_data <- function(data, N_trials){
  # 1. dplyr::select only columns relevant for data analysis
  df <- data %>%
    dplyr::select(prolific_id, submission_id,
                  question, question1, question2, question3, question4,
                  QUD, response,
                  expected, response1, response2, response3, response4,
                  id, trial_name, trial_number, group,
                  timeSpent, RT,
                  education, comments, gender, age)
  # always use the same abbreviation
  df <- df %>% mutate(question1 = case_when(question1 == "gb" ~ "bg",
                                            question1 == "yr" ~ "ry",
                                            TRUE ~ question1),
                      response3 = as.character(response3),
                      response4 = as.character(response4));
  dat.color_vision <- tibble();
  if(N_trials$color_vision != 0) {
    dat.color_vision <- df %>%
      filter(startsWith(trial_name, "color-vision")) %>%
      dplyr::select(prolific_id, id, question, response, expected, QUD, trial_number)
    df <- df %>% filter(!startsWith(trial_name, "color-vision"));
  }
  dat.slider_choice=tibble()
  dat.attention_check=tibble()
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
    unique()
  dat.train <- tidy_train(df)
  dat.test <- tidy_test_joint(df)
  dat.all <- list(test=dat.test, train.smooth=dat.train$smooth,
                  train.orig=dat.train$orig, color=dat.color_vision,
                  train.attention=dat.attention_check,
                  train.slider_choice=dat.slider_choice,
                  info=dat.info, comments=dat.comments)
  
  return(dat.all)
}

standardize_color_groups_exp1 <- function(df){
  # ind2 is used as single training example for production task (always group1!)
  df <- df %>%
    mutate(question =
             case_when((question == "bg" | question == "gb" |
                          question=="ry" | question == "yr") ~ "ac",
                       question == "none" ~ "none",
                       group=="group1" & (question=="b" | question=="r") ~ "a",
                       group=="group1" & (question=="g" | question=="y") ~ "c",
                       group=="group2" & question=="g"  ~ "a",
                       group=="group2" & question=="b" ~ "c"
             ),
           group = "group1",
           question = case_when(question == "a" ~ "b",
                                question == "c" ~ "g",
                                question == "ac" ~ "bg",
                                question == "none" ~ "none")
    )
  return(df)
}

standardize_color_groups_exp2 <- function(df){
  df <- df %>%
    mutate(response=
            case_when(group=="group2" ~ str_replace_all(response, "blue", "G"),
                       T ~ str_replace_all(response, "blue", "B")),
           custom_response=
             case_when(group=="group2" ~ str_replace_all(custom_response, "blue", "-G-"),
                       T ~ str_replace_all(custom_response, "blue", "-B-"))) %>%
    
    mutate(response=case_when(group=="group2" ~ str_replace_all(response, "green", "B"),
                              T ~ str_replace_all(response, "green", "G")),
           custom_response=
             case_when(group=="group2" ~ str_replace_all(custom_response, "green", "-B-"),
                       T ~ str_replace_all(custom_response, "green", "-G-"))) %>%
    mutate(response=str_replace_all(response, "G", "green"),
           custom_response=str_replace_all(custom_response, "-G-", "green")) %>%
    mutate(response=str_replace_all(response, "B", "blue"),
           custom_response=str_replace_all(custom_response, "-B-", "blue"));
  df <- df %>%
    mutate(group="group1", response = as.factor(response),
           custom_response=as.factor(custom_response));
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

# @arg quest: question which is used to generate the clusters, e.g. 'b'
cluster_responses <- function(dat, quest){
  dat.kmeans <- dat %>% filter(question == quest) %>%
    dplyr::select(prolific_id, id, response) %>% add_column(y=1) %>%
    group_by(prolific_id, id) %>%
    unite("rowid", "prolific_id", "id", sep="--") %>%
    column_to_rownames(var = "rowid")
  clusters <- kmeans(dat.kmeans, 2)
  
  df <- dat.kmeans %>%
    rownames_to_column(var = "rowid") %>%
    as_tibble() %>%
    separate(col="rowid", sep="--", into=c("prolific_id", "id")) %>%
    mutate(cluster=as.factor(clusters$cluster), id=as.factor(id),
           prolific_id = as.factor(prolific_id)) %>%
    dplyr::select(prolific_id, id, cluster)
  df <- left_join(dat, df) 
  df <- df %>%
    mutate(cluster=fct_explicit_na(df$cluster, na_level='not-clustered'))
  return(df)
}

# @arg df1 in long-format
# smooth slider ratings from prior elicitation experiment (exp1)
add_smoothed_exp1 <- function(df1){
  df = df1 %>% group_by(prolific_id, id) %>%
    filter(sum(response) != 0)
  # normalize st. slider responses sum up to 1 but also keep original response
  df.with_smoothed = df %>%
    mutate(n=sum(response + epsilon), r_smooth=(response + epsilon)/n) %>%
    rename(r_orig=response)
  return(df.with_smoothed)
}

save_prob_tables <- function(df, result_dir, result_fn){
  # Save all Tables (with smoothed values)
  tables.all <- df %>% dplyr::select(id, question, prolific_id, r_smooth) %>%
    group_by(id, prolific_id) %>%
    pivot_wider(names_from = question, values_from = r_smooth) %>%
    add_probs()
  fn_tables_all <- paste(result_fn, "_tables_smooth.csv", sep="");
  path_tables_all <- paste(result_dir, fn_tables_all, sep=fs);
  write.table(tables.all, file=path_tables_all, sep = ",", row.names=FALSE)
  print(paste('written smoothed probability tables to:', path_tables_all))
  
  tables.empiric.pids = tables.all %>%
    dplyr::select("bg", "b", "g", "none", "prolific_id", "id") %>% 
    unite("p_id", c(prolific_id, id)) %>% group_by(bg, b, g, none) %>%
    summarize(p_id=list(p_id), .groups="keep") %>% ungroup() %>% 
    mutate(bg.round=as.integer(round(bg,2)*100), b.round=as.integer(round(b,2)*100),
           g.round=as.integer(round(g,2)*100), none.round=as.integer(round(none,2)*100)) %>%
    rowid_to_column("empirical_id")
  save_data(tables.empiric.pids, paste(result_dir, "tables-empiric-pids.rds", sep=fs))
}

process_data <- function(data_dir, data_fn, result_dir, result_fn, debug_run, N_trials){
  dat.anonym <- save_raw_data(data_dir, data_fn, result_dir, result_fn, debug_run)
  dat.tidy <- tidy_data(dat.anonym, N_trials);
  # Further process TEST-trial data --------------------------------------------
  data <- dat.tidy$test
  df1 <- data %>% filter(str_detect(trial_name, "multiple_slider"))
  df1 <- add_smoothed_exp1(df1);
  df1 <- standardize_color_groups_exp1(df1)
  save_prob_tables(df1, result_dir, result_fn);
  df2 <- data %>% filter(str_detect(trial_name, "fridge_")) %>%
    mutate(response=utterance) %>%
    dplyr::select(-utterance)
  df2 <- standardize_color_groups_exp2(df2)
  df2 <- standardize_sentences(df2)
  df <- bind_rows(df1 %>% rename(response=utterance), df2);
  
  # save processed data -----------------------------------------------------
  fn_tidy <- paste(result_fn, "_tidy.rds", sep="");
  path_target <- paste(result_dir, fn_tidy, sep=.Platform$file.sep)
  dat.tidy$test <- df
  save_data(dat.tidy, path_target)
  return(dat.tidy)
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
  tables.empiric.pids = readRDS(paste(dir_empiric, "tables-empiric-pids.rds", sep=fs)) %>%
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
  tbls.emp = readRDS(paste(dir_empiric, "tables-empiric-pids.rds", sep=fs)) %>%
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
            paste(tables.par$target_dir, tables.par$tables_path, sep=fs))
  save_data(tables.par,
            paste(tables.par$target_dir, tables.par$target_params, sep=fs))
  
  save_to = paste(str_split(tables.par$tables_path, "\\.")[[1]][[1]], 
                  "with-empirical.rds", sep="-")
  save_data(bns.finite_ll, paste(tables.par$target_dir, save_to, sep=fs))
  
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
  message(paste('read model results from ', target_dir, fs, fn.model, sep=""))
  model.avg = read_csv(paste(target_dir, fn.model, sep=fs)) %>%
    filter(stimulus != "ind2") %>% rename(model=p)
  
  fn.behav = ifelse(chunked, "behavioral-avg-task2-chunked.csv",
                    "behavioral-avg-task2.csv")
  message(paste('read behavioral results from ', dir_empiric, fs, fn.behav, sep=""))
  behav.avg = read_csv(paste(dir_empiric, fn.behav, sep=fs)) %>%
    rename(stimulus = id, behavioral=ratio) %>% filter(stimulus != "ind2")
  
  data.joint = left_join(
    behav.avg %>% dplyr::select(stimulus, utterance, behavioral),
    model.avg %>% dplyr::select(-best.utt), 
    by=c("stimulus", "utterance")
  )
  return(data.joint)
}

rep_each <- function(x, times) {
  times <- rep(times, length.out = length(x))
  rep(x, times = times)
}

ll_table_across_cns = function(bns){
  tables = group_map(bns %>% group_by(table_id), function(table, table_id){
    table_id=table_id$table_id
    ll=logSumExp(table$ll)
    df = table %>% add_column(table_id=(!! table_id))
    return(df[1,] %>% mutate(ll=(!!ll)))
  }) %>% bind_rows() %>% group_by(table_id) %>% mutate(cn="", best.cn=TRUE)
  return(tables)
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



