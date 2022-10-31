library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(reticulate)
library(scales)
library(truncnorm)
library(greta)
source(here("R", "utils.R"))
source(here("R", "utils-exp2.R"))
source(here("R", "Dirichlet-fits.R"))

FS = .Platform$file.sep
# General Data ------------------------------------------------------------
IDS.dep=c("if1_uh", "if1_u-Lh", "if1_hh", "if1_lh",
          "if2_ul", "if2_u-Ll", "if2_hl", "if2_ll");
IDS.ind = c("independent_ll", "independent_hh", "independent_hl",
            "independent_ul", "independent_uh"); #, "ind2");

questions.test = c("bg", "b", "g", "none")
labels.test = c(bg = "Blue and Green", b = "Blue and ¬Green",
                g =" ¬Blue and Green", none = "¬Blue and ¬Green")

# Other -------------------------------------------------------------------
# ordered by informativity
levels.responses = rev(c(
  standardized.sentences$bg, standardized.sentences$none,
  standardized.sentences$g, standardized.sentences$b,
  standardized.sentences$only_b, standardized.sentences$only_g,
  standardized.sentences$only_nb, standardized.sentences$only_ng,
  standardized.sentences$if_bg, standardized.sentences$if_gb, 
  standardized.sentences$if_nbng, standardized.sentences$if_ngnb,
  standardized.sentences$if_bng, standardized.sentences$if_gnb,
  standardized.sentences$if_nbg, standardized.sentences$if_ngb,
  standardized.sentences$might_b, standardized.sentences$might_g,
  standardized.sentences$might_nb, standardized.sentences$might_ng
))

standardized.conj = c(standardized.sentences$bg, standardized.sentences$b,
                      standardized.sentences$g, standardized.sentences$none)
standardized.lit = c(standardized.sentences$only_b, standardized.sentences$only_g,
                     standardized.sentences$only_nb, standardized.sentences$only_ng)
standardized.ifs = levels.responses[str_detect(levels.responses, "if")]

# Experimental Data -------------------------------------------------------
load_formatted_data = function(result_dir){
  data = list()
  message(paste("Data loaded from", result_dir))
  data$plot_dir = paste(result_dir, "plots", sep = FS)
  if(!dir.exists(data$plot_dir)){dir.create(data$plot_dir, recursive=TRUE)}
  data$result_dir = result_dir
  
  # Experimental Data
  data$production = readRDS(paste(result_dir, "human-exp2.rds", sep=FS));
  data$prior.smooth = readRDS(paste(result_dir, "human-exp1-smoothed.rds", sep=FS))
  data$prior.orig = readRDS(paste(result_dir, "human-exp1-orig.rds", sep=FS))
  data$joint.smooth = readRDS(paste(result_dir, "human-exp1-smoothed-exp2.rds", sep=FS))
  data$joint.orig = readRDS(paste(result_dir, "human-exp1-orig-exp2.rds", sep=FS))
  data$info = readRDS(paste(result_dir, "participants-info.rds", sep=FS))
  return(data)
}

# Filter Data ---------------------------------------------------------------
exclude_test_data_and_save = function(result_dir, target_dir, df.ids){
  data = load_formatted_data(result_dir)
  anti_by = c("prolific_id", "id")
  
  exp2 = anti_join(data$production, df.ids, by=anti_by)
  save_data(exp2,paste(target_dir, "human-exp2.rds", sep=FS));
  
  exp1_smoothed = anti_join(data$prior.smooth, df.ids, by=anti_by)
  save_data(exp1_smoothed, paste(target_dir, "human-exp1-smoothed.rds", sep=FS))
  
  exp1_orig = anti_join(data$prior.orig, df.ids, by=anti_by)
  save_data(exp1_orig,paste(target_dir, "human-exp1-orig.rds", sep=FS))
  
  exp1_smoothed_exp2 = anti_join(data$joint.smooth, df.ids, by=anti_by)
  save_data(exp1_smoothed_exp2, paste(target_dir, "human-exp1-smoothed-exp2.rds", sep=FS))
  
  exp1_orig_exp2 = anti_join(data$joint.orig, df.ids, by=anti_by)
  save_data(exp1_orig_exp2, paste(target_dir, "human-exp1-orig-exp2.rds", sep=FS))
  
  out.all_trials = df.ids %>% group_by(prolific_id) %>% mutate(n = n()) %>%
    filter(n == 13) %>% dplyr::select(prolific_id) %>% distinct()
  df.info = anti_join(data$info, out.all_trials, by=c("prolific_id"))
  save_data(df.info, paste(target_dir, "participants-info.rds", sep = FS))

  return(list(exp2 = exp2, 
              exp1_sm = exp1_smoothed, 
              exp1_orig = exp1_orig,
              exp1_sm_exp2 = exp1_smoothed_exp2, 
              exp1_orig_exp2 = exp1_orig_exp2,
              info = df.info))
}


exclude_train_data_and_save = function(data_all, target_dir, df.out){
  anti_by = c("prolific_id")
  # only filter out data from participants from whom all data is excluded
  df.ids = df.out %>% group_by(prolific_id) %>% dplyr::count() %>% 
    filter(n == 13) %>% dplyr::select(prolific_id)
  df.slider_choice = anti_join(data_all$train.slider_choice, df.ids, by=anti_by)
  save_data(df.slider_choice, paste(target_dir, "train-slider-choice.rds", sep=FS));
  
  df.attention = anti_join(data_all$train.attention, df.ids, by=anti_by)
  save_data(df.attention, paste(target_dir, "train-attention.rds", sep=FS))
  
  df.smooth = anti_join(data_all$train.smooth, df.ids, by=anti_by)
  save_data(df.smooth, paste(target_dir, "train-probs-smooth.rds", sep=FS))
  
  df.orig = anti_join(data_all$train.orig, df.ids, by=anti_by)
  save_data(df.orig, paste(target_dir, "train-probs-orig.rds", sep=FS))
  
  return(list(slider_choice = df.slider_choice,
              attention = df.attention, 
              probs_smooth = df.smooth,
              probs_orig = df.orig))
}


# according to criteria filter out and save all cleaned data separately
# @arg out.by_comments: tibble with cols: 'prolific_id', 'id'
filter_data = function(data_dir, target_dir, out.by_comments=NA) {
  # these files are generated by process_data function
  data <- readRDS(here(data_dir, "tidy_data.rds"))
  data.production = readRDS(paste(data_dir, "human-exp2.rds", sep = FS));
  data.joint.orig = readRDS(paste(data_dir, "human-exp1-orig-exp2.rds", sep=FS))
  dat.quality = readRDS(paste(data_dir, "test-data-prior-quality.rds", sep=FS))
  
  data.train.sliders = data$train.slider_choice
  df.subj_trials = data.production %>% dplyr::select(prolific_id, id)
  # exclude all trials of a participant
  # 1. attention check questions in beginning concerning block icons
  df.att = data$train.attention %>% filter(response != expected) 
  df.out = tibble()
  if(nrow(df.att) != 0){
    participants.att = df.att$prolific_id %>% unique()
    df.att = df.subj_trials %>% filter(prolific_id %in% participants.att) %>%
      dplyr::select(prolific_id, id) %>% distinct()
    df.out = bind_rows(df.out, df.att)
    message(paste(length(participants.att),
                  'participants completely excluded due to attention checks'))
  }
  # 2. color vision questions
  dat.col = data$color %>% 
    filter(response != expected) %>%
    dplyr::select(prolific_id, id) %>%
    group_by(prolific_id) %>%
    mutate(n_wrong = n()) %>% filter(n_wrong >= 1)
  if(nrow(dat.col) != 0){
    participants.col = dat.col$prolific_id %>% unique()
    df.col = df.subj_trials %>% filter(prolific_id %in% participants.col) %>%
      dplyr::select(prolific_id, id) %>% distinct()
    df.out = bind_rows(df.out, df.col)
    message(paste(length(participants.col),
                  'participant(s) completely excluded due to >= 1 color questions wrong'))
  }
  # 3. slider-choice trials
  dat.sliders = data.train.sliders %>%
    mutate(correct = response == expected, .groups="drop_last") %>%
    group_by(prolific_id) %>%
    summarize(ratio_correct = sum(correct) / n(), .groups="drop_last") %>%
    filter(ratio_correct < 0.5)
  if(nrow(dat.sliders) != 0){
    participants.sc = dat.sliders$prolific_id %>% unique()
    df.sc = df.subj_trials %>% 
      filter(prolific_id %in% participants.sc) %>%
      dplyr::select(prolific_id, id) %>% 
      distinct()
    df.out = bind_rows(df.out, df.sc)
    m = 'participant(s) completely excluded due to less than 50% of slider-choice trials correct'
    message(paste(length(participants.sc), m))
  }
  # 4. single trials: utterance task 2 is rated with 0 in task 1
  df.utt = data.joint.orig %>%
    filter(!is.na(human_exp2) & human_exp1 == 0) %>%
    dplyr::select(prolific_id, id) %>%
    distinct()
  m = 'trial(s) excluded due to 0 probability in task 1, but chosen in task 2'
  message(paste(nrow(df.utt), m))
  df.out = bind_rows(df.out, df.utt)
  
  # 5. due to comments
  if(!is.na(out.by_comments)){
    out.comments = read_csv(out.by_comments) %>% dplyr::select(prolific_id, id)
    message(paste(length(out.comments$prolific_id %>% unique),
                  'participants excluded due to comments'))
    df.out = bind_rows(df.out, out.comments)
  }
  # 6. Quality
  quality.means = dat.quality %>% arrange(desc(mean.comparator)) %>%
    distinct_at(vars(c(comparator)), .keep_all = TRUE) %>% 
    dplyr::select(-id)
  out.quality = quality.means %>% filter(mean.comparator > 0.5) 
  # remove
  trials = dat.quality$id %>% unique()
  df.quality =  out.quality %>% dplyr::select(mean.comparator, comparator) %>%
    rename(prolific_id = comparator) %>% 
    add_column(id = rep(list(trials), nrow(out.quality))) %>%
    unnest(c(id)) %>% group_by(prolific_id) %>%
    filter(id != "ind2") %>% dplyr::select(-mean.comparator)
  df.out = bind_rows(df.out, df.quality) %>% distinct()
  
  # save data
  ratio_ex = round(nrow(df.out) / nrow(df.subj_trials), 3) * 100
  message(paste(ratio_ex, '% of all trials excluded in total.', sep=""))
  write_csv(df.out, paste(target_dir, "ids_excluded.csv", sep = FS))
  
  df_test.filtered = exclude_test_data_and_save(data_dir, target_dir, df.out)
  df_train.filtered = exclude_train_data_and_save(data, target_dir, df.out)
  
  # also save prob tables with empiric-ids
  df1 = data$test %>% filter(str_detect(trial_name, "multiple_slider"))
  df1 = anti_join(df1, df.out, by=c("prolific_id", "id")) %>%
    dplyr::select(-response) %>% 
    rename(response = r_orig) %>%
    add_smoothed_exp1() %>% 
    standardize_color_groups_exp1()
  save_prob_tables(df1, target_dir);
  
  # average productions
  df.avg.exp2 = task2_avg_per_stimulus(target_dir)
  write_csv(df.avg.exp2, 
            paste(target_dir, "behavioral-avg-task2.csv", sep = FS))
  # also with chunked utterances
  df2.chunked = df.avg.exp2 %>% chunk_utterances()
  df2.chunked.avg = df2.chunked %>%
    group_by(id, utterance) %>% 
    summarize(count=sum(count), ratio=sum(ratio), .groups="drop_last")
  write_csv(df2.chunked.avg, 
            paste(target_dir, "behavioral-avg-task2-chunked.csv", sep = FS))
  
  return(df_test.filtered)
}

process_and_save_data_for_model = function(cleaned_dir){
  df.exp1 = readRDS(paste(cleaned_dir, "human-exp1-orig.rds", sep = FS))
  # fit dirichlet distributions to filtered data
  N_participants = df.exp1 %>% 
    group_by(id) %>% 
    distinct_at(vars(c(prolific_id))) %>%
    summarize(n = n())
  # single distribution for all data
  params.fit.single = run_fit_dirichlet(cleaned_dir, per_stimulus = F) %>%
    add_column(p_cn = 1, cn = "all") %>% 
    mutate(id = "all")
  N_all = N_participants %>% summarize(n = sum(n)) %>% 
    add_column(cn = "all", id = "all")
  res.goodness = compute_goodness_dirichlets(params.fit.single, 
                                             cleaned_dir,
                                             N_all, each=F)
  res.goodness %>% distinct_at(vars(c(stimulus_id)), .keep_all = T)
  p = plot_goodness_dirichlets(res.goodness, params.fit.single, cleaned_dir)
  
  # single distribution for data from each stimulus
  df.params.fit = run_fit_dirichlet(cleaned_dir, per_stimulus=T) %>%
    add_column(p_cn = 1 / 13) %>% mutate(cn = id)
  # df.params.fit = read_csv(paste(cleaned_dir, "params-fitted-dirichlet.csv", 
  #                                sep=FS))  %>%
  #   add_column(p_cn = 1/13) %>% mutate(cn = id)
  tables.dirichlet = makeDirichletTables(df.params.fit, cleaned_dir)
  message("compute goodness of dirichlet fits ...")
  res.goodness = compute_goodness_dirichlets(df.params.fit, cleaned_dir,
                                             N_participants)
  res.goodness %>% distinct_at(vars(c(stimulus_id)), .keep_all = T)
  p = plot_goodness_dirichlets(res.goodness, df.params.fit, cleaned_dir)
  
  # generate abstract state prior tables with filtered data
  tables.model = makeAbstractPriorTables(dir_empiric = cleaned_dir)
}
