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
standardized.conj = c(standardized.sentences$bg, standardized.sentences$b,
                      standardized.sentences$g, standardized.sentences$none)
standardized.lit = c(standardized.sentences$only_b, standardized.sentences$only_g,
                     standardized.sentences$only_nb, standardized.sentences$only_ng)
standardized.ifs = c(standardized.sentences$if_bg, standardized.sentences$if_gb, 
                     standardized.sentences$if_nbng, standardized.sentences$if_ngnb,
                     standardized.sentences$if_bng, standardized.sentences$if_gnb,
                     standardized.sentences$if_nbg, standardized.sentences$if_ngb)
standardized.might = c(standardized.sentences$might_b, standardized.sentences$might_g,
                      standardized.sentences$might_nb, standardized.sentences$might_ng)


save_train_data = function(train_data, target_dir){
  write_csv(train_data$train.slider_choice,
            paste(target_dir, "train-slider-choice.csv", sep = FS))
  write_csv(train_data$train.attention,
            paste(target_dir, "train-attention.csv", sep = FS))

  fn = paste(target_dir, "train-slider-ratings.csv", sep = FS)
  write_csv(train_data$train.pe, fn)
}

clean_train_data = function(train_data, df.out) {
  anti_by = c("prolific_id")
  # only filter out data from participants from whom all data is excluded
  df.ids = df.out %>% group_by(prolific_id) %>% dplyr::count() %>%
    filter(n == 13) %>% dplyr::select(prolific_id)
  
  df.slider_choice = anti_join(train_data$train.slider_choice, df.ids, 
                               by="prolific_id") %>% 
    group_by(prolific_id)
  df.attention = anti_join(train_data$train.attention, df.ids, by="prolific_id") %>% 
    group_by(prolific_id)
  df.pe = anti_join(train_data$train.pe, df.ids, by = "prolific_id")
  
  return(list(train.slider_choice = df.slider_choice,
              train.pe = df.pe, 
              train.attention = df.attention))
}


# according to criteria filter out and save all cleaned data separately
# @arg df.out_comments: tibble with cols: 'prolific_id', 'id'
get_ids_to_exclude = function(test_data, train_data, color_trial_data, 
                              quality_pe_data, target_dir, df.out_comments) {

  df.subj_trials = test_data %>% dplyr::select(prolific_id, id) %>% distinct()
  # exclude all trials of a participant
  # 1. attention check questions in beginning concerning block icons
  df.att = train_data$train.attention %>% filter(response != expected) 
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
  dat.col = color_trial_data %>% filter(response != expected) %>%
    dplyr::select(prolific_id, id) %>% group_by(prolific_id) %>%
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
  dat.sliders = train_data$train.slider_choice %>%
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
  df.utt = test_data %>% filter(!is.na(human_exp2) & pe_task == 0) %>%
    dplyr::select(prolific_id, id) %>% distinct()
  m = 'trial(s) excluded due to 0 probability in task 1, but chosen in task 2'
  message(paste(nrow(df.utt), m))
  df.out = bind_rows(df.out, df.utt)
  
  # 5. due to comments
  message(paste(length(out.comments$prolific_id %>% unique()),
                'participants excluded due to comments'))
  df.out = bind_rows(df.out, df.out_comments %>% distinct())

  # 6. Quality
  quality.means = quality_pe_data %>% arrange(desc(mean.comparator)) %>%
    distinct_at(vars(c(comparator)), .keep_all = TRUE) %>% 
    dplyr::select(-id)
  out.quality = quality.means %>% filter(mean.comparator > 0.5) 
  # remove
  trials = quality_pe_data$id %>% unique()
  df.quality =  out.quality %>% dplyr::select(mean.comparator, comparator) %>%
    rename(prolific_id = comparator) %>% 
    add_column(id = rep(list(trials), nrow(out.quality))) %>%
    unnest(c(id)) %>% group_by(prolific_id) %>%
    dplyr::select(-mean.comparator)
  df.out = bind_rows(df.out, df.quality) %>% distinct()
  
  # save data
  ratio_ex = round(nrow(df.out) / nrow(df.subj_trials), 3) * 100
  message(paste(ratio_ex, '% of all trials excluded in total.', sep=""))
  write_csv(df.out, paste(target_dir, "ids_excluded.csv", sep = FS))
  
  return(df.out)
}

process_and_save_data_for_model = function(df.pe_task, cleaned_dir){
  # fit dirichlet distributions to cleaned data
  N_participants = df.pe_task %>% 
    group_by(id) %>% 
    distinct_at(vars(c(prolific_id))) %>%
    summarize(n = n())
  # single distribution for all data
  params.fit.single = run_fit_dirichlet(cleaned_dir, per_stimulus = F) %>%
    add_column(p_cn = 1, cn = "all") %>% mutate(id = "all")
  N_all = N_participants %>% summarize(n = sum(n)) %>% 
    add_column(cn = "all", id = "all")
  res.goodness = compute_goodness_dirichlets(
    params.fit.single, df.pe_task, N_all, cleaned_dir, each=F
  )
  res.goodness %>% distinct_at(vars(c(stimulus_id)), .keep_all = T)
  p = plot_goodness_dirichlets(res.goodness, params.fit.single, df.pe_task, 
                               paste(cleaned_dir, "plots", sep = FS))
  
  # single distribution for data from each stimulus
  df.params.fit = run_fit_dirichlet(cleaned_dir, per_stimulus=T) %>%
    add_column(p_cn = 1 / 13) %>% mutate(cn = id)
  # df.params.fit = read_csv(paste(cleaned_dir, "params-fitted-dirichlet.csv", 
  #                                sep=FS))  %>%
  #   add_column(p_cn = 1/13) %>% mutate(cn = id)
  tables.dirichlet = makeDirichletTables(df.params.fit, cleaned_dir)
  message("compute goodness of dirichlet fits ...")
  res.goodness = compute_goodness_dirichlets(
    df.params.fit, df.pe_task, N_participants, cleaned_dir
  )
  res.goodness %>% distinct_at(vars(c(stimulus_id)), .keep_all = T)
  p = plot_goodness_dirichlets(res.goodness, df.params.fit, df.pe_task, 
                               paste(cleaned_dir, "plots", sep = FS))
  
  # generate abstract state prior tables with filtered data
  tables.model = makeAbstractPriorTables(dir_empiric = cleaned_dir)
}
