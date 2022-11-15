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

# according to criteria filter out and save all cleaned data separately
# @arg df.out_comments: tibble with cols: 'prolific_id', 'id'
get_ids_to_exclude = function(test_data, train_data, color_trial_data,
                              quality_pe_data, df.out_comments, path_target_csv) {

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
  write_csv(df.out, path_target_csv)

  return(df.out)
}

save_data <- function(data, target_path){
  target_dir = paste(head(str_split(target_path, FS)[[1]], -1), collapse="/")
  if(!dir.exists(target_dir)){
    stop(paste(target_dir, "does not exist"))
  }
  data %>% write_rds(target_path)
  message(paste("saved to:", target_path))
}
