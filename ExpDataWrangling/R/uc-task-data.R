# Constants ---------------------------------------------------------------
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

# Function Definitions ----------------------------------------------------
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

#' Adds column (pe_selected_utt) with probability corresponding to each utterance.
#'
#' @param observations tibble with all probability columns (e.g., from add_probs)
#' and column 'utt.standardized'.
#' @returns A numeric vector.
#' @examples
#' add(1, 1)
#' add(10, 1)
compute_utt_probs = function(observations){
  observations %>%
    mutate(pe_selected_utt = case_when(
      #literals / might + literal
      utt.standardized %in% c(standardized.sentences$only_b,
                              standardized.sentences$might_b) ~ p_a,
      utt.standardized %in% c(standardized.sentences$only_nb,
                              standardized.sentences$might_nb) ~ p_na,
      utt.standardized %in% c(standardized.sentences$only_g,
                              standardized.sentences$might_g) ~ p_c,
      utt.standardized %in% c(standardized.sentences$only_ng,
                              standardized.sentences$might_ng) ~ p_nc,
      # conjunctions
      utt.standardized == standardized.sentences$bg ~ ac,
      utt.standardized == standardized.sentences$b ~ `a-c`,
      utt.standardized == standardized.sentences$g ~ `-ac`,
      utt.standardized == standardized.sentences$none ~ `-a-c`,

      # conditionals
      utt.standardized == standardized.sentences$if_bg ~ p_c_given_a,
      utt.standardized == standardized.sentences$if_bng ~ p_nc_given_a,
      utt.standardized == standardized.sentences$if_nbg ~ p_c_given_na,
      utt.standardized == standardized.sentences$if_nbng ~ p_nc_given_na,

      utt.standardized == standardized.sentences$if_gb ~ p_a_given_c,
      utt.standardized == standardized.sentences$if_gnb ~ p_na_given_c,
      utt.standardized == standardized.sentences$if_ngb ~ p_a_given_nc,
      utt.standardized == standardized.sentences$if_ngnb ~ p_na_given_nc
    ))
}

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

save_utt_frequencies = function(uc_pe_data, result_dir, fn, fn_chunked){
  human.exp2.avg = task2_avg_per_stimulus(uc_pe_data)
  path_utt_freq = paste(result_dir, fn, sep = FS)
  write_csv(human.exp2.avg, path_utt_freq)
  message(paste('frequency utterances written to:', path_utt_freq))

  # also save for chunked utterances
  df2.chunked = human.exp2.avg %>% rename(utterance = utt.standardized) %>%
    chunk_utterances() %>% rename(utt_type = utterance)
  df2.chunked.avg = df2.chunked %>% group_by(id, utt_type) %>%
    summarize(count = sum(count), ratio = sum(ratio), .groups="drop_last")

  path_utt_freq_chunked = paste(result_dir, fn_chunked, sep = FS)
  write_csv(df2.chunked.avg, path_utt_freq_chunked)
  message(paste('frequency utterance types written to:', path_utt_freq_chunked))
}

#' chunks concrete utterances into conditionals, conjunctions, might+literal,
#' and literal. Works with utterances coded in natural language or as abbreviated
#' in model (> for 'if').
#' @param data tibble with column 'utterance'.
#' @param utts_kept (optional) list of concrete utterances that shall not be
#'  replaced by their utterance type
#' @returns Tibble with transformed values in column 'utterance'.
#' @examples
#' chunk_utterances(data=tibble(utterance = c("blue and green fall", "C > A")))
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
    utterance = factor(utterance, levels = c(utts_kept, levels)
    )
  );
  return(data)
}


