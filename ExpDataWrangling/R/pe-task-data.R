# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# Function definitions ----------------------------------------------------
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

#' Computes probabilities corresponding to each utterance based on 4 table values.
#'
#' @param df tibble with columns 'bg', 'b', 'g', 'none' (numeric).
#' @returns tibble with with 12 added columns with utterance probabilities for
#' conditionals, literals and might+literal.
#' @examples
#' add_probs(df = tibble(bg=0.4, b=0.1, g=0.1, none=0.4))
add_probs <- function(df){
  df <- df %>% mutate(p_a = bg+b, p_c = bg+g, p_na = g+none, p_nc = b+none) %>%
    mutate(p_c_given_a = bg / p_a,
           p_c_given_na = g / p_na,
           p_a_given_c = bg / p_c,
           p_a_given_nc = b / p_nc,
           p_nc_given_a = b/p_a,
           p_nc_given_na = none/p_na,
           p_na_given_c = g/p_c,
           p_na_given_nc = none/p_nc,
           p_likely_a = p_a,
           p_likely_na = p_na,
           p_likely_c = p_c,
           p_likely_nc = p_nc
    )
  return(df)
}

# @arg df1 in long-format
# smooth slider ratings from prior elicitation experiment (exp1)
add_smoothed_exp1 <- function(df1){
  df = df1 %>% group_by(prolific_id, id) %>% filter(sum(response) != 0)
  # normalize st. slider responses sum up to 1 but also keep original response
  df.with_smoothed = df %>%
    mutate(n = sum(response + EPSILON),
           pe_task.smooth = (response + EPSILON) / n) %>%
    dplyr::select(-n)
  return(df.with_smoothed)
}

save_prob_tables <- function(df.utt_probs, result_dir, fn_tbls_smooth, fn_tbls_empiric_pids){
  # Save all tables (with smoothed values)
  tables_all <- df.utt_probs %>% dplyr::select(id, prob, prolific_id, response) %>%
    pivot_wider(names_from = prob, values_from = response)
  path_tables_all <- paste(result_dir, fn_tbls_smooth, sep = FS);
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
  save_data(tables_empiric_pids, paste(result_dir, fn_tbls_empiric_pids, sep = FS))
}

#' converts table values to their closest round value restless divisible by 0.05
#' and adds 4 new columns (names end wiht '.binned') to input tibble.
#'
#' @param tables tibble with 4 columns containing rounded cell values (numeric
#' between 0-100),names ending by '.round' and another column functioning as
#' id to join binned and rounded tables again. (any name).
#' @returns Tibble with 4 added columns.
#' @examples
#' bin_tables(tibble(a.round = 2, b.round = 8, c.round = 50, d.round = 40))
bin_tables = function(tables){
  tbls= tables %>% rowid_to_column() %>% group_by(rowid) %>%
    pivot_longer(cols = ends_with(".round"),
                 names_to="cell", values_to="p") %>%
    mutate(mod = p %% 10)
  tables.binned = tbls %>%
    mutate(match_to = case_when(p %in% c(0,1,2) ~ 0,
                                p %in% c(98, 99, 100) ~ 100,
                                mod %in% c(3,4,5,6,7) ~ p - mod + 5,
                                mod %in% c(8,9) ~ p - mod + 10,
                                mod %in% c(0,1,2) ~ p - mod)) %>%
    mutate(cell = str_replace(cell, ".round", ".binned")) %>%
    dplyr::select(-p, -mod) %>%
    pivot_wider(names_from = "cell", values_from = "match_to") %>%
    ungroup() %>% dplyr::select(-rowid)
  # add rounded values again
  tables.binned = left_join(tables.binned, tables)
  return(tables.binned)
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
