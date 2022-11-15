library(tidyverse)
library(here)
library(matrixStats)

# Function Definitions ----------------------------------------------------
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

