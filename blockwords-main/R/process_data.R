library(here)
library(tidyverse)
source("R/utils.R")
source("R/utils-exp1.R")
source("R/utils-exp2.R")
source("R/Dirichlet-fits.R")

# Setup -------------------------------------------------------------------
# debug = TRUE test run vs. experimental (prolific) run
debug = FALSE
exp_name = "blockwords"

data_fn <- paste("results_54_", exp_name, "-main_BG.csv", sep="")

N_participants = 100
N_trials = list(train=14+3+10, test=13*2+1, color_vision=6,
                slider_choice=10, attention_check=3);

data_dir = ifelse(debug,  here("data", "test-runs"), here("data", "prolific"));
result_dir <- paste(data_dir, exp_name, sep=.Platform$file.sep)
if(!dir.exists(result_dir)) dir.create(result_dir, recursive=TRUE);

filtered_dir = paste(result_dir, "filtered_data", sep=.Platform$file.sep)
if(!dir.exists(filtered_dir)) dir.create(filtered_dir)


# Processing --------------------------------------------------------------
data <- process_data(data_dir, data_fn, result_dir, exp_name, debug, N_trials)

# Save data in different formats ------------------------------------------
formatPriorElicitationData = function(test.prior, smoothed=TRUE){
  df.prior_responses = test.prior %>%
    dplyr::select(-custom_response, -QUD, -trial_number, -trial_name) 
  if(smoothed){
    df.prior_responses = df.prior_responses %>% dplyr::select(-r_orig) %>% 
      pivot_wider(names_from = "question", values_from = "r_smooth")
  } else {
    df.prior_responses = df.prior_responses %>% dplyr::select(-r_smooth) %>%
      pivot_wider(names_from = "question", values_from = "r_orig")
  }
  df.prior_responses = df.prior_responses %>% add_probs()
  
  prior_responses = df.prior_responses %>%
    pivot_longer(cols=c("b", "g", "bg", "none", starts_with("p_")),
                 names_to="prob", values_to="val") %>%
    translate_probs_to_utts()
  
  exp1.human = prior_responses %>% dplyr::select(-group, -n) %>%
    rename(human_exp1=val, question=prob) %>% 
    mutate(question = case_when(!question %in% c("bg", "b", "g", "none") ~ NA_character_,
                                TRUE ~ question))
  return(exp1.human)
}
test.prior =  data$test %>% filter(str_detect(trial_name, "multiple_slider"))
exp1.human.orig = formatPriorElicitationData(test.prior, smoothed = FALSE) %>%
  dplyr::select(-response)
exp1.human.smooth = formatPriorElicitationData(test.prior, smoothed = TRUE) %>%
  dplyr::select(-response) 
save_data(exp1.human.orig %>% rename(response=human_exp1),
          paste(result_dir, "human-exp1-orig.rds", sep=fs))
save_data(exp1.human.smooth %>% rename(response=human_exp1),
          paste(result_dir, "human-exp1-smoothed.rds", sep=fs))

test.production = data$test %>% filter(str_detect(trial_name, "fridge_")) %>%
  standardize_sentences()
exp2.human = test.production %>% filter(id!="ind2") %>% 
  dplyr::select(prolific_id, id, response, RT, custom_response) %>%
  rename(utterance=response) %>% add_column(human_exp2=1)
save_data(exp2.human %>% rename(response=utterance),
          paste(result_dir, "human-exp2.rds", sep=fs))

# merge data from prior elicitation and production
joint.human.smooth = left_join(
  exp1.human.smooth %>% dplyr::select(-question, -RT),
  exp2.human %>% dplyr::select(-RT, -custom_response),
  by=c("prolific_id", "id", "utterance")
)
joint.human.orig = left_join(
  exp1.human.orig %>% dplyr::select(-question, -RT),
  exp2.human %>% dplyr::select(-RT, -custom_response),
  by=c("prolific_id", "id", "utterance")
)
save_data(joint.human.orig, paste(result_dir, "human-exp1-orig-exp2.rds", sep=fs))
save_data(joint.human.smooth, paste(result_dir, "human-exp1-smoothed-exp2.rds", sep=fs))

# save average per stimulus
human.exp2.avg = task2_avg_per_stimulus(result_dir)
write_csv(human.exp2.avg, paste(result_dir, "behavioral-avg-task2.csv", sep=fs))
# also with chunked utterances
df2.chunked = human.exp2.avg %>% chunk_utterances()
df2.chunked.avg = df2.chunked %>% group_by(id, utterance) %>% 
  summarize(count=sum(count), ratio=sum(ratio), .groups="drop_last")
write_csv(df2.chunked.avg, paste(result_dir, "behavioral-avg-task2-chunked.csv", sep=fs))


# Participants info
save_data(data$info, paste(result_dir, "participants-info.rds", sep=fs))

# Quality of data in slider ratings:
df = exp1.human.orig %>% rename(response=human_exp1) %>%
  dplyr::select(-utterance) %>% filter(!is.na(question))
prior.quality = distancesResponses(df)
save_data(prior.quality, paste(result_dir, "test-data-prior-quality.rds", sep=fs))


# look at comments/time/quality and specify in results_joint_experiment.Rmd
# participants to be excluded,then run filter-function
source(here("R", "analysis-utils.R"))
df.filtered = filter_data(result_dir, exp_name, "out_by_comments.csv",
                          "out_by_quality.csv")



