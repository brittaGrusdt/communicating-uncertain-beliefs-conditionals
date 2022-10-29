library(here)
library(tidyverse)
source("R/utils.R")
source("R/utils-exp2.R")
source("R/Dirichlet-fits.R")

# Setup -------------------------------------------------------------------
N_trials = list(train = 14 + 3 + 10, 
                test = 13 * 2 + 1, 
                color_vision = 6,
                slider_choice = 10, 
                attention_check = 3);
data_fn <- "raw_results_54_blockwords-main_BG.csv"
data_dir = here("data")
path_to_raw_data = paste(data_dir, data_fn, sep=.Platform$file.sep)

result_dir <- here("data", "formatted-all")
if(!dir.exists(result_dir)) dir.create(result_dir, recursive=TRUE);

# Processing --------------------------------------------------------------
data <- process_data(path_to_raw_data, result_dir, N_trials)

# Save data in different formats ------------------------------------------
# PE-Task data
test.prior =  data$test %>% filter(str_detect(trial_name, "multiple_slider"))
exp1.human.orig = formatPriorElicitationData(test.prior, smoothed = FALSE) %>%
  dplyr::select(-response)
exp1.human.smooth = formatPriorElicitationData(test.prior, smoothed = TRUE) %>%
  dplyr::select(-response) 
save_data(exp1.human.orig %>% rename(response = human_exp1),
          paste(result_dir, "human-exp1-orig.rds", sep = FS))
save_data(exp1.human.smooth %>% rename(response=human_exp1),
          paste(result_dir, "human-exp1-smoothed.rds", sep = FS))

# UC-Task data
test.production = data$test %>% filter(str_detect(trial_name, "fridge_"))
exp2.human = test.production %>% 
  filter(id != "ind2") %>% # Test Trial
  dplyr::select(prolific_id, id, response, RT, custom_response, 
                response_non_standardized) %>%
  rename(utterance = response) %>% add_column(human_exp2 = 1)
save_data(exp2.human %>% rename(response=utterance),
          paste(result_dir, "human-exp2.rds", sep = FS))

# merge data from prior elicitation (PE) and production (UC)
joint.human.smooth = left_join(
  exp1.human.smooth %>% dplyr::select(-question, -RT),
  exp2.human %>% dplyr::select(-RT, -custom_response),
  by = c("prolific_id", "id", "utterance")
)
joint.human.orig = left_join(
  exp1.human.orig %>% dplyr::select(-question, -RT),
  exp2.human %>% dplyr::select(-RT, -custom_response),
  by = c("prolific_id", "id", "utterance")
)
save_data(joint.human.orig, 
          paste(result_dir, "human-exp1-orig-exp2.rds", sep = FS))
save_data(joint.human.smooth,
          paste(result_dir, "human-exp1-smoothed-exp2.rds", sep = FS))

# save count + frequency of created utterances per stimulus
human.exp2.avg = task2_avg_per_stimulus(result_dir)
write_csv(human.exp2.avg, paste(result_dir, "behavioral-avg-task2.csv", sep=FS))
# also save for chunked utterances
df2.chunked = human.exp2.avg %>% chunk_utterances()
df2.chunked.avg = df2.chunked %>% group_by(id, utterance) %>% 
  summarize(count = sum(count), ratio = sum(ratio), .groups="drop_last")
write_csv(df2.chunked.avg, 
          paste(result_dir, "behavioral-avg-task2-chunked.csv", sep = FS))

# Participants info
save_data(data$info, paste(result_dir, "participants-info.rds", sep = FS))

# Quality of data in slider ratings:
df = exp1.human.orig %>% rename(response=human_exp1) %>%
  dplyr::select(-utterance) %>% filter(!is.na(question))
prior.quality = distancesResponses(df)
save_data(prior.quality, 
          paste(result_dir, "test-data-prior-quality.rds", sep = FS))


# look at comments/time/quality and specify in results_joint_experiment.Rmd
# participants to be excluded,then run filter-function
source(here("R", "analysis-utils.R"))
df.filtered = filter_data(result_dir, 
                          here("data", "out_by_comments.csv"),
                          here("data", "out_by_quality.csv"))



