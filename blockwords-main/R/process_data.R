library(here)
library(tidyverse)
source("R/utils.R")
source("R/utils-exp2.R")
source("R/Dirichlet-fits.R")
source(here("R", "analysis-utils.R"))

# Setup -------------------------------------------------------------------
N_trials = list(train = 14 + 3 + 10, 
                test = 13 * 2 + 1, 
                color_vision = 6,
                slider_choice = 10, 
                attention_check = 3);
data_dir = here("data")
data_fn <- "raw_results_54_blockwords-main_BG.csv"
path_to_raw_data = paste(data_dir, data_fn, sep=.Platform$file.sep)

result_dir <- here("data", "formatted-all")
if(!dir.exists(result_dir)) dir.create(result_dir, recursive=TRUE);

# create folder for cleaned data
cleaned_dir = here("data", "formatted-cleaned")
if(!dir.exists(cleaned_dir)) dir.create(cleaned_dir)

# Processing --------------------------------------------------------------
data <- process_data(path_to_raw_data, result_dir, N_trials)

# Check comments for cleaning data  ---------------------------------------
df.comments = data$comments %>% filter(!is.na(comments) & comments!="")
df.comments$comments
# filter based on these comments:
out.comments = df.comments %>%
  filter(comments == "I apologize because the first couple of graphics did not appear properly, so I picked any answer. The rest of the study went fine." |
           comments == "The descriptions on this quiz were terrible and confusing. Non native English speakers will have a lot of trouble." |
           comments == "This was hard to understand")

# remove all trials for these participants
test_trials = data$test$id %>% unique()
df.out = out.comments %>% dplyr::select(prolific_id) %>% 
  add_column(id = rep(list(test_trials), nrow(out.comments))) %>%
  unnest(c(id)) %>%
  group_by(prolific_id)
# write_csv(df.out, here("data", "out_by_comments.csv"))


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
save_data(joint.human.orig, paste(result_dir, "human-exp1-orig-exp2.rds", sep = FS))
save_data(joint.human.smooth,paste(result_dir, "human-exp1-smoothed-exp2.rds", sep = FS))

# save count + frequency of created utterances per stimulus
human.exp2.avg = task2_avg_per_stimulus(result_dir)
write_csv(human.exp2.avg, paste(result_dir, "behavioral-avg-task2.csv", sep=FS))
# also save for chunked utterances
df2.chunked = human.exp2.avg %>% chunk_utterances()
df2.chunked.avg = df2.chunked %>% group_by(id, utterance) %>% 
  summarize(count = sum(count), ratio = sum(ratio), .groups="drop_last")
write_csv(df2.chunked.avg, paste(result_dir, "behavioral-avg-task2-chunked.csv", sep = FS))

# Participants info
save_data(data$info, paste(result_dir, "participants-info.rds", sep = FS))

# compute + save quality of data in slider ratings:
df = exp1.human.orig %>% rename(response=human_exp1) %>%
  dplyr::select(-utterance) %>% filter(!is.na(question))
prior.quality = distancesResponses(df)
save_data(prior.quality, paste(result_dir, "test-data-prior-quality.rds", sep = FS))


# Filter data according to predefined criteria ----------------------------
df.cleaned = filter_data(result_dir, cleaned_dir, here("data", "out_by_comments.csv"))

# format data for model ---------------------------------------------------
process_and_save_data_for_model(cleaned_dir)


