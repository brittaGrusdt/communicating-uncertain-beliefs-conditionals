library(here)
library(tidyverse)
source("R/utils.R")
source("R/utils-exp2.R")
source("R/Dirichlet-fits.R")
source(here("R", "analysis-utils.R"))

# 1. Setup ----------------------------------------------------------------
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

# 2. Processing -----------------------------------------------------------
data <- process_data(path_to_raw_data, N_trials)
save_prob_tables(data$test.pe_utt_probs_smooth, result_dir)

train_data = data[c("train.attention", "train.pe", "train.slider_choice")]
save_train_data(train_data, result_dir)

# 3. Check comments for cleaning data  ------------------------------------
df.comments = data$comments %>% filter(!is.na(comments) & comments!="")
df.comments$comments
# filter based on these comments:
out.comments = df.comments %>%
  filter(comments == "I apologize because the first couple of graphics did not appear properly, so I picked any answer. The rest of the study went fine." |
           comments == "The descriptions on this quiz were terrible and confusing. Non native English speakers will have a lot of trouble." |
           comments == "This was hard to understand")
# remove all trials for these participants
test_trials = data$test.pe_task$id %>% unique()
df.out_comments = out.comments %>% dplyr::select(prolific_id) %>% 
  add_column(id = rep(list(test_trials), nrow(out.comments))) %>%
  unnest(c(id)) %>%
  group_by(prolific_id)

# 4. compute + save quality of data in slider ratings (used for cleaning): 
df = data$test.pe_utt_probs_orig %>% dplyr::select(-utt.standardized) %>% 
  filter(!is.na(slider))
prior.quality = distancesResponses(df)
save_data(prior.quality, paste(result_dir, "test-data-prior-quality.rds", sep=FS))


# 5. merge data from prior elicitation (PE) and production (UC) to save all in one csv
joint.pe_smooth = join_pe_uc_data(data$test.pe_utt_probs_smooth, data$test.uc_task)
joint.pe_orig = join_pe_uc_data(data$test.pe_utt_probs_orig, data$test.uc_task)
uc_pe_data = left_join(joint.pe_orig, 
                       joint.pe_smooth %>% rename(pe_task.smooth = pe_task))
joint_data = left_join(uc_pe_data, data$info, by="prolific_id")
write_csv(joint_data, here("data", "all-data.csv"))

save_utt_frequencies(joint_data, result_dir)

# 6. Filter data according to predefined criteria -------------------------
df.out = get_ids_to_exclude(joint_data, train_data, data$color, 
                            prior.quality, cleaned_dir, df.out_comments)

joint_data.cleaned = anti_join(joint_data, df.out, by = c("prolific_id", "id"))
write_csv(joint_data.cleaned, here("data", "cleaned-data.csv"))

# save smoothed probability tables separately (for model)
pe_task_utt_probs.cleaned = anti_join(data$test.pe_utt_probs_smooth, 
                                      df.out, by = c("prolific_id", "id"))
save_prob_tables(pe_task_utt_probs.cleaned, cleaned_dir)

save_utt_frequencies(joint_data.cleaned, cleaned_dir)
train_data.cleaned = clean_train_data(train_data, df.out)
save_train_data(train_data.cleaned, cleaned_dir)

# 7. format cleaned data for model ----------------------------------------
df = joint_data.cleaned %>%
  dplyr::select(prolific_id, id, slider, pe_task.smooth, utt.standardized) %>% 
  filter(!is.na(slider))

process_and_save_data_for_model(df, cleaned_dir)


