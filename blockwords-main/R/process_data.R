library(here)
library(devtools)
devtools::install('../ExpDataWrangling')

library(ExpDataWrangling)
source(here("R", "Dirichlet-fits.R"))
source(here("R", "model-tables.R"))

# 1. Setup ----------------------------------------------------------------
Sys.setenv(R_CONFIG_ACTIVE = "process_data")
params <- config::get()

path_to_raw_data = here(params$dir_data, params$fn_raw_data)

result_dir <- here(params$dir_formatted_data_all)
if(!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE);

# create folder for cleaned data
cleaned_dir = here(params$dir_formatted_data_cleaned)
cleaned_dir.plots = paste(cleaned_dir, "plots", sep = FS)
if(!dir.exists(cleaned_dir.plots)) dir.create(cleaned_dir.plots, recursive = T)

# 2. Processing -----------------------------------------------------------
data <- process_data(path_to_raw_data, params$N_trials)
save_prob_tables(data$test.pe_utt_probs_smooth, result_dir, 
                 params$fn_tables_smooth, params$fn_tbls_empiric_pids)

train_data = data[c("train.attention", "train.pe", "train.slider_choice")]
write_csv(data$`train.attention`, paste(result_dir, params$fn_train_attention, sep = FS))
write_csv(data$`train.slider_choice`, paste(result_dir, params$fn_train_slider_choice, sep = FS))
write_csv(data$`train.pe`, paste(result_dir, params$fn_train_pe, sep = FS))


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
save_data(prior.quality, here(result_dir, params$fn_quality_data))


# 5. merge data from prior elicitation (PE) and production (UC) to save all in one csv
joint.pe_smooth = join_pe_uc_data(data$test.pe_utt_probs_smooth, data$test.uc_task)
joint.pe_orig = join_pe_uc_data(data$test.pe_utt_probs_orig, data$test.uc_task)
uc_pe_data = left_join(joint.pe_orig, 
                       joint.pe_smooth %>% rename(pe_task.smooth = pe_task))
joint_data = left_join(uc_pe_data, data$info, by="prolific_id")
write_csv(joint_data, here(params$dir_data, params$fn_formatted_data_all))

save_utt_frequencies(joint_data, result_dir, params$fn_uc_avg, params$fn_uc_avg_chunked)

# 6. Filter data according to predefined criteria -------------------------
path_csv_ids_out <- paste(cleaned_dir, params$fn_ids_excluded, sep = FS)
df.out = get_ids_to_exclude(joint_data, train_data, data$color, 
                            prior.quality, df.out_comments, path_csv_ids_out)

joint_data.cleaned = anti_join(joint_data, df.out, by = c("prolific_id", "id"))
write_csv(joint_data.cleaned, here(params$dir_data, params$fn_formatted_data_cleaned))

# save smoothed probability tables separately (for model)
pe_task_utt_probs.cleaned = anti_join(data$test.pe_utt_probs_smooth, 
                                      df.out, by = c("prolific_id", "id"))
save_prob_tables(pe_task_utt_probs.cleaned, cleaned_dir, 
                 params$fn_tables_smooth, params$fn_tbls_empiric_pids)

save_utt_frequencies(joint_data.cleaned, cleaned_dir, params$fn_uc_avg, params$fn_uc_avg_chunked)
train_data.cleaned = clean_train_data(train_data, df.out)
write_csv(train_data.cleaned$`train.attention`, paste(cleaned_dir, params$fn_train_attention, sep = FS))
write_csv(train_data.cleaned$`train.slider_choice`, paste(cleaned_dir, params$fn_train_slider_choice, sep = FS))
write_csv(train_data.cleaned$`train.pe`, paste(cleaned_dir, params$fn_train_pe, sep = FS))


# 7. Prepare Data for situation-specific model (dirichlet) ----------------
Sys.setenv(R_CONFIG_ACTIVE = "situation_specific_prior")
params.sit_specific <- config::get()

path_simulated_p.overall = paste(cleaned_dir, 
                                 params.sit_specific$fn_simulated_p_overall, 
                                 sep = FS)
path_simulated_p.contexts = paste(cleaned_dir, 
                                  params.sit_specific$fn_simulated_p_by_context,
                                  sep = FS)
# fit dirichlet distributions to cleaned data
df.pe_task = joint_data.cleaned %>%
  dplyr::select(prolific_id, id, slider, pe_task.smooth, utt.standardized) %>% 
  filter(!is.na(slider))
N_participants = df.pe_task %>% group_by(id) %>% 
  distinct_at(vars(c(prolific_id))) %>% summarize(n = n())

# single distribution for all data
params.fit.single = run_fit_dirichlet(cleaned_dir, per_stimulus = F) %>%
  add_column(p_cn = 1, cn = "all") %>% mutate(id = "all")
df.N_all = N_participants %>% summarize(n = sum(n)) %>%
  add_column(cn = "all", id = "all")
res.goodness = compute_goodness_dirichlets(
  params.fit.single, df.pe_task, df.N_all, path_simulated_p.overall
)
res.goodness %>% distinct_at(vars(c(stimulus_id)), .keep_all = T)
fn = "goodness-single-dirichlet-fit.png"
path_fn = paste(cleaned_dir.plots, fn, sep = FS)
p = plot_goodness_dirichlets(params.fit.single, res.goodness, df.pe_task, path_fn)

# fit one distribution for data from EACH stimulus
df.params.fit = run_fit_dirichlet(cleaned_dir, per_stimulus = T) %>%
  add_column(p_cn = 1 / 13) %>% mutate(cn = id)
seed = params.sit_specific$seed_fitted_tables
tables.dirichlet = makeDirichletTables(df.params.fit, seed, cleaned_dir, 
                                       params$dir_model_input)
res.goodness = compute_goodness_dirichlets(
  df.params.fit, df.pe_task, N_participants, path_simulated_p.contexts
)
res.goodness %>% distinct_at(vars(c(stimulus_id)), .keep_all = T)
fn = "goodness-dirichlet-fits.png"
p = plot_goodness_dirichlets(res.goodness, df.params.fit, df.pe_task,
                             paste(cleaned_dir.plots, fn, sep = FS))


# 8. Prepare Data for abstract state prior --------------------------------
# generate abstract state prior tables with filtered data
tables.model = makeAbstractPriorTables(dir_empiric = cleaned_dir)

