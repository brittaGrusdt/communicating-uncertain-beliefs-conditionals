library(here)
source(here("R", "analysis-utils.R"))
DATA.all = load_formatted_data(here("data", "formatted-cleaned"))

# UC-task -----------------------------------------------------------------
behav.uc_task.all = read_csv(
  here("data", "prolific", "results_54_blockwords-main_BG.csv")
  ) %>%
  filter(str_detect(trial_name, "fridge_") & id != "ind2") %>% 
  separate(col=response, sep="\\|", into=c("response", "custom_response")) %>% 
  standardize_color_groups_exp2() %>% 
  distinct_at(vars(c(response, custom_response, prolific_id, id, cost, RT))) %>% 
  mutate(response.orig = response) %>% 
  standardize_sentences() %>% 
  mutate(custom_response = as.character(custom_response))

ids.included = DATA.all$production %>% distinct_at(vars(c(prolific_id, id)))
behav.uc_task.cleaned = left_join(ids.included, behav.uc_task.all) %>% 
  rename(utt.standardized = response, uc_task = response.orig, cost.uc = cost,
         RT.uc = RT)

data.uc = left_join(
  behav.uc_task.cleaned, 
  DATA.all$info %>% mutate(gender = case_when(is.na(gender) ~ "not specified", 
                                              T ~ gender))
)
# PE-task -----------------------------------------------------------------
pe.smooth = DATA.all$joint.smooth %>% dplyr::select(-human_exp2) %>% 
  rename(utt.standardized = utterance, pe.smoothed_derived = human_exp1)

pe.orig = DATA.all$prior.orig %>%
  filter(!is.na(question)) %>% dplyr::select(-question) %>% 
  rename(utt.standardized = utterance, RT.pe = RT, pe_task = response)

data.pe = left_join(pe.smooth, pe.orig) %>% 
  mutate(RT.pe = case_when(is.na(RT.pe) ~ max(RT.pe, na.rm = T), T ~ RT.pe))
  
data = left_join(data.pe, data.uc) %>% group_by(prolific_id, id) %>% 
  mutate(RT.uc = case_when(is.na(RT.uc) ~ max(RT.uc, na.rm = T),
                           TRUE ~ RT.uc),
         cost.uc = case_when(is.na(cost.uc) ~ max(cost.uc, na.rm = T),
                             TRUE ~ cost.uc),
         age = case_when(is.na(age) ~ max(age, na.rm = T),
                         TRUE ~ age),
         timeSpent = case_when(is.na(timeSpent) ~ max(timeSpent, na.rm = T),
                               TRUE ~ timeSpent),
         uc_task = as.character(uc_task),
         uc_task = case_when(is.na(uc_task) ~ max(uc_task, na.rm = T),
                             TRUE ~ uc_task),
         custom_response = case_when(is.na(custom_response) ~ 
                                       max(custom_response, na.rm = T),
                                     TRUE ~ custom_response),
         education = case_when(is.na(education) ~ max(education, na.rm = T),
                               TRUE ~ education),
         gender = case_when(is.na(gender) ~ max(gender, na.rm = T),
                            TRUE ~ gender)
         )

write_csv(data, here("data", "cleaned-data.csv"))
  
