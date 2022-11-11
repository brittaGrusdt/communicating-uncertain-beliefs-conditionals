library(brms)
library(here)
source(here("R", "utils.R"))

path_cleaned_data = here("data", "cleaned-data.csv")
data = get_controlled_factors(read_csv(path_cleaned_data)) %>% 
  mutate(utterance = utt.standardized) %>% 
  chunk_utterances() %>%
  rename(utt_type = utterance, utterance = utt.standardized) %>% 
  mutate(utt_type = as.character(utt_type))

data.uc = data %>% 
  filter(human_exp2 == 1) %>% 
  dplyr::select(id, relation, relation_type, utt_type, prior_blue, prolific_id, 
                pe_task.smooth, pe_task) %>% 
  mutate(conditional = case_when(utt_type != "conditional" ~ 0, T ~ 1),
         prior_blue = as.character(prior_blue),
         prior_blue = case_when(startsWith(prior_blue, "unc") ~ "unc",
                                T ~ "confident")) %>% 
  rename(subject_id = prolific_id)

# Model relation:dependent vs. independent
# plot data 
data.uc %>%
  group_by(relation_type, prior_blue) %>% 
  summarize(N = n(), num_conditionals = sum(conditional)) %>% 
  mutate(ratio = num_conditionals / N) %>% 
  ggplot(aes(x = relation_type, y = ratio, group = prior_blue, color = prior_blue)) +
  geom_point() + geom_line() +
  theme_minimal() + theme(legend.position = "top") +
  ylab("ratio created conditionals")
# model
df.dep_ind = data.uc %>% dplyr::select(-relation) %>% 
  rename(relation = relation_type)
fit.bernoulli1 = brm(
  data = df.dep_ind,
  family = "bernoulli",
  formula = conditional ~ prior_blue * relation +
    (1 + prior_blue + relation|subject_id),
  seed = 0710, 
  iter = 4000,
  control = list(adapt_delta = 0.9)
)
pp_check(fit.bernoulli1)

# reference level: dependent + confident
# Conditionals are more likely in dependent situations than in INdependent
# H1. when UNCERTAIN about blue block
hypothesis(fit.bernoulli1,
           "prior_blueunc - (relationindependent + prior_blueunc + prior_blueunc:relationindependent) > 0")

# H2. and when CONFIDENT about blue block.
hypothesis(fit.bernoulli1, "relationindependent < 0")

# In dependent situations
# H3. conditionals are more likely in UNCERTAIN than in CONFIDENT situations
hypothesis(fit.bernoulli1, "prior_blueunc>0")


# More fine-grained models ------------------------------------------------

# 1. Model relation: if1, if2 vs. independent
# plot data by relation
data.uc %>%
  group_by(relation, prior_blue) %>% 
  summarize(N = n(), num_conditionals = sum(conditional)) %>% 
  mutate(ratio = num_conditionals / N) %>% 
  ggplot(aes(x = relation, y = ratio, group = prior_blue, color = prior_blue)) +
  geom_point() + geom_line() +
  theme_minimal() + theme(legend.position="top") +
  ylab("ratio created conditionals")

fit.bernoulli2 = brm(
  data = data.uc,
  family = "bernoulli",
  formula = conditional ~ prior_blue * relation +
    (1 + prior_blue + relation|subject_id),
  seed = 0710, 
  iter = 4000,
  control = list(adapt_delta = 0.9)
)
pp_check(fit.bernoulli2)

# if1 uncertain
hypothesis(fit.bernoulli2, 
           "prior_blueunc + relationif1 + prior_blueunc:relationif1 > 0")

# if2 uncertain
hypothesis(fit.bernoulli2, "prior_blueunc > 0")

hypothesis(fit.bernoulli2, "prior_blueunc:relationif1 > 0")
hypothesis(fit.bernoulli2, "relationindependent < 0")


# 2. with PE-task data instead of trial condition as predictor
data.pe = data %>% filter(prob == "p_a") %>%
  dplyr::select(prolific_id, id, pe_task) %>% 
  rename(p_blue = pe_task, subject_id = prolific_id)
  
data.p_blue = left_join(data.uc %>% 
                          dplyr::select(-pe_task.smooth, -pe_task, -prior_blue),
                        data.pe) %>% 
  mutate(prior_blue = case_when(p_blue <= 0.2 | p_blue >= 0.8 ~ "confident", 
                                T ~ "unc"))
data.p_blue %>%
  group_by(relation, prior_blue) %>% 
  summarize(N = n(), num_conditionals = sum(conditional)) %>% 
  mutate(ratio = num_conditionals / N) %>% 
  ggplot(aes(x = prior_blue, y = ratio, group = relation, color = relation)) +
  geom_point() + geom_line() +
  theme_minimal() + theme(legend.position="top") +
  ylab("ratio created conditionals")

fit.bernoulli3 = brm(
  data = data.p_blue,
  family = "bernoulli",
  formula = conditional ~ prior_blue * relation +
    (1 + prior_blue + relation|subject_id),
  seed = 0710, 
  iter = 4000,
  control = list(adapt_delta = 0.9)
)
pp_check(fit.bernoulli3)


hypothesis(fit.bernoulli3, "prior_blueunc > 0")
hypothesis(fit.bernoulli3, "prior_blueunc:relationif1 > 0")
hypothesis(fit.bernoulli3, "relationindependent < 0")




