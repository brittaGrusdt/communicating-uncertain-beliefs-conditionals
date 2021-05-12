library(brms)
source(here("R", "utils.R"))
source(here("model", "R", "helper-functions.R"))

data = readRDS(here("data", "prolific", "blockwords", "filtered_data",
                    "human-exp1-smoothed-exp2.rds"))
data = get_controlled_factors(data)

data.uc = data %>% filter(human_exp2 == 1 & id!="ind2") %>% 
  mutate(utt=utterance) %>% 
  chunk_utterances() %>%
  rename(utt_type=utterance, utterance=utt) %>% 
  mutate(utt_type=as.character(utt_type)) %>%
  dplyr::select(-human_exp2)


df.m_rel = data.uc %>% 
  ungroup() %>% dplyr::select(relation_type, utt_type, prior_blue, prolific_id) %>%
  mutate(conditional=case_when(utt_type != "conditional" ~ 0, T ~ 1),
         prior_blue = as.character(prior_blue),
         prior_blue = case_when(startsWith(prior_blue, "unc") ~ "unc",
                                T ~ "confident")) %>% 
  rename(relation=relation_type, subject_id=prolific_id) %>%
  group_by(relation, prior_blue)

df.sum = df.m_rel %>%
  summarize(N=n(), num_conditionals=sum(conditional)) %>% 
  mutate(ratio=num_conditionals/N)

# plot data
df.sum %>% 
  ggplot(aes(x=relation, y=ratio)) +
  geom_point(aes(color=prior_blue)) +
  theme_minimal() + theme(legend.position="top") +
  ylab("ratio created conditionals")

fit.bernoulli = brm(
  data = df.m_rel,
  family = "bernoulli",
  formula = conditional ~ prior_blue * relation +
    (1 + prior_blue + relation|subject_id),
  seed=0710, 
  iter = 4000,
  control = list(adapt_delta = 0.9)
)
pp_check(fit.bernoulli)

# reference level: dependent + confident
# Conditionals are more likely in dependent situations than in INdependent
# H1. when UNCERTAIN about blue block
hypothesis(fit.bernoulli,
           "prior_blueunc - (relationindependent + prior_blueunc + prior_blueunc:relationindependent) > 0")

# H2. and when CONFIDENT about blue block.
hypothesis(fit.bernoulli, "relationindependent < 0")

# In dependent situations
# H3. conditionals are more likely in UNCERTAIN than in CONFIDENT situations
hypothesis(fit.bernoulli, "prior_blueunc>0")



