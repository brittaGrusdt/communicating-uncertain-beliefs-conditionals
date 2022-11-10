library(rwebppl)
library(tidyverse)
library(here)
source(here("model", "R", "helpers-webppl.R"))
source(here("model", "R", "helper-functions.R"))
source(here("R", "utils.R"))
source(here("R", "utils-exp2.R"))

path_cleaned_data = here("data", "cleaned-data.csv")
# select priors to be used HERE -------------------------------------------

# used_tables = "tables_dirichlet_prior_with_empirical"
used_tables = "tables_abstract_prior_with_empirical"

# Setup -------------------------------------------------------------------
params <- configure(c("speaker", used_tables))
params$used_tables = used_tables
if(!dir.exists(params$target_dir)) dir.create(params$target_dir, recursive = T)
params$target_params <- file.path(params$target_dir, params$target_params, fsep=fs)

## Retrieve tables
path_tbls = params$target_uniq_tables
tables <- readRDS(path_tbls)
print(paste("tables read from:", path_tbls))

params$tables = tables %>% ungroup() %>%
  dplyr::select(bn_id, table_id, ps, vs, ll, cn)

# --------- Which states to compute predictions for --------- # 
params$bn_ids_mapping = tables %>% filter(added) %>%
  unnest(c(p_id, stimulus)) %>% group_by(p_id, bn_id) %>% 
  dplyr::select(table_id, bn_id, p_id, ll, stimulus, cn, best.cn)
fn_avg =  "model-avg-predictions-empirical"

params$bn_ids = params$bn_ids_mapping %>%
  distinct_at(vars(c(bn_id))) %>% pull(bn_id)
tbls.long = params$tables %>% unnest(c(vs,ps))
tbls.wide = tbls.long %>% pivot_wider(names_from = "vs", values_from="ps")

## Generate/Retrieve utterances
if(params$generate_utterances || !file.exists(params$utts_path)){
  utterances <- generate_utts(params)
} else {
  utterances <- readRDS(params$utts_path)
  print(paste("utterances read from:", params$utts_path))
}
params$utterances <- utterances

# Run Model ---------------------------------------------------------------
posterior <- run_webppl(params$model_path, params)

# restructure data and save
speaker <- posterior %>% structure_speaker_data(params) %>% group_by(bn_id)
res.avg = average_predictions_empirical(speaker, params, fn_avg)
res.behav_model = join_model_behavioral(speaker, params, path_cleaned_data)
