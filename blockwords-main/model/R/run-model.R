library(here)
library(rwebppl)
library(tidyverse)
library(ExpDataWrangling)
source(here("model", "R", "helpers-webppl.R"))
source(here("model", "R", "helpers-model.R"))

# select priors to be used HERE -------------------------------------------
active_config = "situation_specific_prior"
# active_config = "abstract_state_prior"

# Setup -------------------------------------------------------------------
Sys.setenv(R_CONFIG_ACTIVE = active_config)
params <- config::get()

if(!dir.exists(params$dir_results)) dir.create(params$dir_results, recursive = T)
if(!dir.exists(params$dir_model_input)) dir.create(params$dir_model_input, recursive = T)

## Retrieve tables
path_tbls = here(params$dir_model_input, params$fn_uniq_tables)
tables <- readRDS(path_tbls)
print(paste("tables read from:", path_tbls))

params$tables = tables %>% ungroup() %>%
  dplyr::select(bn_id, table_id, ps, vs, ll, cn)

# --------- Which states to compute predictions for --------- # 
bn_ids.mapping = tables %>% filter(added) %>%
  unnest(c(p_id, stimulus)) %>% group_by(p_id, bn_id) %>% 
  dplyr::select(table_id, bn_id, p_id, ll, stimulus, cn, best.cn)

params$bn_ids = bn_ids.mapping %>% distinct_at(vars(c(bn_id))) %>% pull(bn_id)
tbls.long = params$tables %>% unnest(c(vs,ps))
tbls.wide = tbls.long %>% pivot_wider(names_from = "vs", values_from="ps")

## Generate/Retrieve utterances
path_utterances = here(params$dir_model_input, params$fn_utterances)
if(params$generate_utterances || !file.exists(path_utterances)){
  utterances <- generate_utts(params, path_to_target = path_utterances)
} else {
  utterances <- readRDS(path_utterances)
  print(paste("utterances read from:", path_utterances))
}
params$utterances <- utterances

# Run Model ---------------------------------------------------------------
posterior <- run_webppl(params$model_path, params)

# restructure data and save
speaker <- posterior %>% structure_speaker_data(params) %>% group_by(bn_id)

path_predictions_by_context <- here(params$dir_results, 
                                    params$fn_predictions_by_context)
path_predictions_by_context.chunked <- here(params$dir_results, 
                                            params$fn_predictions_by_context_chunked)
results.model = model_prediction_by_context(speaker, 
                                            bn_ids.mapping, 
                                            path_predictions_by_context, 
                                            path_predictions_by_context.chunked)

tables.mapping = readRDS(here(params$dir_model_input, params$fn_tables_mapping))
path_joint_results = here(params$dir_results, params$fn_joint_results)
res.behav_model = join_model_behavioral(speaker, 
                                        bn_ids.mapping,
                                        tables.mapping, 
                                        params$path_cleaned_data,
                                        path_joint_results
                                        )





