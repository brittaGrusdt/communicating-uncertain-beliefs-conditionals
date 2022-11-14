library(tidyverse)
library(greta)
library(here)
library(MCMCpack)
library(rwebppl)
library(matrixStats)

FS = .Platform$file.sep
# SEED_FITTED_TABLES = "20202020"


# Fit dirichlet distributions ---------------------------------------------
get_optimal_alphas <- function(table_data, st_id) {
  if(st_id != ""){
    y <- table_data %>% filter(id == st_id)
  } else {
    y <- table_data
  }
  y <- y[, c("bg", "b", "g", "none")] %>% as.matrix()
  y <- prop.table(y + EPSILON, 1)
  
  alpha <- uniform(0, 20, 4)
  distribution(y) <- greta::dirichlet(t(alpha), n_realisations = nrow(y))
  m <- model(alpha)
  fit_opt <- opt(m)
  
  tibble(
    id = st_id,
    alpha_1 = fit_opt$par$alpha[1],
    alpha_2 = fit_opt$par$alpha[2],
    alpha_3 = fit_opt$par$alpha[3],
    alpha_4 = fit_opt$par$alpha[4],
    )
}

sample_dirichlet <- function(params, seed, n=1000){
  set.seed(seed)
  tables = map_dfr(seq(1, nrow(params)), function(i){
     row = params[i,]
     rdirichlet(n, row[1, 2:5] %>% as.numeric()) %>% as_tibble() %>%
       add_column(context=row$id)
    }) %>% rename(bg=V1, b=V2, g=V3, none=V4)
  return(tables)
}

makeDirichletTables = function(df.params.fit, path_empiric_tbl_ids) {
  Sys.setenv(R_CONFIG_ACTIVE = "situation_specific_prior")
  par <- config::get()
  tables.dirichlet = sample_dirichlet(df.params.fit, par$seed_fitted_tables)
  tables.generated =  tables.dirichlet %>% 
    rename(`AC`=bg, `A-C`=b, `-AC`=g, `-A-C`=none) %>%
    mutate(`AC.round`=as.integer(round(AC, 2) * 100),
           `A-C.round`=as.integer(round(`A-C`, 2) * 100),
           `-AC.round`=as.integer(round(`-AC`, 2) * 100),
           `-A-C.round`=as.integer(round(`-A-C`, 2) * 100)) %>%
    group_by(`AC`, `A-C`, `-AC`, `-A-C`)
  tables.generated$table_id = tables.generated %>% group_indices() 
  
  path_empiric_tbls_ids = par$fn_tbls_empiric_pids
  tables.model = match_sampled_and_empiric_tables(tables.generated, 
                                                  path_empiric_tbl_ids)
  path_table_mappings = here(par$dir_model_input, par$fn_tables_mapping)
  tables.with_empirical = add_empirical_tables(tables.model,
                                               path_empiric_tbl_ids,
                                               path_table_mappings)

  # for each table add ll for each context
  stimuli = tables.dirichlet$context %>% unique
  tables.ll = map_dfc(seq(1, length(stimuli)), function(i){
    stimulus = stimuli[[i]]
    par = df.params.fit %>% filter(id == (!! stimulus))
    df.tbls = tables.with_empirical %>% ungroup() %>%
      dplyr::select(AC, `A-C`, `-AC`, `-A-C`)
    ll = add_ll_dirichlet(df.tbls, par)
    names(ll)[names(ll) == "ll.table"] <- paste("ll", stimulus, sep=".")
    return(ll)
  })
  tables.ll.long = bind_cols(tables.with_empirical, tables.ll) %>%
    rowid_to_column() %>% group_by(rowid) %>% 
    pivot_longer(cols = starts_with("ll."), names_to = "ll_stimulus", values_to="ll")
  
  # just sampled tables with respective ll
  tables = tables.ll.long %>%
    mutate(ll_stimulus = str_replace(ll_stimulus, "ll.", "")) %>%
    filter(context == ll_stimulus) %>% rename(cn = ll_stimulus) %>%
    mutate(bn_id = paste(table_id, context, sep="_")) %>% 
    ungroup() %>% dplyr::select(-rowid) %>% group_by(bn_id) %>%
    mutate(vs = list(c("AC", "A-C", "-AC", "-A-C")),
           ps = list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
    dplyr::select(-`AC`, -`A-C`, -`-AC`, -`-A-C`)
  
  dir_model_data <- par$dir_model_input
  # tables %>% save_data(here(dir_model_data, str_replace(par$tables_path, ".rds", "-no-bns.rds")))
  
  # combine each sampled table with each stimulus
  bns = tables.ll.long %>% combine_tables_and_contexts() %>%
    ungroup() %>% dplyr::select(-rowid) %>%
    mutate(bn_id = paste(table_id, cn, sep="_")) %>% group_by(bn_id) %>%
    mutate(vs = list(c("AC", "A-C", "-AC", "-A-C")),
           ps = list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
    dplyr::select(-`AC`, -`A-C`, -`-AC`, -`-A-C`)
  
  bns %>% filter(!added) %>% save_data(here(dir_model_data, par$fn_tables))
  bns %>% save_data(here(dir_model_data, par$fn_tables_with_empiric))
  
  # unique tables marginal P(tables) computed across cns with logsumexp
  tables = group_map(bns %>% group_by(table_id), function(table, table_id){
    table_id=table_id$table_id
    ll = logSumExp(table$ll) + log(1/13)
    df = table %>% add_column(table_id=(!! table_id))
    return(df[1,] %>% mutate(ll=(!!ll)) %>% dplyr::select(-cn, -best.cn))
  }) %>% bind_rows() %>% group_by(table_id) %>% 
    mutate(cn = "", best.cn=TRUE)
  
  save_data(tables, here(par$dir_model_input, par$fn_uniq_tables))
  
  return(bns)
}

combine_tables_and_contexts = function(tbls.ll){
  tbls = tbls.ll  %>% 
    separate("ll_stimulus", into=c("tmp", "cn"), sep="\\.") %>%
    dplyr::select(-tmp) %>%
    group_by(table_id) %>% mutate(best.cn = ll == max(ll)) %>% 
    arrange(desc(ll)) %>% mutate(rank = seq(1:n()))
  return(tbls)
}


# Log likelihoods ---------------------------------------------------------
# adds log likelihood for each table for stimulus it was generated for 
# with fitted params (params)
add_ll_dirichlet = function(tables, params) {
  tables.mat = tables %>% as.matrix()
  # iterate over all causal nets (if several)
  likelihoods = map_dfr(seq(1, nrow(params)), function(i){
    par = params[i,] %>% ungroup()
    vec = rep(par %>% dplyr::select(-id, -p_cn, -cn) %>% as.matrix(), nrow(tables.mat))
    par.mat = matrix(vec, nrow(tables.mat), 4, byrow = TRUE)
    densities = MCMCpack::ddirichlet(tables.mat, par.mat)
    df = tables %>% add_column(lik=densities * par$p_cn) %>% rowid_to_column() 
  })
  log_likelihoods = likelihoods %>% group_by(rowid) %>%
    summarize(ll.table=log(sum(lik)), .groups="drop_last") %>%
    dplyr::select(-rowid)
  
  return(log_likelihoods)
}

# compute log likelihood for set of all tables (considering different cns)
# @arg tables: must be smoothed, with only 4 columns: bg,b,g,none
# @arg params: columns: a, b, c, d, id, p_cn, cn
ll_dirichlet = function(tables, params){
  likelihoods = add_ll_dirichlet(tables, params)
  df = likelihoods %>% summarize(ll=sum(ll.table))
  return(df)
}

# fit single dirichlet distribution for each stimulus (or all) ----------------
run_fit_dirichlet = function(result_dir, per_stimulus){
  path_smoothed_tables <- paste(result_dir, "pe_tables_smooth.csv", sep = FS)
  table_data <- read_csv(path_smoothed_tables) %>% arrange(id)
  message(paste('load data for fitting dirichlets from', path_smoothed_tables))
  
  stimulus_id_list <- table_data %>% filter(id != "ind2") %>% pull(id) %>% unique()
  if(per_stimulus){
    save_to = "params-fitted-dirichlet.csv"
    params.fit <- map_df(
      stimulus_id_list,
      function(s) {
        print(s)
        get_optimal_alphas(table_data, st_id = s)
      }
    )
  } else {
    save_to <- "params-fitted-single-dirichlet.csv"
    params.fit <- get_optimal_alphas(table_data, "")
  }
  fn = paste(result_dir, save_to, sep=FS)
  write_csv(params.fit, fn)
  message(paste("saved fitted params to to", fn))
  return(params.fit)
}

# Goodness fits -----------------------------------------------------------
# considers different causal nets, i.e. more than 1 dirichlet per stimulus
ll_dirichlet_empirical_data = function(params, df.pe_task){
  tbls.empiric <- df.pe_task %>% dplyr::select(-utt.standardized) %>% 
      pivot_wider(names_from = "slider", values_from = "pe_task.smooth")
  stimuli_ids = tbls.empiric %>% pull(id) %>% unique()
  # only exact empirical data ll 
  if(params[1,]$id == "all"){
    id = "all"
    ll = tbls.empiric %>% ungroup() %>% 
      dplyr::select(b, g, bg, none) %>% ll_dirichlet(params)
    name_ll = paste("ll", id, sep="__")
    ll.empirical = tibble(stimulus_id=id, ll_sample=ll$ll)
  } else {
    # separately for each stimulus
    ll.empirical = map_dfr(stimuli_ids, function(id){
      par = params %>% filter(id == (!! id))
      ll = tbls.empiric %>% filter(id == (!!id)) %>% ungroup() %>% 
        dplyr::select(b, g, bg, none) %>% ll_dirichlet(par)
      name_ll = paste("ll", id, sep="__")
      tibble(stimulus_id = id, ll_sample = ll$ll)
    });
  }
  return(ll.empirical)
}

# @arg params: with columns: id, alpha_1, alpha_2, alpha_3, alpha_4, p_cn, cn
# @arg df.N: tibble with cols id, n. For each id gives number of participants
goodness_fits_dirichlet = function(params, df.pe_task, n, df.N){
  ll.empirical = ll_dirichlet_empirical_data(params, df.pe_task)
  # Sample n times N=#participants values for each stimulus
  # (each has a different fitted dirichlet distribution)
  ll_samples = group_map(params %>% group_by(id), function(par, stimulus){
    id = stimulus$id
    print(id)
    ll.obs = ll.empirical %>% filter(stimulus_id == (!! id)) %>% pull(ll_sample)
    par.vec = par[, c("alpha_1", "alpha_2", "alpha_3", "alpha_4")] %>% as.numeric()
    # sample n times N (N: #participants) tables and compute log likelihood
    ll.simulated = map_dfr(seq(1, n), function(i){
      N_stim = df.N %>% filter(id == (!! id)) %>% pull(n)
      # generated set of tables
      tables.chunk = rdirichlet(N_stim, par.vec) %>% as_tibble() %>%
        add_ll_dirichlet(par %>% add_column(id=(!! id)))
      return(tables.chunk %>% add_column(idx_rep = i))
    })
    return(ll.simulated %>% mutate(stimulus_id = id))
  });
  ll.per_sample_and_id = ll_samples %>% bind_rows() %>%
    group_by(stimulus_id, idx_rep) %>%
    summarize(ll_sample = sum(ll.table), .groups="drop_last")

  dat = left_join(ll.per_sample_and_id,
                  ll.empirical %>% rename(ll.obs=ll_sample)
                  ) %>%
    mutate(sample_worse = ll_sample < ll.obs) %>% add_column(n = n)
  p.val = dat %>% group_by(stimulus_id) %>% summarize(p.val = mean(sample_worse))
  return(left_join(dat, p.val))
}

# use id="all" if not per stimulus (each=F)
compute_goodness_dirichlets = function(params, df.pe_task, df.N, 
                                       path_target_csv=NA, n=10**3){
  res.goodness = goodness_fits_dirichlet(params, df.pe_task, n, df.N) %>% 
    arrange(desc(p.val));
  p.vals = res.goodness %>% dplyr::select(-ll_sample, -sample_worse, -idx_rep) %>%
    distinct_at(vars(c(stimulus_id)), .keep_all = T) %>% 
    mutate(p.val = round(p.val, 3))
  if(!is.na(path_target_csv)) {
    write_csv(p.vals, path_target_csv)
    print(paste("wrote simulated p-values to:", path_target_csv))
  }
  return(res.goodness)
}

plot_goodness_dirichlets = function(params.fit, res.goodness, df.pe_task, 
                                    path_target=NA){
  ll.obs = ll_dirichlet_empirical_data(params.fit, df.pe_task)
  p = res.goodness %>% ggplot(aes(x = ll_sample)) +
    geom_density() +
    geom_point(data = ll.obs, aes(x = ll_sample, y = 0), color = 'red', size = 2) +
    geom_vline(data=ll.obs, aes(xintercept = ll_sample)) +
    facet_wrap(~stimulus_id) +
    theme_classic() +
    labs(x="log-likelihood simulated data")
  
  if(!is.na(path_target)) {
    ggsave(path_target, p, height=6)
    print(paste("saved plot to:", path_target))
  }
  return(p)
}


