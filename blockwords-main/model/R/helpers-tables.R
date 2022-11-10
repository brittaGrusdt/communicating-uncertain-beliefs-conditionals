library(truncnorm)
library(dplyr)
library(here)
source(here("model", "R", "helper-functions.R"))
fs = .Platform$file.sep

create_dependent_tables <- function(params, cns){
  all_tables <- list()
  idx <- 1
  if(!"n_tables" %in% names(params)){
    params$n_tables = tibble(cn=cns, n=params$n_dep_tables)
  }
  for(cn in cns){
    n = params$n_tables %>% filter(cn == (!! cn)) %>% pull(n)
    theta <- rbeta(n, params$beta_pos_a, params$beta_pos_b)
    beta <- rbeta(n, params$beta_neg_a, params$beta_neg_b)
    p_child_parent <- theta + beta * (1 - theta)
    p_child_neg_parent <- beta
    p_parent <- runif(n)
    
    if(cn %in% c("A implies C", "C implies A")){
      probs <- tibble(cond1=p_child_parent, cond2=p_child_neg_parent, marginal=p_parent)
    } else if(cn %in% c("A implies -C", "C implies -A")){
      probs <- tibble(cond1=1-p_child_parent, cond2=1-p_child_neg_parent, marginal=p_parent)
    }  
    # A -> C and -A -> C use the same probabilities (P(C|A), P(C|-A), P(A)/P(-A))
    if(startsWith(cn, "A")){
      probs <- probs %>% mutate(`AC`=cond1 * marginal,
                                `A-C`=(1-cond1) * marginal,
                                `-AC`=cond2 * (1-marginal),
                                `-A-C`=(1-cond2) * (1-marginal))
    } else if(startsWith(cn, "C")){
      # diagonals are switched
      probs <- probs %>% mutate(`AC`=cond1 * marginal,
                                `A-C`=cond2 * (1-marginal),
                                `-AC`=(1-cond1) * marginal,
                                `-A-C`=(1-cond2) * (1-marginal))
    } else {
      stop(paste(cn, "not implemented."))
    }
    tables <- probs %>% dplyr::select(-cond1, -cond2, -marginal) %>%
      rowid_to_column("id")
    tables_long <- tables %>%
      gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", val="val") #%>%
    #mutate(val=round(val, 4))
    tables_wide <- tables_long %>% group_by(id) %>%
      summarise(ps = list(val), .groups = 'drop') %>% add_column(cn=(!! cn)) %>%
      mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id)
    
    all_tables[[idx]] <- tables_wide
    idx <- idx + 1
  }
  tables <- all_tables %>% bind_rows()
  return(tables)
}

create_independent_tables <- function(params){
  tables <- tibble(pc=runif(params$n_ind_tables), pa=runif(params$n_ind_tables)) %>%
    rowid_to_column("id") %>%
    mutate(upper_bound = pmin(pa, pc),
           lower_bound = ifelse(1-(pa+pc) < 0, abs(1-(pa+pc)), 0),
           #noisy samples
           `AC`= rtruncnorm(1, a=lower_bound, b=upper_bound, mean=pa*pc, sd=params$indep_sigma), 
           `-AC`=pc-`AC`, `A-C`=pa-`AC`, s=`AC` + `-AC` + `A-C`, `-A-C`= 1 - s) %>%
    dplyr::select(-upper_bound, -lower_bound, -pa, -pc, -s)
  tables.mat = tables  %>% dplyr::select(-id) %>% as.matrix() 
  
  tables = prop.table(tables.mat, 1) %>% as_tibble() %>%
    mutate(n=AC + `A-C` + `-AC` + `-A-C`) %>%
    add_column(id=tables$id)
  
  tables_long <- tables %>%
    gather(`AC`, `A-C`, `-AC`, `-A-C`, key="cell", val="val") %>%
    filter(val != 0)
  tables_wide <- tables_long %>% group_by(id) %>%
    summarise(ps = list(val), .groups = 'drop') %>% add_column(cn="A || C") %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% dplyr::select(-id) 
  return(tables_wide)
}

create_tables <- function(params, use_seed=TRUE){
  if(use_seed) set.seed(params$seed_tables)
  cns_dep=params$cns[params$cns != "A || C"]
  tables_all <- list()
  tables_ind <- create_independent_tables(params)
  tables_dep <- create_dependent_tables(params, cns_dep)
  tables <- bind_rows(tables_ind, tables_dep) %>% rowid_to_column("table_id")
  if(use_seed) tables = tables %>% mutate(seed=params$seed_tables)
  return(tables)
}

# @arg tables: one column per loglikelihood
tables_to_bns = function(tables, params){
  tables.ll = tables %>% group_by(table_id) %>%
    pivot_longer(cols=starts_with("logL_"), names_to="ll_cn", values_to="ll")
  tbls = tables.ll  %>%
    mutate(cn=case_when(ll_cn=="logL_ind" ~ "A || C",
                        ll_cn=="logL_if_ac" ~ "A implies C",
                        ll_cn=="logL_if_ca" ~ "C implies A",
                        ll_cn=="logL_if_anc" ~ "A implies -C",
                        ll_cn=="logL_if_cna" ~ "C implies -A")) %>%
    dplyr::select(-ll_cn, -ind.lower, -ind.upper) %>% 
    group_by(table_id) %>% mutate(best.cn=ll==max(ll)) %>%
    arrange(desc(ll)) %>% mutate(rank=seq(1:n()))
  return(tbls)
}

sampleModelTables = function(){
  params <- configure(c("abstract_tables"))
  params$save=FALSE
  tables.model <- create_tables(params)
  return(list(tables=tables.model, params=params))
}

