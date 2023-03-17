# Function definitions ----------------------------------------------------
# adds empirical tables to set of all generated tables (tables.gen)
#' @import dplyr
#' @import tibble
add_empirical_tables = function(tables.gen, path_empiric_tbl_ids, path_mapping=NA){
  max.table_id = tables.gen$table_id %>% max()
  tbls.emp = readRDS(path_empiric_tbl_ids) %>%
    rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none) %>%
    dplyr::select(-ends_with(".round")) %>%
    rowid_to_column("table_id") %>% group_by(table_id) %>%
    mutate(table_id = table_id + max.table_id, added=TRUE) %>%
    group_by(table_id) %>%
    unnest_longer(c(p_id)) %>%
    separate("p_id", into=c("prolific_id", "rel", "prior"), sep="_") %>%
    unite("id", "rel", "prior")

  tables.map = tbls.emp %>% ungroup() %>%
    dplyr::select(prolific_id, id, table_id, empirical_id,
                  AC, `A-C`, `-AC`, `-A-C`) %>%
    rename(stimulus=id)
  if(!is.na(path_mapping)) save_data(tables.map, path_mapping)

  tables.empiric = tbls.emp %>% mutate(stimulus=id) %>%
    unite("p_id", "prolific_id", "id", sep="_") %>%
    mutate(empirical_id = list(empirical_id), p_id = list(p_id),
           stimulus = list(stimulus)) %>%
    distinct()

  tbls.joint = bind_rows(tables.gen %>% add_column(added=FALSE), tables.empiric)
  return(tbls.joint)
}

# ** computes log likelihood for independent net and  A->C / C->A for dep nets**
#' @import dplyr
#' @import tibble
likelihood_tables <- function(df_wide, sigma_indep, a=10, b=1){
  # prepare
  df <- df_wide %>%
    compute_cond_prob("P(C|A)") %>% rename(p_c_given_a=p) %>%
    compute_cond_prob("P(C|-A)") %>% rename(p_c_given_na=p) %>%
    compute_cond_prob("P(A|C)") %>% rename(p_a_given_c=p) %>%
    compute_cond_prob("P(A|-C)") %>% rename(p_a_given_nc=p) %>%
    mutate(pa=AC+`A-C`, pc=AC+`-AC`,
           ind.lower=case_when(1-(pa+pc) < 0 ~ abs(1-(pa+pc)),
                               TRUE ~ 0),
           ind.upper=pmin(pa, pc))
  df <- df %>%
    mutate(
      p_nc_given_a = 1 - p_c_given_a,
      p_na_given_c = 1 - p_a_given_c,
      p_nc_given_na = 1 - p_c_given_na,
      p_na_given_nc = 1 - p_a_given_nc,

      logL_ind=log(truncnorm::dtruncnorm(x=`AC`, a=ind.lower, b=ind.upper,
                                         mean=pa*pc, sd=sigma_indep)),
      logL_if_ac = log(dbeta(p_c_given_a, a, b))+log(dbeta(p_c_given_na, b, a)),
      logL_if_anc = log(dbeta(p_nc_given_a, a, b)) + log(dbeta(p_nc_given_na, b, a)),
      logL_if_ca = log(dbeta(p_a_given_c, a, b)) + log(dbeta(p_a_given_nc, b, a)),
      logL_if_cna = log(dbeta(p_na_given_c, a, b)) + log(dbeta(p_na_given_nc, b, a))
    ) %>%
    dplyr::select(-p_c_given_na, -p_c_given_a, -p_a_given_c, -p_a_given_nc, -pa, -pc,
                  -p_nc_given_a, -p_na_given_c, -p_nc_given_na, -p_na_given_nc)
  return(df)
}

#' @import dplyr
#' @import tibble
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

#' @import dplyr
#' @import tibble
#' @importFrom truncnorm rtruncnorm
create_independent_tables <- function(params){
  tables <- tibble(pc=runif(params$n_ind_tables), pa=runif(params$n_ind_tables)) %>%
    rowid_to_column("id") %>%
    mutate(upper_bound = pmin(pa, pc),
           lower_bound = ifelse(1-(pa+pc) < 0, abs(1-(pa+pc)), 0),
           #noisy samples
           `AC`= truncnorm::rtruncnorm(1, a=lower_bound, b=upper_bound, mean=pa*pc, sd=params$indep_sigma),
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

#' @import dplyr
#' @import tibble
create_tables <- function(params, use_seed = TRUE){
  if(use_seed) set.seed(params$seed_tables)
  cns_dep = params$cns[params$cns != "A || C"]
  tables_all <- list()
  tables_ind <- create_independent_tables(params)
  tables_dep <- create_dependent_tables(params, cns_dep)
  tables <- bind_rows(tables_ind, tables_dep) %>% rowid_to_column("table_id")
  if(use_seed) tables = tables %>% mutate(seed = params$seed_tables)
  return(tables)
}

#' adds log likelihood for each table and each of five causal nets
#' @param tables tibble with one column per loglikelihood
#' @param params list
#' @import dplyr
#' @import tibble
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

#' generates states for abstract prior
#' @import dplyr
#' @import tibble
makeAbstractPriorTables = function(path_empiric_tbls_ids) {
  Sys.setenv(R_CONFIG_ACTIVE = "abstract_state_prior")
  tables.par <- config::get()
  tables.generated <- create_tables(tables.par) %>%
    unnest(c(vs, ps)) %>%
    pivot_wider(names_from="vs", values_from="ps") %>%
    mutate(AC.round=as.integer(round(AC, 2) * 100),
           `A-C.round`=as.integer(round(`A-C`,2) * 100),
           `-AC.round`=as.integer(round(`-AC`, 2) * 100),
           `-A-C.round`=as.integer(round(`-A-C`, 2) * 100))
  tables.generated = tables.generated %>% group_by(table_id)
  # for each sampled table add which empirical tables match
  tables.model = match_sampled_and_empiric_tables(tables.generated,
                                                  path_empiric_tbls_ids)
  # add all distinct exact empirical tables to generated set of tables
  path_mapping = here(tables.par$dir_model_input, tables.par$fn_tables_mapping)
  tables.with_empirical = add_empirical_tables(tables.model,
                                               path_empiric_tbls_ids,
                                               path_mapping)

  # combine each generated/empirical table with each causal net
  # filter out those with -Infinity log likelihood
  tables <- tables.with_empirical %>% likelihood_tables(tables.par$indep_sigma) %>%
    rename(cn.orig=cn)
  bns = tables_to_bns(tables, tables.par) %>%
    mutate(bn_id=case_when(
      cn=="A || C" ~ paste(table_id, "independent", sep="_"),
      TRUE ~ paste(table_id, str_replace_all(cn, " ", ""), sep="_")
    )) %>% group_by(bn_id) %>%
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
           ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>%
    dplyr::select(-`AC`, -`A-C`, -`-AC`, -`-A-C`) %>%
    add_column(indep_sigma=tables.par$indep_sigma)

  bns.finite_ll = bns  %>% filter(!is.infinite(ll))

  # save_data(bns.finite_ll %>% filter(!added), here(tables.par$dir_model_input, tables.par$fn_tables))
  save_data(tables.par, here(tables.par$dir_model_input, tables.par$fn_params_tables))
  # save_data(bns.finite_ll, here(tables.par$dir_model_input, tables.par$fn_tables_with_empiric))

  # unique tables marginal P(tables) computed across cns with logsumexp
  tables = group_map(bns %>% group_by(table_id), function(table, table_id){
    table_id=table_id$table_id
    tbls.dep = table %>% filter(!endsWith(bn_id, "independent"))
    p_dep = (1/8) * exp(matrixStats::logSumExp(tbls.dep$ll))
    p_ind = (1/2) *
      exp(table %>% filter(endsWith(bn_id, "independent")) %>% pull(ll))
    ll = log(p_dep + p_ind)
    # ll=matrixStats::logSumExp(table$ll)
    df = table %>% add_column(table_id=(!! table_id))
    return(df[1,] %>% mutate(ll=(!!ll)))
  }) %>% bind_rows() %>% group_by(table_id) %>% mutate(cn="", best.cn=TRUE)
  save_data(tables, here(tables.par$dir_model_input, tables.par$fn_uniq_tables))
  return(tables)
}

#' matches empirical_ids with table_ids from sampled tables and saves mapping
#' and brings result into format for webppl model
#' @param tables.generated tables uniquely grouped by table_id
#' (before combining with causal nets)
#' @import dplyr
#' @import tibble
#' @import ExpDataWrangling
match_sampled_and_empiric_tables = function(tables.generated,
                                            path_empiric_tbl_ids) {
  tables.empiric.pids = readRDS(path_empiric_tbl_ids) %>%
    rename(AC = bg, `A-C` = b, `-AC` = g, `-A-C` = none,
           AC.round = `bg.round`, `A-C.round` = `b.round`,
           `-AC.round` = `g.round`, `-A-C.round` = `none.round`) %>%
    ExpDataWrangling::bin_tables()
  tables.emp = tables.empiric.pids %>% unnest(c(p_id)) %>%
    mutate(p_id.copy=p_id) %>%
    separate(p_id.copy, into=c("pid", "rel", "prior"), sep="_") %>%
    unite("stimulus", "rel", "prior", sep="_") %>%
    dplyr::select(ends_with(".binned"), empirical_id, stimulus, p_id) %>%
    group_by(`AC.binned`, `A-C.binned`, `-AC.binned`, `-A-C.binned`)

  tables.emp$bin_id = tables.emp %>% group_indices()
  tables.emp.binned = tables.emp %>%
    mutate(empirical_id = list(empirical_id), stimulus = list(stimulus),
           p_id = list(p_id)) %>% distinct()
  tables.gen.binned = tables.generated %>% ExpDataWrangling::bin_tables()

  tbls.joint = left_join(
    tables.gen.binned, tables.emp.binned,
    by=c("AC.binned", "A-C.binned", "-AC.binned", "-A-C.binned")
  ) %>% group_by(table_id) %>%
    mutate(match.empirical = !is.null(empirical_id[[1]])) %>%
    dplyr::select(-ends_with(".round"), -ends_with("binned"), -bin_id)

  return(tbls.joint)
}



# Extended-Analysis -------------------------------------------------------
#' creates names of all causal nets distinguishing between low/high/unc for
#' causal power, noise and marginal probabilities.
#' only includes cases where causal power and noise are not identical!
#' order within returned string: 1.p_ant 2.causal power 3.noise
#'
#' @param p_noise char vector at least one of: "high", "low", "unc"
#' @param p_cp char vector at least one of: "high", "low", "unc"
#' @param p_ant char vector at least one of: "high", "low", "unc"
#' @param rels_dep char vector with names of dep. causal nets (e.g., A implies C)
#' @param p_a char vector at least one of: "high", "low", "unc"
#' @param p_c at least one of: "high", "low", "unc"
#' @import dplyr
#' @import tibble
create_causal_nets = function(p_noise, p_cp, p_ant, rels_dep, p_a, p_c){
  cns.dep = expand.grid(p_noise, p_cp, p_ant, rels_dep)  %>% as_tibble() %>%
    rename(noise = Var1, causal_power = Var2, antecedent = Var3, r = Var4) %>%
    mutate(noise = as.character(noise),
           causal_power = as.character(causal_power),
           antecedent = as.character(antecedent),
           r = as.character(r)) %>%
    filter(causal_power != noise) %>%
    unite("probs", antecedent, causal_power, noise, sep = "-") %>%
    unite("cn", r, probs, sep = "_") %>% pull(cn)

  cns.ind = expand.grid(p_a, p_c) %>% as_tibble() %>%
      rename(pa = Var1, pc = Var2) %>%
      unite("probs", pa, pc, sep="-") %>%
      mutate(cn = paste("A || C", probs, sep = "_")) %>% pull(cn)
  return(list(dep = cns.dep, ind = cns.ind))
}










