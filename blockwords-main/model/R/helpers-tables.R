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

plot_tables <- function(data){
  # data must be in long format with columns *cell* and *val*
  cns <- data$cn %>% as.factor() %>% levels()
  plots <- list(); idx = 1
  for(causal_net in cns){
    if(causal_net == "A || C"){
      cn_title <- "A,C indep."
    } else {
      cn_title = ifelse(endsWith(causal_net, "-A"), TeX("$C\\rightarrow\\neg A$"),
                        ifelse(endsWith(causal_net, "-C"), TeX("$A\\rightarrow\\neg C$"),
                               parse(text=str_replace(causal_net, " implies ", '%->%'))))
    }
    ylab = ifelse(idx %in% c(1,4), "density", "");
    xlab = ifelse(idx %in% c(3,4,5), "probability", "");
    
    p <- data %>% 
      filter(cn==causal_net) %>%
      ggplot(aes(x=val,  color = cell)) +
      geom_density() +
      facet_wrap(~cell, ncol = 2, scales = "free",
                 labeller = labeller(cell = c(`AC` = "P(A,C)", `A-C` = "P(A,¬C)",
                                              `-AC`= "P(¬A,C)", `-A-C` = "P(¬A,¬C)"))
      ) +
      labs(title = cn_title, x=xlab, y=ylab) +
      theme_classic(base_size = 20) +
      theme(legend.position = "none", axis.text.x = element_text(size=10))
    plots[[idx]] <- p
    idx <- idx + 1
    print(p)
  }
  return(plots)
}

plot_tables_all_cns <- function(tables_path, plot_dir, w, h){
  tables.wide <- readRDS(tables_path) %>% unnest_tables() %>%
    rename(bn_id=rowid) %>% group_by(bn_id, cn) %>% 
    pivot_wider(names_from = cell, values_from = val) %>% ungroup()
  tables.long <- tables.wide %>% 
    mutate(`-A-C` = case_when(is.na(`-A-C`) ~ rowSums(dplyr::select(., starts_with("-A-C_"))),
                              TRUE ~ `-A-C`),
           `-AC` = case_when(is.na(`-AC`) ~ rowSums(dplyr::select(., starts_with("-AC_"))),
                             TRUE ~ `-AC`), 
           `A-C` = case_when(is.na(`A-C`) ~ rowSums(dplyr::select(., starts_with("A-C_"))),
                             TRUE ~ `A-C`),
           `AC` = case_when(is.na(`AC`) ~ rowSums(dplyr::select(., starts_with("AC_"))),
                            TRUE ~ `AC`)) %>% 
    group_by(bn_id, cn) %>%
    pivot_longer(cols = c(AC, `A-C`, `-AC`, `-A-C`), names_to = "cell", values_to = "val") %>% 
    ungroup() %>% 
    mutate(cell=factor(cell, levels=c("AC", "A-C", "-AC", "-A-C")),
           cn=case_when(cn=="A || C" ~ "A,C independent",
                        TRUE ~ cn),
           cn=as.factor(cn)) %>% 
    group_by(bn_id, cn)
  
  # tables must be in long format with columns *cell* and *val*
  all_plots = list()
  cns <- list(c("A,C independent"), c("A implies -C", "C implies -A"), c("A implies C", "C implies A"))
  cns.short <- c("indep", "anc-cna", "ac-ca")
  for(i in seq(1,3)) {
    p <- tables.long %>% filter(cn %in% cns[[i]]) %>%
      ggplot(aes(x=val,  fill = cn)) +
      geom_density(alpha=0.5) +
      facet_wrap(~cell, ncol = 2, scales = "free",
                 labeller = labeller(cell = c(`AC` = "P(A,C)", `A-C` = "P(A,¬C)",
                                              `-AC`= "P(¬A,C)", `-A-C` = "P(¬A,¬C)"))
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      labs(x="probability", y="density") +
      theme_classic(base_size = 20) +
      theme(legend.position = "bottom")
    all_plots[[i]] = p
    
    save_to = paste(plot_dir, paste("tables-", cns.short[[i]], ".png", sep=""), sep=SEP)
    ggsave(save_to, p, width=w, height=h)
    print(paste('saved to', save_to))
  }
  return(all_plots)
}

# Analyze generated Tables ------------------------------------------------
#@arg TABLES: in long format, with cells vs, ps unnested
analyze_tables <- function(path, theta, TABLES=tibble()){
  if(TABLES %>% nrow() == 0) {
    TABLES <- read_rds(path) %>%
      dplyr::select(table_id, cn, vs, ps)  %>% 
      rowid_to_column() %>% group_by(rowid) %>%
      unnest(c(ps, vs))
  }    
  TABLES.wide <-  TABLES %>% pivot_wider(names_from = vs, values_from = ps)  
  n.wide = nrow(TABLES.wide)
  n.long = nrow(TABLES)
  # conjunctions
  print('#true conjunctions')
  conj <- TABLES %>% mutate(conj=case_when(ps >= theta ~ T, T ~ F)) %>% 
    group_by(vs, bn_id) %>%
    summarize(conj=sum(conj)>0, .groups="drop_last") %>% 
    summarize(n.conj=sum(conj), .groups="drop") %>% 
    mutate(ratio=round(n.conj/n.wide, 3),
           ratio.total=round(sum(n.conj)/n.long, 3))
  print(conj)
  
  print('#true ifs')
  conditionals <- TABLES.wide %>%
    add_column(pca=compute_cond_prob(TABLES.wide, "P(C|A)") %>% pull(p) > theta,
               pac=compute_cond_prob(TABLES.wide, "P(A|C)") %>% pull(p) > theta,
               pcna = compute_cond_prob(TABLES.wide, "P(C|-A)") %>% pull(p) > theta,
               panc = compute_cond_prob(TABLES.wide, "P(A|-C)") %>% pull(p) > theta
    )
  df.ifac = conditionals %>% filter(pca) %>% nrow()
  df.ifca = conditionals %>% filter(pac) %>% nrow()
  df.ifnac = conditionals %>% filter(pcna) %>% nrow()
  df.ifncna = conditionals %>% filter(panc) %>% nrow()
  print(paste("P(C|A)", df.ifac, "(", round(df.ifac / n.wide, 2), ")"))
  print(paste("P(A|C)", df.ifca, "(", round(df.ifca / n.wide, 2), ")"))
  print(paste("P(C|-A)", df.ifnac, "(", round(df.ifnac / n.wide, 2), ")"))
  print(paste("P(-A|-C)", df.ifncna, "(", round(df.ifncna / n.wide, 2), ")"))
  
  print('#true likely+literal')
  literals <- TABLES.wide %>%
    mutate(a=`AC` + `A-C` > 0.25,
           c=`AC` + `-AC` > 0.25,
           na=`-AC` + `-A-C` > 0.25,
           nc=`A-C` + `-A-C` > 0.25)
  print(paste("likely A", round(literals %>% filter(a) %>% nrow / n.wide, 2)))
  print(paste("likely C", round(literals %>% filter(c) %>% nrow / n.wide, 2)))
  print(paste("likely -A", round(literals %>% filter(na) %>% nrow / n.wide, 2)))
  print(paste("likely -C", round(literals %>% filter(nc) %>% nrow / n.wide, 2)))
  
  print('#true literals')
  literals <- TABLES.wide %>%
    mutate(a=`AC` + `A-C` > theta,
           c=`AC` + `-AC` > theta,
           na=`-AC` + `-A-C` > theta,
           nc=`A-C` + `-A-C` > theta)
  df.a = literals %>% filter(a) %>% nrow()
  df.c = literals %>% filter(c) %>% nrow()
  df.na = literals %>% filter(na) %>% nrow()
  df.nc = literals %>% filter(nc) %>% nrow()
  print(paste("A", df.a, "(", round(df.a / n.wide, 2), ")"))
  print(paste("C", df.c, "(", round(df.c / n.wide, 2), ")"))
  print(paste("-A", df.na, "(", round(df.na / n.wide, 2), ")"))
  print(paste("-C", df.nc, "(", round(df.nc / n.wide, 2), ")"))
}

# associate tables with stimuli of experiment
tables_to_stimuli <- function(tables.all.wide, t=0.8){
  tables <- tables.all.wide %>%
    mutate(pa=`AC` + `A-C`, pc=`AC`+`-AC`) %>%
    compute_cond_prob("P(C|A)") %>% rename(pc_given_a=p) %>%
    compute_cond_prob("P(C|-A)") %>% rename(pc_given_na=p) %>%
    mutate(stimulus_id=case_when(
      str_detect(stimulus_id, "independent") & pa>=t & pc>=t ~ paste(stimulus_id, "hh", sep="_"),
      str_detect(stimulus_id, "independent") & pa>=t & pc<=1-t ~ paste(stimulus_id, "hl", sep="_"),
      str_detect(stimulus_id, "independent") & pa>=t & pc>=0.4 & pc<=0.6 ~ paste(stimulus_id, "hu", sep="_"), 
      
      str_detect(stimulus_id, "independent") & pa<=1-t & pc>=t ~ paste(stimulus_id, "lh", sep="_"),
      str_detect(stimulus_id, "independent") & pa<=1-t & pc<=1-t ~ paste(stimulus_id, "ll", sep="_"),
      str_detect(stimulus_id, "independent") & pa<=1-t & pc>=0.4 & pc<=0.6 ~ paste(stimulus_id, "lu", sep="_"),
      
      str_detect(stimulus_id, "independent") & pa>=0.4 & pa<=0.6 & pc>=t ~ paste(stimulus_id, "uh", sep="_"),
      str_detect(stimulus_id, "independent") & pa>=0.4 & pa<=0.6 & pc<=1-t ~ paste(stimulus_id, "ul", sep="_"),
      str_detect(stimulus_id, "independent") & pa>=0.4 & pa<=0.6 & pc>=0.4 & pc<=0.6 ~ paste(stimulus_id, "uu", sep="_"),
      TRUE ~ stimulus_id));
  
  tables <- tables %>%
    mutate(stimulus_id= case_when(str_detect(stimulus_id, "if") & pc_given_na <= 0.05 ~ "if1",
                                  TRUE ~ stimulus_id));
  df <- tables %>% 
    mutate(stimulus_id=case_when(
      str_detect(stimulus_id, "if") & pc_given_a >=t & pa >= t ~ paste(stimulus_id, "hh", sep="_"),
      str_detect(stimulus_id, "if") & pc_given_a >=t & pa<=1-t ~ paste(stimulus_id, "lh", sep="_"),
      str_detect(stimulus_id, "if") & pc_given_a >=t & pa >=0.4 & pa<=0.6 ~ paste(stimulus_id, "uh", sep="_"),
      TRUE ~ stimulus_id)) %>%
    mutate(stimulus_id=case_when(
      str_detect(stimulus_id, "if1") & pc_given_a <=1-t & pa >= t ~ paste(stimulus_id, "hl", sep="_"),
      str_detect(stimulus_id, "if1") & pc_given_a <=1-t & pa <= 1-t ~ paste(stimulus_id, "ll", sep="_"),
      str_detect(stimulus_id, "if1") & pc_given_a <=1-t & pa>=0.4 & pa <= 0.6 ~ paste(stimulus_id, "ul", sep="_"),
      
      str_detect(stimulus_id, "if1") & (pc_given_a >=0.4 & pc_given_a <=0.6) & pa >= t ~ paste(stimulus_id, "hu", sep="_"),
      str_detect(stimulus_id, "if1") & (pc_given_a >=0.4 & pc_given_a <=0.6) & pa <= 1-t ~ paste(stimulus_id, "lu", sep="_"),
      str_detect(stimulus_id, "if1") & (pc_given_a >=0.4 & pc_given_a <=0.6) & pa>=0.4 & pa <= 0.6 ~ paste(stimulus_id, "uu", sep="_"),
      startsWith(stimulus_id, "if_") & pc_given_na >=t ~ paste("if2_", substr(stimulus_id, 4, 4), "h", sep=""),
      startsWith(stimulus_id, "if_") & pc_given_na <=1-t ~ paste("if2_", substr(stimulus_id, 4, 4), "l", sep=""),
      startsWith(stimulus_id, "if_") & pc_given_na >=0.4 & pc<=0.6 ~  paste("if2_", substr(stimulus_id, 4, 4), "u", sep=""),
      TRUE ~ stimulus_id));
  return(df)
}
