
# plotting ----------------------------------------------------------------
plotSliderRatings <- function(data, questions, labels, cluster_by="g", relation=FALSE, target_dir=NA){
  dat <- data %>% ungroup() %>%
    mutate(question = factor(question, levels = (!!questions)),
           response=as.numeric(response))
  
  df <- cluster_responses(dat, cluster_by);
  if(relation) {
    df.stats = df %>% group_by(question, id, understood_rel) %>%
      dplyr::select(question, id, response, understood_rel) %>%
      summarize(mu=mean(response), med=median(response), .groups="keep");
  } else {
    df.stats = df %>% group_by(question, id) %>%
      dplyr::select(question, id, response) %>%
      summarize(mu=mean(response), med=median(response), .groups="keep");
  }
  ids <- df$id %>% unique() %>% as.character()
  # ids <- c("independent_hh")
  for (id in ids){
    p <- df  %>% filter(id == (!! id)) %>% 
      ggplot(aes(y=response, x=factor(0)))
    if(relation) {
      p <- p + geom_boxplot(aes(colour=understood_rel), outlier.shape=NA,
                            position=position_dodge(0.75))
      df.stats <- df.stats %>% group_by(question, id, understood_rel)
    } else {
      p <- p + geom_boxplot(outlier.shape=NA) +
        guides(colour=FALSE)
    }
    p <- p + 
      geom_jitter(aes(colour=prolific_id), width=0.1, height = 0, alpha=0.5) +
      geom_point(data=df.stats %>% filter(id==(!! id)),
                 mapping=aes(y=mu, x=factor(0)),  size=2, shape=23) +
      # geom_vline(aes(xintercept=med), color = "red", size=0.5) +
      geom_hline(aes(yintercept=0), color = "grey", size=0.2, linetype='dashed') +
      geom_hline(aes(yintercept=0.25), color = "grey", size=0.2, linetype='dashed') +
      geom_hline(aes(yintercept=0.5), color = "gray45", size=0.5, linetype='dashed') +
      geom_hline(aes(yintercept=0.75), color = "grey", size=0.2, linetype='dashed') +
      geom_hline(aes(yintercept=1), color = "grey", size=0.2, linetype='dashed') +
      scale_y_continuous(limits=c(-0.2, 1.2),
                         breaks=c(0, 0.25, 0.5, 0.75, 1)) +
      labs(x="estimates probability", y="", title=id) +
      theme_bw() +
      facet_wrap(~question, nrow=2, ncol=2, labeller = as_labeller(labels)) + 
      theme(axis.text.x=element_blank(),
            axis.ticks.x =element_blank(),
            text = element_text(size=14),
            legend.position = "right")
  
    if(!is.na(target_dir)){
      ggsave(filename=paste(target_dir,  paste("slider-ratings-", id, ".png", sep=""), 
                            sep=.Platform$file.sep), p, width=12, height = 12)
    }
    
  }
}

plotSliderDensities <- function(data, questions, labels, target_dir=NA, jitter=TRUE){
  dat <- data %>% ungroup() %>%
    mutate(question = factor(question, levels = (!!questions)),
           response=as.numeric(response))
  dat.stats = dat %>% group_by(question, id) %>%
    dplyr::select(question, id, response) %>%
    summarize(mu=mean(response), med=median(response), .groups="keep");
  ids <- dat$id %>% unique() %>% as.character()
  for (id in ids){
    df = dat  %>% filter(id == (!! id))
    df.stats = dat.stats %>% filter(id == (!! id))
    p <- df %>% 
      ggplot(aes(x=response)) +
        geom_density(aes(fill=question), alpha=0.6) +
        guides(fill=FALSE)
    if(jitter){
      p = p + geom_jitter(aes(y=0), width=0, height = 0.1, alpha=0.5)
    }
    p = p + # geom_vline(aes(xintercept=med), color = "red", size=0.5) +
      geom_vline(data=df.stats, aes(xintercept=mu), color = "red",
                 size=0.5, linetype='dashed') +
      labs(x="slider ratings", y="", title=id) +
      theme_bw() +
      facet_wrap(~question, nrow=2, ncol=2, labeller = as_labeller(labels)) + 
      theme(text = element_text(size=14), legend.position = "right")
    
    if(!is.na(target_dir)){
      ggsave(filename=paste(target_dir,  paste("slider-ratings-", id, ".png", sep=""), 
                            sep=.Platform$file.sep), p, width=12, height = 12)
    }
  }
}

PlotMeans <- function(df, id, sd_error_bars=TRUE) {
  df.sorted = df %>% arrange(desc(means)) %>%
    mutate(label=case_when(question=="b" ~ labels.test[["b"]], 
                           question=="g" ~ labels.test[["g"]],
                           question=="bg" ~ labels.test[["bg"]],
                           question=="none" ~ labels.test[["none"]]))
  id.levels = df.sorted %>% pull(id) %>% unique()
  df.sorted = df.sorted %>%
    mutate(id=factor(id, levels=id.levels),
           question=factor(question, levels=questions.test, labels=labels.test))
  p <- df.sorted %>%
    ggplot(aes(x=id, y=means)) +
    geom_bar(aes(fill=id), stat="identity", position="dodge") + labs(x="")
  if(sd_error_bars) {
    p = p + geom_errorbar(aes(ymin=means-sd, ymax=means+sd), width=.2,
                          position=position_dodge(.9))
  }
  p <- p +
    facet_wrap(~question, nrow=2, ncol=2) +
    theme(text = element_text(size=20), axis.text.x=element_text(angle=90))
  return(p)
}

plot_ratings_across_conditions = function(df, title){
  p = df %>% group_by(dir) %>%
    ggplot(aes(x=condition, y=response)) +
    geom_boxplot(aes(color=relation), outlier.shape = NA, position=position_dodge(1)) +
    geom_jitter(aes(color=relation), shape=16, alpha=0.5, width=0, height=0.1) +
    facet_wrap(~dir) +
    theme_classic(base_size = 22) +
    theme(axis.text.x = element_text(size=15), legend.position = "bottom") +
    coord_flip() +
    ggtitle(title)
  return(p)
}

# Quality of the data -----------------------------------------------------
# for each participant take mean response of all other participants for each
# stimulus and question, then compute squared difference between participant's
# response and mean of all others -> 4 values, one for each question (for each participant)
# to get one value per participant sum these up (for each participant)
distancesResponses = function(df.prior, save_as=NA){
  df = df.prior %>% 
    dplyr::select(prolific_id, id, response, question) %>%
    unite(col = "id_quest", "id", "question", sep="__", remove=FALSE)
  
  distances <- tibble()
  for(proband in df.prior$prolific_id %>% unique()) {
    message(proband)
    res = df %>% filter(prolific_id == proband) %>% ungroup()
    for(stimulus in df$id %>% unique()) {
      dat <- df %>% filter(id == (!! stimulus));
      res_proband = res %>%
        filter(str_detect(id_quest, paste(stimulus, ".*", sep=""))) %>%
        dplyr::select(id_quest, response) %>% rename(r_proband = response) %>%
        add_column(comparator = proband)
      dat.others = anti_join(dat, res_proband %>% rename(prolific_id=comparator),
                             by=c("prolific_id", "id_quest"))
        
      means.others = dat.others %>% group_by(id_quest) %>% 
        summarize(mean.others=mean(response), .groups="drop_last")
      
      diffs = left_join(means.others, res_proband, by=c("id_quest")) %>%
        mutate(sq_diff = (r_proband - mean.others)**2)
      distances = bind_rows(distances, diffs)
    }
  }
  dist.sums <- distances %>%
    separate("id_quest", into=c("id", "question"), sep="__") %>%
    group_by(id, comparator) %>%
    summarize(sum_sq_diffs = sum(sq_diff), .groups="drop_last") %>% 
    mutate(mean.id=mean(sum_sq_diffs), sd.id=sd(sum_sq_diffs)) %>%
    group_by(comparator) %>% 
    mutate(mean.comparator=mean(sum_sq_diffs)) %>%
    ungroup()
  
  if(!is.na(save_as)){
    message(paste('save data to:', save_as))
    saveRDS(dist.sums, save_as)
  }
  return(dist.sums)
}

#@arg df: wide
log_likelihood = function(df, cn, par){
  if(cn == "A implies C"){
    df <- df %>% 
          mutate(ll=dbeta(p_c_given_a, par$pos1, par$pos2, log=TRUE) +
                    dbeta(p_c_given_na, par$neg1, par$neg2, log=TRUE) +
                    dbeta(p_a, par$marg1, par$marg2, log=TRUE),
                 cn=cn)
  } else if(cn == "A implies -C") {
    df <- df %>% 
      mutate(ll=dbeta(1-p_c_given_a, par$pos1, par$pos2, log=TRUE) +
                dbeta(1-p_c_given_na, par$neg1, par$neg2, log=TRUE) +
                dbeta(p_a, par$marg1, par$marg2, log=TRUE),
             cn=cn)
  } else if(cn=="C implies A"){
    df <- df %>% 
      mutate(ll=dbeta(p_a_given_c, par$pos1, par$pos2, log=TRUE) +
                dbeta(p_a_given_nc, par$neg1, par$neg2, log=TRUE) +
                dbeta(p_c, par$marg1, par$marg2, log=TRUE),
             cn=cn)
  } else if(cn=="C implies -A"){
    df <- df %>% 
      mutate(ll=dbeta(1-p_a_given_c, par$pos1, par$pos2, log=TRUE) +
                dbeta(1-p_a_given_nc, par$neg1, par$neg2, log=TRUE) +
                dbeta(p_c, par$marg1, par$marg2, log=TRUE),
             cn=cn)
  } else if(cn=="A || C" || cn == "ind") {
    df <- df %>% 
      mutate(ll=dbeta(p_a, par$p_a1, par$p_a2, log=TRUE) +
                dbeta(p_c, par$p_c1, par$p_c2, log=TRUE) +
                log(dtruncnorm(AC-(p_a*p_c), a=-1, b=1, mean=0, sd=par$p_diff_sd)),
             cn=cn)
  } else {
    stop(paste("likelihood not defined for cn:", cn))
  }
  return(df)
}

add_cn_probs = function(df.wide, cn){
  if(cn == "dep"){
    df.cn = df.wide %>%
      compute_cond_prob("P(C|A)") %>% rename(p_c_given_a=p) %>%
      compute_cond_prob("P(-C|A)") %>% rename(p_nc_given_a=p) %>%
      compute_cond_prob("P(C|-A)") %>% rename(p_c_given_na=p) %>%
      compute_cond_prob("P(-C|-A)") %>% rename(p_nc_given_na=p) %>% 
      compute_cond_prob("P(A|C)") %>% rename(p_a_given_c=p) %>%
      compute_cond_prob("P(-A|C)") %>% rename(p_na_given_c=p) %>%
      compute_cond_prob("P(A|-C)") %>% rename(p_a_given_nc=p) %>%
      compute_cond_prob("P(-A|-C)") %>% rename(p_na_given_nc=p) %>%  
      mutate(p_a=AC+`A-C`, p_c=AC+`-AC`)
  } else if(cn == "ind") {
    df.cn = df.wide %>% mutate(p_a=AC+`A-C`, p_c=AC+`-AC`)
  }else {
    stop("not implemented")
  }
  return(df.cn)
}

formatParams4WebPPL = function(params){
  par=params %>% ungroup() %>% dplyr::select(-cn) %>% group_by(id) %>% 
    transmute(p_ind=list(c(a1=p_a1, a2=p_a2, c1=p_c1, c2=p_c2)),
              p_dep=list(c(pos1=pos1, pos2=pos2, neg1=neg1, neg2=neg2,
                           marg1=marg1, marg2=marg2))
             )
  par.ind = par %>% filter(str_detect(id, "independent")) %>% dplyr::select(-p_dep) %>%
    pivot_wider(names_from="id", values_from="p_ind", names_prefix="logL_")
  par.dep = par %>% filter(!str_detect(id, "independent")) %>% dplyr::select(-p_ind) %>%
    pivot_wider(names_from="id", values_from="p_dep", names_prefix="logL_")
 formatted = bind_cols(par.ind, par.dep) 
 return(formatted)
}

understood_relations = function(data.prior.norm){
  prior.exp.wide = data.prior.norm %>% filter(id!="ind2") %>%
    separate(id, into = c("relation", "condition"), sep="_", remove=FALSE) %>%
    group_by(prolific_id, relation, condition) %>%
    mutate(response=(response+epsilon)/sum(response+epsilon)) %>% 
    pivot_wider(names_from="question", values_from="response") %>%
    mutate(p_blue=bg+b, p_green=bg+g, g_given_b=bg/(bg+b),
           g_given_nb=g/(g+none), b_given_g=bg/(bg+g), b_given_ng=b/(b+none))
  
  threshold.ind = 0.07
  threshold = 0.15
  prior.relations = prior.exp.wide %>% group_by(relation, prolific_id) %>%
    dplyr::select(relation, id, prolific_id, condition, g, b, p_blue, p_green, bg, none,
           g_given_nb, g_given_b) %>%
    mutate(as_expected=case_when(
      relation=="independent" ~ abs(bg - p_blue * p_green) < threshold.ind,
      (relation == "if1" | relation=="if2") ~ g<threshold & g_given_b > g_given_nb)
    ) %>% group_by(id)
  # df = prior.relations %>% 
  #   summarize(N=n(), n=sum(as_expected), ratio=n/N, .groups = "keep") %>%
  #   arrange(desc(ratio))
  return(prior.relations)
}

