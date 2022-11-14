webppl_distrs_to_tibbles <- function(posterior){
  posterior_tibbles <- map2(posterior, names(posterior), function(x, y){
    x <- x %>% rowid_to_column() 
    bn_probs <- x %>% dplyr::select("probs", "rowid")
    data_tibble <- x$support %>% rowid_to_column() %>% 
      unnest(cols = c(table.probs, table.support)) %>%
      as_tibble() %>% 
      left_join(bn_probs, by = "rowid") %>% 
      mutate("bn_id" = as.character(rowid)) %>%
      add_column(level=y) %>% 
      rename(prob=probs, val=table.probs, cell=table.support)
    return(data_tibble)             
  })
  df <- bind_rows(posterior_tibbles)
  return(df)
}

run_webppl <- function(path_wppl_file, params){
  if(params$verbose){
    print(paste('model file read from:', path_wppl_file))
    print(paste('packages loaded from:', params$packages))
  }
  print(paste("theta:", params$theta, 
              "alpha:", params$alpha, 
              "cost_c:", params$cost_conditional))
  data <-   webppl(program_file = path_wppl_file,
                   data = params,
                   data_var = "data",
                   random_seed = params$seed_webppl,
                   packages = params$packages
  )
  # data is a list of lists
  data <- data %>% map(function(x){as_tibble(x)})
  return(data)
}


# summarise webppl distributions ------------------------------------------
webppl_speaker_distrs_to_tibbles <- function(posterior){
  speaker <- posterior[names(posterior) != "bns"] 
  posterior_tibbles <- map2(speaker, names(speaker), function(x, y){
    data_tibble <- x %>% rowid_to_column() %>% unnest(cols = c(probs, support)) %>% 
      rename(utterance=support)
    return(data_tibble)             
  })
  speaker <- bind_rows(posterior_tibbles) %>% mutate(utterance=paste("utt_", utterance, sep=""))
  bns_unique <- posterior$bns %>% rowid_to_column() %>%
    unnest(cols = c(table.probs, table.support)) %>% 
    rename(cell=table.support, val=table.probs) %>%
    spread(key=cell, val=val) %>%
    nest(data = c(cn, `-A-C`, `-AC`, `A-C`, `AC`))
  
  # add cn, AC, A-C, -AC, -A-C cell entries to speaker data
  bns <- bns_unique[speaker$rowid,]$data
  speaker_wide <- speaker %>% add_column(bn=bns) %>% unnest(cols = c(bn)) %>%
    spread(key=utterance, val=probs, fill=0)
  
  return(speaker_wide)
}


structure_speaker_data <- function(posterior, params, save=NA){
  if(is.na(save)) save=params$save
  speaker_wide <- webppl_speaker_distrs_to_tibbles(posterior)
  bns = posterior$bns %>% rowid_to_column() %>% group_by(rowid) %>%
    unnest(c(table.probs, table.support)) %>%
    pivot_wider(names_from=table.support, values_from=table.probs) %>%
    dplyr::select(-cn);
  df.wide = left_join(
    speaker_wide %>%  dplyr::select(-`AC`, -`A-C`, -`-AC`, -`-A-C`),
    bns, by = "rowid"
  )
  # df <- acceptability_conditions(df.wide)
  df <- df.wide %>% group_by(rowid) %>%
    pivot_longer(cols=c(starts_with("utt_")),
                 names_to = "utterance", values_to = "probs", names_prefix="utt_") %>%
    ungroup()
  
  if(save){
    df %>% save_data(here(params$dir_results, params$fn_results))
    params %>% save_data(here(params$dir_results, params$fn_params))
  }
  return(df)
}
