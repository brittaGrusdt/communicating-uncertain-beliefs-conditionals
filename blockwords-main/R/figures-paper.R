library(cowplot)
library(ggpubr)
library(here)
library(gridExtra)
library(png)
library(latex2exp)
library(RColorBrewer)
library(boot) 
library(readr)
library(tidyverse)
library(ExpDataWrangling)
# Functions ---------------------------------------------------------------
rep_each <- function(x, times) {
  times <- rep(times, length.out = length(x))
  rep(x, times = times)
}
# Data --------------------------------------------------------------------
result_dir = here("data", "formatted-cleaned")
plot_dir <- here("data", "formatted-cleaned", "plots", "cogsci-paper")
if(!dir.exists(plot_dir)) dir.create(plot_dir, recursive = T)
DATA = read_csv(here("data", "cleaned-data.csv"))

tables.smooth = DATA %>% filter(!is.na(slider)) %>% 
  dplyr::select(prolific_id, id, pe_task.smooth, slider) %>% 
  pivot_wider(names_from = "slider", values_from="pe_task.smooth") %>%
  group_by(prolific_id, id) %>%
  rowid_to_column()

stimuli = tables.smooth$id %>% unique() %>% sort()

Sys.setenv(R_CONFIG_ACTIVE = "process_data")
params_behav <- config::get()
path_behav <- here(params_behav$dir_formatted_data_cleaned, 
                   params_behav$fn_uc_avg)

Sys.setenv(R_CONFIG_ACTIVE = "situation_specific_prior")
params.sit_specific <- config::get()
path_model.sit_spec = here(params.sit_specific$dir_results, 
                           params.sit_specific$fn_predictions_by_context)
behav_dirichlet = join_model_behavioral_averages(path_behav, path_model.sit_spec) %>%
  mutate(utterance = utt.standardized) %>% chunk_utterances() %>%
  rename(utt.type = utterance)

Sys.setenv(R_CONFIG_ACTIVE = "abstract_state_prior")
params.abstract <- config::get()
path_model.abstract = here(params.abstract$dir_results, 
                           params.abstract$fn_predictions_by_context)
behav_abstract = join_model_behavioral_averages(path_behav, path_model.abstract) %>% 
  mutate(utterance = utt.standardized) %>% chunk_utterances() %>%
  rename(utt.type = utterance)

# behavioral utterance choice data (all, not only ratios)
data.uc =  DATA %>% get_controlled_factors() %>% filter(human_exp2 == 1) %>% 
  mutate(utterance = utt.standardized) %>% 
  chunk_utterances() %>%
  rename(utt.type = utterance)

# Figure 2 ----------------------------------------------------------------
df.stats = data.uc %>% group_by(utt.type) %>%
  summarize(m = mean(pe_task)) %>%
  mutate(m.all = mean(m))
df.stats$m.wo_conj = df.stats %>% filter(utt.type != "conjunction") %>%
  pull(m) %>% mean()
df.uc = data.uc %>%
  mutate(prior_blue = as.character(prior_blue), 
         prior_blue = case_when(
           (prior_blue == "unc" | prior_blue == "uncl") ~ "uncertain",
           T ~ prior_blue),
         relation_type = as.character(relation_type),
         relation_type = case_when(relation_type != "independent" ~ "if",
                                   T ~ "independent")) %>% 
  mutate(prior_blue = as.factor(prior_blue), 
               relation_type = as.factor(relation_type)) %>%
  dplyr::select(prolific_id, id, utt.type, prior_blue, relation_type,
                pe_task, utt.standardized)

df.uc.unc = df.uc %>% filter(prior_blue == "uncertain")
p.uc_pe_uncertain = df.uc.unc %>%
  ggplot(aes(x = utt.type, y = pe_task)) +
  geom_violin() +
  geom_jitter(aes(color = relation_type), width=0.2, height=0, alpha=0.5, size=.8) +
  geom_point(data = df.stats, aes(x = utt.type, y = m), color = 'black', size=1) +
  labs(x="utterance type created in UC task",
       y=str_wrap("rating from PE task for created utterance", width=22)
  ) +
  theme_minimal() +
  theme(legend.position = "top", axis.text = element_text(size = 11)) +
  scale_color_brewer(name = "relation type", palette = "Dark2") +
  scale_x_discrete(
    labels=c('conjunction'='conjunction',
             'conditional'='conditional',
             'might + literal' = parse(text=TeX(r'(might $\phi$/$\neg\phi$)')),
             'literal' = parse(text=TeX(r'($\phi$/$\neg\phi$)')))
  )
p.uc_pe_uncertain
ggsave(paste(plot_dir, "fig2-new.png", sep=FS), p.uc_pe_uncertain, width=5, height=2.5)

# retrieve numbers
df.uc.unc %>%
  dplyr::select(prolific_id, id, utt.standardized, utt.type) %>% 
  group_by(id, utt.type) %>% dplyr::select(-utt.standardized) %>% 
  summarize(count = n(), .groups="drop_last") %>% 
  mutate(N = sum(count)) %>% group_by(id, utt.type) %>% 
  mutate(ratio = count/N) %>% dplyr::select(id, utt.type, ratio) %>%
  mutate(utt.type = as.character(utt.type),
         utt.type = case_when(utt.type=="might + literal" ~ "might+literal",
                              utt.type == "conjunction" ~ "AND",
                              utt.type=="conditional" ~ "IF", 
                              TRUE ~ utt.type),
         stimulus = as.factor(id),
         id = as.character(id)) %>% 
  filter(utt.type == "IF") %>% arrange(ratio)

# Figure 4 - correlations -------------------------------------------------
# abstract + specific model plotted together
results.joint = bind_rows(
  behav_dirichlet %>% add_column(predictor = "situation-specific prior"),
  behav_abstract %>% add_column(predictor = "abstract state prior")
) %>% 
  mutate(utt.type = as.character(utt.type)) %>% 
  mutate(stimulus.type = case_when(str_detect(stimulus, "if") ~ "dependent",
                                   TRUE ~ "independent")) %>%
  mutate(utt.type = case_when(utt.type == "might + literal" ~ "might+literal",
                              utt.type == "conjunction" ~ "AND",
                              utt.type == "conditional" ~ "IF", 
                              TRUE ~ utt.type)) %>%
  mutate(utt.type = factor(utt.type, levels=c("might+literal", "literal", "IF", "AND")))

p.scatter = ggscatter(
  results.joint,
  y = "behavioral", x = "model", add = "reg.line",
  conf.int = FALSE, cor.coef = FALSE, cor.method = "pearson",
  ylab = "Empirical observations", xlab = "Model predictions"
  ) +
  stat_cor(label.x = 0, label.y=0.6, method="pearson", label.sep="\n", size=3) +
  geom_point(data=results.joint, 
             aes(y=behavioral, x=model, color=utt.type),
             alpha=1, size=1) +
  theme_minimal() +
  scale_color_brewer(
    name = 'utterance type', palette="Dark2",
    labels=c(#'AND'= parse(text = TeX("$\\phi\\vee\\psi$")),
             #'IF'= parse(text = TeX("$\\phi\\rightarrow\\psi$")),
             'AND' = "conjunction",
             'IF' = "conditional",
             'might+literal' = parse(text=TeX(r'(might $\phi$/$\neg\phi$)')),
             'literal' = parse(text=TeX(r'($\phi$/$\neg\phi$)'))),
    guide=guide_legend(title.position="left", nrow=1)
  ) +
  facet_wrap(~predictor, scales="free") +
  theme(legend.position="bottom", legend.title = element_text(size = 11),
        # legend.spacing.x = unit(1.0, 'cm'),
        legend.key.size=unit(.25, "line"),
        legend.text = element_text(margin = margin(r = 10, unit = "pt"),
                                   size = 10),
        legend.margin = margin(c(5, 5, 5, 0)),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11))
p.scatter
ggsave(paste(plot_dir, "correlations-empirical.png", sep = FS), p.scatter, width=6, height=3.3)


# Figure 3 - behavioral both tasks ----------------------------------------
tables.long = tables.smooth %>% 
   pivot_longer(cols = c(bg, b, g, none), names_to="slider", values_to="response") 

dat <- tables.long %>% ungroup() %>%
  mutate(order = case_when(slider == "bg" ~ 1,
                           slider == "b" ~ 2,
                           slider == "g" ~ 3,
                           slider == "none" ~ 4),
         slider = as.factor(slider),         
         slider = fct_recode(slider,
                             "BG" = "bg", 
                             "B" = "b",
                             "G" = "g",
                             "NO" = "none"),
         slider = fct_reorder(slider, order))
pids =  dat$prolific_id %>% unique()
# B. slider rating plots
plots.sliders=list()
for(id in stimuli) {
  set.seed(123)
  df = dat %>% filter(id == (!! id)) %>%
    filter(prolific_id %in% sample(pids, 6, replace = FALSE)) %>% 
    group_by(slider, prolific_id)
  p.ex <- dat %>%
    filter(id == (!! id)) %>%
    ggplot(aes(x = slider, y = response)) +
    geom_violin() +
    geom_jitter(data = df, aes(shape = prolific_id), width=0.1, height = 0, show.legend=F) +
    geom_line(data = df, aes(group = prolific_id), alpha = 0.5) +
    theme_minimal() +
    theme(legend.position="none",
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=18)
    ) + labs(y = "", x = "")
  plots.sliders[[id]] = p.ex
}

# C. utterance choice bar plots (behav/model_abstrct/model_dirichlet)
sample_means <- function(df, i) {
  # make sure that 0-occurences are written into results tibble! (by complete)
  # utt_type must be factor!!
  means = df[i,] %>% group_by(utt.type) %>% summarize(n=n()) %>%
          complete(utt.type, fill = list(n = 0)) %>% 
           mutate(N = sum(n), p = n/N) %>% dplyr::select(utt.type, p) %>% 
      pivot_wider(names_from = "utt.type", values_from = "p") %>% as.matrix()
  return(means)
}

y_obs = map_dfr(stimuli, function(stim){
  dat = data.uc %>% dplyr::select(prolific_id, id, utt.type, utt.standardized) %>% 
    filter(id == !!stim) 
  n=1000
  bootstrapped_means = boot(data = dat, statistic = sample_means, R = n)
  bootstraps = bootstrapped_means$t %>% as_tibble() %>%
    rename_all(~bootstrapped_means$t0 %>% colnames()) %>% 
    rowid_to_column("idx_sample") %>%
    pivot_longer(cols = -idx_sample, names_to = "utt", values_to = "rating")
  
  lower = 0.025*n + 1; upper = 0.975*n
  n.utts = data.uc$utt.type %>% unique() %>% length()
  boot_ci =  bootstraps %>%
    group_by(utt) %>%
    arrange(utt, rating) %>% # order from small to large within groups
    dplyr::select(-idx_sample) %>% 
    add_column(rowid = c(rep(seq(1, n), n.utts))) %>% 
    filter(rowid == lower | rowid == upper) %>% 
    arrange(utt, rowid) %>%
    add_column(ci = rep(c("ci.low", "ci.up"), n.utts)) %>% 
    dplyr::select(utt, rating, ci) %>%
    pivot_wider(names_from = "ci", values_from = "rating")
  
  means.obs = bootstrapped_means$t0 %>% as_tibble() %>%
    pivot_longer(cols = everything(), names_to = "utt", values_to = "p") %>%
    add_column(stimulus = stim)
  res <- left_join(boot_ci, means.obs, by="utt") %>% 
    rename(utterance = utt)
  return(res)
})

res_chunked.dirichlet = read_csv(
  here(params.sit_specific$dir_results, 
       params.sit_specific$fn_predictions_by_context_chunked)
) %>% dplyr::select(-best.utt) %>% 
  add_column(predictor = "situation-specific prior")

res_chunked.abstract = read_csv(
  here(params.abstract$dir_results, 
       params.abstract$fn_predictions_by_context_chunked)
) %>% dplyr::select(-best.utt) %>% 
  add_column(predictor = "abstract state prior")
res_chunked.models = bind_rows(res_chunked.abstract, res_chunked.dirichlet)
results.joint = bind_rows(y_obs %>% add_column(predictor = "behavioral"),
                          res_chunked.models) %>%
  mutate(utterance = as.factor(utterance), predictor = as.factor(predictor)) 
  
plots.bars = list()
for(id in stimuli) {
  df = results.joint %>% filter(stimulus == (!! id))
  p.bars =  df %>% ggplot(aes(y = p, x=utterance)) +
    geom_bar(data=df %>% filter(predictor=="behavioral") , stat="identity") +
    geom_errorbar(data = df %>% filter(predictor=="behavioral"),
                  aes(ymin = ci.low, ymax = ci.up), width = 0.2) +
    geom_point(data = df %>% filter(predictor != "behavioral"), 
               aes(shape = predictor, color = predictor), size=2) +
    ylim(0, 1) +
    scale_color_brewer(name="model", palette="Dark2") +
    theme_minimal() + labs(y="", x="") +
    theme(legend.position="top", legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 16, angle=40, hjust = 1, vjust=1)) +
                                   # vjust = 0.6, hjust=.5)) +
    scale_shape_discrete(name="model") + 
    scale_x_discrete(
      labels=c('conjunction'= parse(text = TeX("$\\phi\\vee\\psi$")),
               'conditional'= parse(text = TeX("$\\phi\\rightarrow\\psi$")),
               'might + literal' = parse(text=TeX('might $\\phi$')),
               'literal' = parse(text=TeX('$\\phi$')))
    )
  plots.bars[[id]] = p.bars
}

#------------------------#
# A. stimuli pictures
plots.stimuli = list()
for(id in stimuli){
  p.stim = readPNG(here("experiment", "stimuli", "img", "group1", 
                        paste(id, "png", sep=".")))
  
  conditions = str_split(id, "_")[[1]]
  rel = conditions[[1]]
  priors = conditions[[2]]
  if(startsWith(priors, "u")){ pb = "0.5"
  }else if(startsWith(priors, "l")){ pb = "L"
  }else { pb = "H"
  }
  if(endsWith(priors, "l")){ pg = "L"
  }else if(endsWith(priors, "h") & rel=="if1"){ pg = "0"
  }else if(endsWith(priors, "h") & rel=="independent"){ pg = "H"
  }else { pg = "0.5"
  }
  t = paste("P(b): ", pb, "  P(g): ", pg, "\n", rel,  sep="")
  im.stim = ggplot() + background_image(p.stim) +
    ggtitle(label=t) +
    theme(plot.title = element_text(size=16, hjust = 0.6))

  plots.stimuli[[id]] = im.stim
}

# selected_stim = c("if1_uh", "if1_hh", "if2_ul", "if2_ll", "independent_ul",
#                   "independent_hh")
selected_stim = stimuli
df.utterance = data.uc %>% group_by(id) %>%
  dplyr::count(utt.standardized) %>% arrange(desc(n)) %>% 
  rename(utterance = utt.standardized)

data.utts = group_map(df.utterance, function(df, id){
  lens = rle(df$n)$lengths
  ranks = seq(1:length(lens))
  df$rank = rep_each(ranks, times = lens)
  return(df %>% add_column(id = id$id))
}) %>% bind_rows() %>% rename(occurrence = n)

# adding utterances less often used as 'other'
data.utt_other = data.utts %>%
  filter(rank >= 4 & id %in% selected_stim) %>%
  group_by(id) %>% 
  mutate(occurrence = sum(occurrence),
         utterance = "other",
         rank = as.integer(4)) %>%
  group_by(utterance, id) %>% 
  distinct()

data.utterance = data.utts %>%
  filter(rank <= 3 & id %in% selected_stim) %>%
  full_join(data.utt_other) %>% # J: add 'other' to utterances
  mutate(id = factor(id, levels = selected_stim), 
         rank = factor(rank, levels = seq(1, 4))) %>% 
  arrange(id, rank)

utt.levels = data.utterance$utterance %>% unique 
data.utterance = data.utterance %>% 
  mutate(utterance = 
           factor(utterance,
                  levels=c(utt.levels[utt.levels!="other"], "other"))
  )
# create single plots from the above dataframe
# extend 
names_colors = data.utterance$utterance %>% levels()
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(length(names_colors))
mycolors <- setNames(mycolors, names_colors)
mycolors["other"] = "#D3D3D3"

plots.bar = list()
for(idx in stimuli) {
  df = data.utterance %>%
    filter(id == idx) %>% 
    ggplot(aes(y=id)) +
    geom_bar(aes(x=occurrence, fill=utterance), stat="identity", width = 0.2) +
    theme_void() +
    theme(legend.title = element_text(size = 20),
          legend.text = element_text(margin = margin(r = 10, unit = "pt"),
                                     size = 20),
          legend.key.size=unit(0.5, "line"),
          legend.position = c(0.5, 0.6)
    ) + guides(guide_legend(nrow = 2)) +
    labs(y="") +
    scale_fill_manual(values = mycolors, drop=FALSE) +
    guides(fill=guide_legend(title.position="left", nrow=2))
    
  plots.bar[[idx]] = df
}
plots.bar[["all"]] <- data.utterance %>%
  ggplot(aes(y=id)) +
  geom_bar(aes(x=occurrence, fill=utterance), stat="identity", width = 0.1) +
  theme_void() +
  theme(legend.title = element_text(size = 22),
        #  legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 11),
        legend.key.size=unit(0.5, "line"),
        legend.position = c(0.5, 0.6)
  ) + 
  labs(y="") +
  scale_fill_manual(values = mycolors, drop=FALSE,
                    guide=guide_legend(title.position="left", nrow=1)
  )

# save single bars first with common legend
p.single_bars = ggpubr::ggarrange(
    plotlist = plots.bar[selected_stim],
    common.legend = T, legend = "top",
    nrow = 1, ncol = length(selected_stim)#,
    # font.label = list(size = 22), align="h"
  ) + theme(plot.margin = unit(c(0, 0, -3, 0), "cm"))
ggsave(paste(plot_dir, "single-bars.png", sep = FS), p.single_bars,
       width = 24, height = 2)

p.stimuli <- ggpubr::ggarrange(
  plotlist = plots.stimuli[selected_stim], 
  nrow = 1, 
  ncol = length(selected_stim),
  font.label = list(size = 22), 
  align="h"
) + theme(plot.margin = unit(c(-1, 0, -.4, 0), "cm")) 
ggsave(paste(plot_dir, "stimuli.png", sep = FS), p.stimuli, width=24, height=2)

p.sliders <- ggpubr::ggarrange(
  plotlist = plots.sliders[selected_stim], 
  nrow = 1, ncol = length(selected_stim),
  font.label = list(size = 22),
  align = "h"
) + theme(plot.margin = unit(c(0, 0, -0.4, 0), "cm")) 
ggsave(paste(plot_dir, "sliders.png", sep = FS), p.sliders, width=24, height=2)

p.bars <- ggpubr::ggarrange(
  plotlist = plots.bars[selected_stim], 
  common.legend = T, legend="bottom",
  font.label = list(size = 22), 
  align = "h",
  nrow = 1, 
  ncol = length(selected_stim)
) + theme(plot.margin = unit(c(0, 0, -0.1, 0), "cm")) 
ggsave(paste(plot_dir, "bars.png", sep = FS), p.bars, width = 24, height = 3)

p.all <- ggpubr::ggarrange(
  p.single_bars, p.stimuli, p.sliders, p.bars,
  nrow = 4, 
  ncol = 1, 
  labels = c("", "A", "B", "C"),
  font.label = list(size = 16), 
  align="h",
  heights = c(1, 2, 2, 2.5)
)
ggsave(paste(plot_dir, "stimuli-all.png", sep = FS), p.all, width=28,  height=9)


