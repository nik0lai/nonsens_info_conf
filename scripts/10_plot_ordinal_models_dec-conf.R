# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, ggplot2, patchwork)

# ***************************************************
# Experiment 1 ------
# ***************************************************

bfs <- read_csv('data/ordinal_modeling/ord_models_conf-dec_exp1.csv', show_col_types = FALSE)

# Compare all simple models against null
bfs_directional_models <-
  bfs %>% 
  filter(nchar(model) == 2 | model == 'UN') %>% 
  mutate(baseline = bfs$bf[bfs$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

# simple arranged from more likely to least
bfs_directional_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))

## Plot models ---------

# Base plot
p_bias <-
  bfs_directional_models %>%
  # mutate(model = toupper(model)) %>% 
  # filter(nchar(model) == 1 | model %in% c('BM'), model != 'a') %>%
  mutate(model = recode(model, 'UN'='null'))  %>% 
  # mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + 
  xlab(expression(BF[model-over-null])) #+ ggtitle('Experiment 1') 

# Best performing models annotation
p_exp1 <-
  p_bias  +
  annotate("text", x = Inf, y = -Inf,
           hjust = 1.43, vjust = -27.8,
           label = "Best performing models", size = 3.5) +
  geom_segment(aes(x = 10500, xend = 30000,
                   y = 12, yend = 12),
               arrow = arrow(length = unit (0.3, "cm"))) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4))


# Save bf values relative to baseline and to best model
bfs_directional_models %>%
  mutate(bf_best_over_rest = max(bf_over_null_raw)/bf_over_null_raw) %>%
  write_csv('data/ordinal_modeling/bf_dec-conf_exp1.csv')

# ***************************************************
# Experiment 2 ------
# ***************************************************

bfs <- read_csv('data/ordinal_modeling/ord_models_conf-dec_exp2.csv', show_col_types = FALSE)

# Compare all simple models against null
bfs_directional_models <-
  bfs %>% 
  filter(nchar(model) == 2 | model == 'UN') %>% 
  mutate(baseline = bfs$bf[bfs$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

# simple arranged from more likely to least
bfs_directional_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))

## Plot models ---------

# Base plot
p_bias <-
  bfs_directional_models %>%
  # mutate(model = toupper(model)) %>% 
  # filter(nchar(model) == 1 | model %in% c('BM'), model != 'a') %>%
  mutate(model = recode(model, 'UN'='null'))  %>% 
  # mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + 
  xlab(expression(BF[model-over-null])) #+ ggtitle('Experiment 2') 

# Best performing models annotation
p_exp2 <-
  p_bias  +
  annotate("text", x = Inf, y = -Inf,
           hjust = 1.43, vjust = -27.8,
           label = "Best performing models", size = 3.5) +
  geom_segment(aes(x = 500, xend = 1500,
                   y = 12, yend = 12),
               arrow = arrow(length = unit (0.3, "cm"))) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4))


# Save bf values relative to baseline and to best model
bfs_directional_models %>%
  mutate(bf_best_over_rest = max(bf_over_null_raw)/bf_over_null_raw) %>%
  write_csv('data/ordinal_modeling/bf_dec-conf_exp2.csv')

# Combine -----------------------------------------------------------------

p_exp1 + p_exp2

ggsave('plots/ord_models_dec-conf_exp12.png', width = 7.2, height = 5, device=png)

# ***************************************************
# Experiment 1 and 2 ------
# ***************************************************

bfs <- read_csv('data/ordinal_modeling/ord_models_conf-dec_both.csv', show_col_types = FALSE)

# Compare all simple models against null
bfs_directional_models <-
  bfs %>% 
  filter(nchar(model) == 2 | model == 'UN') %>% 
  mutate(baseline = bfs$bf[bfs$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

# simple arranged from more likely to least
bfs_directional_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))

## Plot models ---------

# Base plot
p_bias <-
  bfs_directional_models %>%
  # mutate(model = toupper(model)) %>% 
  # filter(nchar(model) == 1 | model %in% c('BM'), model != 'a') %>%
  mutate(model = recode(model, 'UN'='null'))  %>% 
  # mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + 
  xlab(expression(BF[model-over-null])) #+ ggtitle('Experiment 1 & 2') 

# Best performing models annotation
p_exp12 <-
  p_bias  +
  annotate("text", x = Inf, y = -Inf,
           hjust = 1.6, vjust = -28.4,
           label = "Best performing models", size = 3.5) +
  geom_segment(aes(x = 1e+08, xend = 2e+08,
                   y = 12, yend = 12),
               arrow = arrow(length = unit (0.3, "cm"))) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4))

# Save bf values relative to baseline and to best model
bfs_directional_models %>%
  mutate(bf_best_over_rest = max(bf_over_null_raw)/bf_over_null_raw) %>%
  write_csv('data/ordinal_modeling/bf_dec-conf_both.csv')

# for reporting on paper 
report_df <- bfs_directional_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e')) %>% 
  mutate(bf_over_rest = exp(max(bf_over_null_log) - bf_over_null_log)) %>% 
  mutate(bf_over_rest_format = formatC(bf_over_rest, digits = 1, format = 'e'))

sprintf('#####################\nbest model: %s (%s) %s\nsecond best: %s (%s)\nbf best over second-best %s\n#####################', 
        report_df$model[[1]], report_df$model_label[[1]], report_df$bf_over_null_raw[[1]], 
        report_df$model[[2]], report_df$model_label[[2]], report_df$bf_over_rest_format[[2]]) %>% 
  cat()


p_exp12 + theme(plot.margin = margin(.5, .5, .5, .5, "cm"))

ggsave('plots/ord_models_dec-conf_both.png', width = 4.3, height = 5, device=png)

