# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, ggplot2, patchwork)

# ***************************************************
# Decision data ------
# ***************************************************

bfs_decision <- read_csv('data/ordinal_modeling/ord_models_decision_both.csv', show_col_types = FALSE)

# Compare all simple models against null
bfs_decision_simple_models <-
  bfs_decision %>% 
  mutate(baseline = bfs_decision$bf[bfs_decision$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

# simple arranged from more likely to least
bfs_decision_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))

# for reporting on paper 
report_df <- bfs_decision_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e')) %>% 
  mutate(bf_over_rest = exp(max(bf_over_null_log) - bf_over_null_log)) %>% 
  mutate(bf_over_rest_format = formatC(bf_over_rest, digits = 1, format = 'e'))


sprintf('######################\nbest model: %s (%s) %s\nsecond best: %s (%s)\nbf best over second-best %s\n######################', 
        report_df$model[[1]], report_df$model_label[[1]], report_df$bf_over_null_raw[[1]], 
        report_df$model[[2]], report_df$model_label[[2]], report_df$bf_over_rest_format[[2]]) %>% 
  cat()

## Plot simple models ---------

# Base plot
p_bias <-
  bfs_decision_simple_models %>%
  mutate(model = toupper(model)) %>% 
  mutate(model = recode(model, 'UN'='null'))  %>% 
  mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + 
  xlab(expression(BF[model-over-null])) +
  ggtitle('Decision task') 

# Best performing models annotation
p_bias <-
  p_bias  +
  annotate("text", x = Inf, y = -Inf,
           hjust = 1.2, vjust = -12.5,
           label = "Best performing models", size = 2.5) +
  geom_segment(aes(x = 5e+51, xend = 1e+52,
                   y = 5, yend = 5),
               arrow = arrow(length = unit (0.3, "cm"))) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4))

# Save bf values relative to baseline and to best model
bfs_decision_simple_models %>%
  mutate(bf_best_over_rest = max(bf_over_null_raw)/bf_over_null_raw) %>%
  write_csv('data/ordinal_modeling/bf_decision.csv')

# ***************************************************
# Confidence data ------
# ***************************************************

bfs_confidence <- read_csv('data/ordinal_modeling/ord_models_confidence_both.csv', show_col_types = FALSE)

# Compare all simple models against null
bfs_confidence_simple_models <- 
  bfs_confidence %>% 
  mutate(baseline = bfs_confidence$bf[bfs_confidence$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

# simple arranged from more likely to least
bfs_confidence_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))

# for reporting on paper 
report_df <- bfs_confidence_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e')) %>% 
  mutate(bf_over_rest = exp(max(bf_over_null_log) - bf_over_null_log)) %>% 
  mutate(bf_over_rest_format = formatC(bf_over_rest, digits = 1, format = 'e'))

sprintf('######################\nbest model: %s (%s) %s\nsecond best: %s (%s)\nbf best over second-best %s\n######################', 
        report_df$model[[1]], report_df$model_label[[1]], report_df$bf_over_null_raw[[1]], 
        report_df$model[[2]], report_df$model_label[[2]], report_df$bf_over_rest_format[[2]]) %>% 
  cat()

## Plot simple models ---------

# Base plot
p_conf <-
  bfs_confidence_simple_models %>%
  mutate(model = toupper(model)) %>% 
  # filter(nchar(model) == 1 | model %in% c('BM'), model != 'a') %>%
  mutate(model = recode(model, 'UN'='null'))  %>% 
  mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + 
  xlab(expression(BF[model-over-null])) +
  ggtitle('Confidence task') 

# Best performing models annotation
p_conf <-
  p_conf  +
  annotate("text", x = Inf, y = -Inf,
           hjust = 1.2, vjust = -12.5,
           label = "Best performing models", size = 2.5) +
  geom_segment(aes(x = 3e+11, xend = 6e+11,
                   y = 5, yend = 5),
               arrow = arrow(length = unit (0.3, "cm"))) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4))

# Save bf values relative to baseline and to best model
bfs_confidence_simple_models %>%
  mutate(bf_best_over_rest = max(bf_over_null_raw)/bf_over_null_raw) %>%
  write_csv('data/ordinal_modeling/bf_confidence.csv')

# ***************************************************
# Reproduction data ------
# ***************************************************

bfs_reproduction <- read_csv('data/ordinal_modeling/ord_models_reproduction_both.csv', show_col_types = FALSE)

# Compare all simple models against null
bfs_reproduction_simple_models <- 
  bfs_reproduction %>% 
  filter(nchar(model) == 1 | model == 'UN') %>% 
  mutate(baseline = bfs_reproduction$bf[bfs_reproduction$model == 'UN']) %>% 
  mutate(bf_over_null_log = bf - baseline,
         bf_over_null_raw = exp(bf_over_null_log)) %>%
  arrange(desc(bf_over_null_raw)) 

# simple arranged from more likely to least
bfs_reproduction_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e'))

# for reporting on paper 
report_df <- bfs_reproduction_simple_models %>% 
  mutate(bf_over_null_raw = formatC(bf_over_null_raw, digits = 1, format = 'e')) %>% 
  mutate(bf_over_rest = exp(max(bf_over_null_log) - bf_over_null_log)) %>% 
  mutate(bf_over_rest_format = formatC(bf_over_rest, digits = 1, format = 'e'))

sprintf('######################\nbest model: %s (%s) %s\nsecond best: %s (%s)\nbf best over second-best %s\n######################', 
        report_df$model[[1]], report_df$model_label[[1]], report_df$bf_over_null_raw[[1]], 
        report_df$model[[2]], report_df$model_label[[2]], report_df$bf_over_rest_format[[2]]) %>% 
  cat()

## Plot simple models ---------

# Base plot
p_rep <-
  bfs_reproduction_simple_models %>%
  mutate(model = toupper(model)) %>% 
  # filter(nchar(model) == 1 | model %in% c('BM'), model != 'a') %>%
  mutate(model = recode(model, 'UN'='null'))  %>% 
  mutate(model = factor(model, levels = c('null', sort(LETTERS[1:13], decreasing = TRUE)))) %>%
  ggplot(., aes(y=model, x=bf_over_null_raw)) + 
  geom_point() +
  ylab('Model') + 
  xlab(expression(BF[model-over-null])) +
  ggtitle('Reproduction task') 

# Best performing models annotation
p_rep <- p_rep  +
  annotate("text", x = Inf, y = -Inf,
           hjust = 1.2, vjust = -12.5,
           label = "Best performing models", size = 2.5) +
  geom_segment(aes(x = 1e+18, xend = 2e+18,
                   y = 5, yend = 5),
               arrow = arrow(length = unit (0.3, "cm"))) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))

# Save bf values relative to baseline and to best model
bfs_reproduction_simple_models %>%
  mutate(bf_best_over_rest = max(bf_over_null_raw)/bf_over_null_raw) %>%
  write_csv('data/ordinal_modeling/bf_reproduction.csv')

(p_bias + theme(axis.text.x = element_text(size=7))) + (p_rep + theme(axis.text.x = element_text(size=7))) + (p_conf + theme(axis.text.x = element_text(size=7)))
ggsave('plots/ord_models.png', width = 10, height = 3.5, scale = .8, dpi=1200, device=png)

