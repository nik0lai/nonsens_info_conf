# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
# p_load(magrittr, purrr, dplyr, here, readr, tidyr, colorspace, ggplot2, tibble, litHelp, patchwork)
p_load(magrittr, readr, dplyr, purrr, tidyr, tibble, ggplot2, stringr, patchwork)

source('scripts/funcs.R')

# Data --------------------------------------------------------------------

dat <- dir('data/processed/', pattern = 'filtered', full.names = TRUE) %>% 
  map(., ~read_csv(.x)) %>% 
  bind_rows() %>% factor_bias_source(.)

# Get SDT measures --------------------------------------------------------

sdt_data <-
  dat %>% 
  filter(trial_type == 'decision') %>% 
  mutate(answer = recode(answer, `1`='long', `0`='short')) %>% 
  mutate(conf_mat = paste0(tolower(accuracy), '_', answer)) %>%
  group_by(confidence_type, participant, bias_source, bias_direction, conf_mat) %>%
  summarise(count = n(), 
            .groups = "keep") %>% 
  pivot_wider(data = ., names_from = conf_mat, 
              values_from = count, values_fill = list(count = 0)) %>% 
  do(sdt_calc(hits = .$true_long, misses = .$false_short, 
              crrej = .$true_short, far = .$false_long)) %>%
  pivot_longer(data = ., 
               cols = c(hr, fr, lambda, d, ccrit, beta), names_to = "key", 
               values_to = "value") %>% 
  filter(key %in% c('d', 'ccrit')) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  ungroup()

# Get reproduction error data ---------------------------------------------

rep_data <- 
  dat %>% 
  filter(trial_type == 'reproduction') %>% 
  mutate(answer = as.integer(answer),
         reproduction_error = answer - target_length) %>% 
  group_by(confidence_type, participant, bias_source, bias_direction) %>% 
  summarise(reproduction_error = mean(reproduction_error))

# Labels ------------------------------------------------------------------

# Bias source label
label_bias_source = c('baserate' = 'Base-rate', 
                      'mullerlyer' = 'Müller-Lyer',
                      'payoff' = 'Payoff')
# Bias direction label
label_bias_direction = c('long'='Long', 'short'='Short')

# Create condition column -------------------------------------------------

# set order for conditions in the plot
sep <- '_'
condition_levels <- c()
for (s in c('mullerlyer', 'baserate', 'payoff')) {
  for (d in c('long', 'short'))
    condition_levels <- c(condition_levels, paste0(s,sep,d))
}

# Create column on SDT data
sdt_data <- sdt_data %>% 
  mutate(condition = factor(paste0(bias_source, sep, bias_direction), levels = condition_levels))
# Create column on reproduction data
rep_data <- rep_data %>% 
  mutate(condition = factor(paste0(bias_source, sep, bias_direction), levels = condition_levels))

# Plot Bias ---------------------------------------------------------------

bias_plot_bf_d_y_position <- 1.5
bias_plot_y_limits <- c(-1.5, 2.2)

## Concurrent ------

data_to_plot <- sdt_data %>% 
  filter(confidence_type == 'concurrent') %>% 
  select(-d) %>% 
  rename(value = ccrit) %>% 
  ungroup()

p_bias_concurrent <- basic_point_plot(data=data_to_plot, bf_d_y_position=bias_plot_bf_d_y_position) +
  ggtitle('Categorization bias') +
  ylab('SDT criterion') +
  ylim(bias_plot_y_limits)

p_bias_concurrent
ggsave('plots/concurrent_sdt_criterion.png', width = 6, height = 4, scale = .7)

## Delayed ------

data_to_plot <- sdt_data %>% 
  filter(confidence_type == 'delayed') %>% 
  select(-d) %>% 
  rename(value = ccrit) %>% 
  ungroup()

p_bias_delayed <- basic_point_plot(data=data_to_plot, bf_d_y_position=bias_plot_bf_d_y_position) +
  ggtitle('Categorization bias') +
  ylab('SDT criterion') +
  ylim(bias_plot_y_limits)

p_bias_delayed
ggsave('plots/delayed_sdt_criterion.png', width = 6, height = 4, scale = .7)

# Plot Reproduction ---------------------------------------------------------------

reproduction_plot_bf_d_y_position <- 65
reproduction_plot_y_limits <- c(-80, 82)

## Concurrent ------

data_to_plot <- 
  rep_data %>% 
  filter(confidence_type == 'concurrent') %>%
  rename(value = reproduction_error) %>% 
  ungroup()

p_reproduction_concurrent <- 
  basic_point_plot(data=data_to_plot, bf_d_y_position=reproduction_plot_bf_d_y_position) +
  ggtitle('Reproduction bias') +
  ylab('Reproduction error')  +
  ylim(reproduction_plot_y_limits)

p_reproduction_concurrent
ggsave('plots/concurrent_reproduction_error.png', width = 6, height = 4, scale = .7)

## Delayed ------

data_to_plot <- 
  rep_data %>% 
  filter(confidence_type == 'delayed') %>%
  rename(value = reproduction_error) %>% 
  ungroup()

p_reproduction_delayed <- 
  basic_point_plot(data=data_to_plot, bf_d_y_position=reproduction_plot_bf_d_y_position) +
  ggtitle('Reproduction bias') +
  ylab('Reproduction error') +
  ylim(reproduction_plot_y_limits)

p_reproduction_delayed
ggsave('plots/delayed_reproduction_error.png', width = 6, height = 4, scale = .7)

# Combine reproduction plot -----------------------------------------------

data_to_plot <- 
  rep_data %>% 
  ungroup() %>% 
  select(-confidence_type) %>% 
  rename(value = reproduction_error) 

p_reproduction_combined <- basic_point_plot(data=data_to_plot, bf_d_y_position=reproduction_plot_bf_d_y_position) +
  ggtitle('Reproduction bias') +
  ylab('Reproduction error') +
  ylim(reproduction_plot_y_limits)

p_reproduction_combined
ggsave('plots/combined_reproduction_error.png',width = 7, height = 4, dpi = 300, scale=.68)

# Plot Sensitivity ---------------------------------------------------------------

sensitivity_plot_bf_d_y_position <- 2.3
sensitivity_plot_y_limits <- c(0, 2.5)

## Concurrent ------

data_to_plot <- sdt_data %>% 
  filter(confidence_type == 'concurrent') %>% 
  select(-ccrit) %>% 
  rename(value = d) %>% 
  ungroup()

p_sensitivity_concurrent <- basic_point_plot(data=data_to_plot, bf_d_y_position=sensitivity_plot_bf_d_y_position, one_tail = FALSE) +
  ggtitle('Categorization sensitivity') +
  ylab('SDT d\'') +
  ylim(sensitivity_plot_y_limits)

p_sensitivity_concurrent
ggsave('plots/concurrent_sdt_sensitivity.png', width = 6, height = 4, scale = .7)

## Delayed ------

data_to_plot <- sdt_data %>% 
  filter(confidence_type == 'delayed') %>% 
  select(-ccrit) %>% 
  rename(value = d) %>% 
  ungroup()

p_sensitivity_delayed <- basic_point_plot(data=data_to_plot, bf_d_y_position=sensitivity_plot_bf_d_y_position, one_tail = FALSE) +
  ggtitle('Categorization sensitivity') +
  ylab('SDT d\'') +
  ylim(sensitivity_plot_y_limits)

p_sensitivity_delayed
ggsave('plots/delayed_sdt_sensitivity.png', width = 6, height = 4, scale = .7)


# Combine sensitivity plots -----------------------------------------------

p_sensitivity_concurrent / p_sensitivity_delayed

ggsave('results/plots/combined_sdt_sensitivity.png', width = 10, height = 10, scale = .6)

