# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, here, readr, tidyr, ggplot2, patchwork, stringr, BayesFactor)

source('scripts/funcs.R')


# Data --------------------------------------------------------------------

# Read decision data
dec_dat <-
  bind_rows(
    read_csv(here('data/processed/decision_predicted_curve_concurrent.csv'), show_col_types = FALSE),
    read_csv(here('data/processed/decision_predicted_curve_delayed.csv'), show_col_types = FALSE)
  )

# Read confidence data
conf_dat <-
  bind_rows(
    read_csv(here('data/processed/confidence_predicted_curve_concurrent.csv'), show_col_types = FALSE),
    read_csv(here('data/processed/confidence_predicted_curve_delayed.csv'), show_col_types = FALSE)
  )


# Get estimates -----------------------------------------------------------

# Get PSE
dec_dat <- 
  dec_dat %>% 
  get_pse() %>% 
  ungroup() %>% 
  select(-data)

# Get PMU
conf_dat <- 
  conf_dat %>% 
  get_lowest_conf() %>% 
  ungroup() %>% 
  select(-value)

# Remove participants without point to test -------------------------------

# remove participants with bad fits
bad_fits <- read_csv(here('data/processed/bad_fits.csv'), show_col_types = FALSE)
# filter out people
dec_dat <- dec_dat %>% anti_join(bad_fits %>% select(-c(data, type)), by = join_by(confidence_type, participant, bias_source))
conf_dat <- conf_dat %>% anti_join(bad_fits %>% select(-c(data, type)), by = join_by(confidence_type, participant, bias_source))

# Calculate differences in decision PSE
dec_dat <- dec_dat %>% 
  pivot_wider(names_from = bias_direction, values_from = target_length) %>% 
  mutate(pse = short - long) %>% 
  select(-c(short, long))

# Calculate differences in confidence PMU
conf_dat <- conf_dat %>% 
  pivot_wider(names_from = bias_direction, values_from = target_length) %>% 
  mutate(pmu = short - long) %>% 
  select(-c(short, long))

# Format data -------------------------------------------------------------

# Combine all data
dat <- full_join(dec_dat, conf_dat, by = join_by(confidence_type, participant, bias_source))

# Put data in long format
dat <- dat %>% 
  pivot_longer(names_to = 'task', values_to = 'value', cols = c(pse, pmu))

# Renumber participants. In both experiments participant numbers are counted from 0 so there will be overlapping in numbers.
dat <-
  dat %>%
  group_by(confidence_type, participant) %>%
  mutate(participant = sprintf('S%04d', (cur_group_id()))) %>%
  ungroup()

# Create condition code
dat <- 
  dat %>% 
  mutate(cond_code = paste0(substr(bias_source, 1, 2), substr(task, 1, 3))) 

# Count subjects
dat %>% 
  group_by(bias_source) %>% 
  select(bias_source, participant) %>% 
  distinct() %>% 
  reframe(count = n())

# Summarize estimation
dat %>% 
  group_by(bias_source, task) %>% 
  reframe(value=mean(value)) %>% 
  pivot_wider(names_from = task, values_from = value) %>% 
  mutate(delta = pmu - pse)


# Set data to plot --------------------------------------------------------

plots <- c('exp1', 'exp2', 'both')

for (e in plots) {
  
  # select data
  if (e == 'exp1') {
    data_to_plot <- 
      dat %>% 
      filter(confidence_type == 'concurrent')
    
  } else if (e == 'exp2') {
    data_to_plot <- 
      dat %>% 
      filter(confidence_type == 'delayed')
  } else {
    data_to_plot <- dat
  }
  
  # Count subjects
  data_to_plot %>% 
  group_by(confidence_type, bias_source, task) %>% 
    reframe(count = n()) %>% 
    pivot_wider(names_from = task, values_from = count)
    
  # Summarize data
  summ_data <-
    data_to_plot %>% 
    group_by(bias_source, task) %>% 
    reframe(value = mean_se(value)) %>% 
    unnest(value) %>% 
    mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')))
  
  # Plot A)
  pd <- position_dodge(width = 0)
  
  summ_data %>% 
    ggplot(aes(x=bias_source, y=y, linetype=task, group=task)) +
    geom_point(position = pd) +
    geom_errorbar(aes(x=bias_source, ymin = ymin, ymax = ymax), width=.2, position = pd) +
    geom_line(position = pd) +
    scale_x_discrete(labels = label_bias_source) +
    scale_linetype_discrete(labels = c('pmu' = 'Confidence', 'pse'='Decision')) +
    guides(linetype = guide_legend('Task')) +
    ylab('Bias effect (pixels)') +
    xlab('Condition') +
    theme(legend.position = 'right',
          legend.direction = 'vertical',
          legend.justification.right = 'bottom',
          axis.title = element_text(size=12),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10)) +
    ylim(0, 30)
  
  plot_name <- sprintf('plots/dec-conf_%s.png', e)
  ggsave(plot_name, width = 4, height = 1.8, scale = 1.3, dpi = 600)
  
}





