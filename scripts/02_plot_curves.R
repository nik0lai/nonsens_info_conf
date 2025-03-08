# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, here, ggplot2, tibble, stringr, purrr, patchwork)

source('scripts/funcs.R')

# Data --------------------------------------------------------------------

# Empirical data
dat_concurrent <- read_csv(here('data/processed/filtered_concurrent.csv'), col_types = cols()) %>% factor_bias_source(.)
dat_delayed <- read_csv(here('data/processed/filtered_delayed.csv'), col_types = cols()) %>% factor_bias_source(.)

# Fit data decision
dat_decision_concurrent <- read_csv(here('data/processed/decision_predicted_curve_concurrent.csv'), col_types = cols()) %>% factor_bias_source(.)
dat_decision_delayed <- read_csv(here('data/processed/decision_predicted_curve_delayed.csv'), col_types = cols()) %>% factor_bias_source(.)

# Fit data confidence
dat_confidence_concurrent <- read_csv(here('data/processed/confidence_predicted_curve_concurrent.csv'), col_types = cols()) %>% factor_bias_source(.)
dat_confidence_delayed <- read_csv(here('data/processed/confidence_predicted_curve_delayed.csv'), col_types = cols()) %>% factor_bias_source(.)

# Fit data reproduction
dat_reproduction_concurrent <- read_csv(here('data/processed/reproduction_predicted_curve_concurrent.csv'), col_types = cols()) %>% factor_bias_source(.)
dat_reproduction_delayed <- read_csv(here('data/processed/reproduction_predicted_curve_delayed.csv'), col_types = cols()) %>% factor_bias_source(.)

# Remove participants with bad fits ---------------------------------------

# read list with bad participants
bad_fits <- read_csv('data/processed/bad_fits.csv', show_col_types = FALSE)

# check how many participants have a non-significant coefficient and/or inverted curve 
bad_fits %>% 
  mutate(check=TRUE) %>% 
  pivot_wider(names_from = type, values_from = check) %>% 
  unnest() %>% 
  arrange(confidence_type, bias_source, participant) %>% 
  print(n=100)

# remove participants

# custom function to remove bad participants
remove_bad_fits <- 
  function(df) {
  df %>% 
      anti_join(bad_fits, by = join_by(confidence_type, participant, bias_source))
      
}

dat_decision_concurrent     <- dat_decision_concurrent %>%      remove_bad_fits()
dat_decision_delayed        <- dat_decision_delayed %>%         remove_bad_fits()
dat_confidence_concurrent   <- dat_confidence_concurrent %>%    remove_bad_fits()
dat_confidence_delayed      <- dat_confidence_delayed %>%       remove_bad_fits()
dat_reproduction_concurrent <- dat_reproduction_concurrent %>%  remove_bad_fits()
dat_reproduction_delayed    <- dat_reproduction_delayed %>%     remove_bad_fits()

# Check NAs ---------------------------------------------------------------

# decision
dat_decision_concurrent %>% filter(if_any(c(value, target_length), is.na))
dat_decision_delayed %>% filter(if_any(c(value, target_length), is.na))
# confidence
dat_confidence_concurrent %>% filter(if_any(c(value, target_length), is.na))
dat_confidence_delayed %>% filter(if_any(c(value, target_length), is.na))
# reproduction
dat_reproduction_concurrent %>% filter(if_any(c(value, target_length), is.na))
dat_reproduction_delayed %>% filter(if_any(c(value, target_length), is.na))

# Plot decision -----------------------------------------------------------

## Concurrent ---------------------------------

p_decision_concurrent <- 
  basic_curve_plot(curve_data = dat_decision_concurrent %>% filter(target_length >= 365 & target_length <= 435), 
                   empirical_data = dat_concurrent %>% 
                     filter(trial_type=='decision') %>% 
                     mutate(answer=recode(answer,'short'=0,'long'=1))) +
  ggtitle('Decision task')

# Get PSE
pse_concurrent <- get_pse(curve_data = dat_decision_concurrent) 

# Get PSE summary
pse_concurrent_summary <- get_pse_summary(pse_concurrent)

# Add PSE
p_decision_concurrent <-
  add_pse_to_plot(base_plot = p_decision_concurrent, pse_data = pse_concurrent_summary, 
                  pse_position = 'bottom', reproduction_plot = FALSE) +
  ylab('Proportion "Long" answers') +
  xlab('Target length') +
  geom_text(data=get_bf_d(data = pse_concurrent %>% select(-data)), 
            aes(x=386, y= .9, label=label), size=2.5, parse=TRUE, inherit.aes = FALSE) +
  theme(axis.title.x = element_blank(),
        legend.position = 'none') 

## Delayed ---------------------------------

p_decision_delayed <- 
  basic_curve_plot(curve_data = dat_decision_delayed %>% filter(target_length >= 365 & target_length <= 435), 
                   empirical_data = dat_delayed %>% 
                     filter(trial_type=='decision') %>%
                     mutate(answer=recode(answer,'short'=0,'long'=1))) +
  ggtitle('Decision task')

# Get PSE
pse_delayed <- get_pse(curve_data = dat_decision_delayed) 

# Keep an eye for NAs
pse_delayed_summary <- get_pse_summary(pse_delayed)

# Add PSE
p_decision_delayed <-
  add_pse_to_plot(base_plot = p_decision_delayed, pse_data = pse_delayed_summary, 
                  pse_position = 'bottom', reproduction_plot = FALSE) +
  ylab('Proportion "Long" answers') +
  xlab('Target length') +
  geom_text(data=get_bf_d(data = pse_delayed %>% select(-data)), 
            aes(x=386, y= .9, label=label), size=2.5, parse=TRUE, inherit.aes = FALSE) +
  theme(axis.title.x = element_blank(),
        legend.position = 'none')

# Plot confidence ---------------------------------------------------------

## Concurrent ---------------------------------

p_confidence_concurrent <- 
  basic_curve_plot(curve_data = dat_confidence_concurrent %>% filter(target_length >= 365 & target_length <= 435), 
                   empirical_data = dat_concurrent %>% 
                     filter(trial_type=='decision') %>% 
                     mutate(answer = recode(confidence, 'low'=0,'high'=1))) +
  ggtitle('Confidence task')

### Lowest confidence point -------------------

lowest_confidence_concurrent <- get_lowest_conf(dat_confidence_concurrent)
lowest_confidence_summary_concurrent <- get_pse_summary(lowest_confidence_concurrent)

# Add PSE
p_confidence_concurrent <-
  add_pse_to_plot(base_plot = p_confidence_concurrent, pse_data = lowest_confidence_summary_concurrent, 
                  pse_position = 'bottom', reproduction_plot = FALSE) +
  # format_axis(., y_axis_position = 'left', y_limits = get_y_limits(dat_decision_concurrent)) +
  ylab('Proportion "High" answers') +
  xlab('Target length') +
  geom_text(data=get_bf_d(data = lowest_confidence_concurrent %>% select(-value)), 
            aes(x=400, y= 1.1, label=label), size=2.8, parse=TRUE, inherit.aes = FALSE)

# Add bias direction legend
p_confidence_concurrent <-
  p_confidence_concurrent +
  geom_point(aes(fill=bias_direction), alpha=0, show.legend = TRUE) +
  scale_fill_manual(name = 'Bias direction', 
                    values = c(1, 1),
                    breaks = c('long', 'short'),
                    labels = c('Long', 'Short')) +
  guides(
    color='none',
    fill=guide_legend(title='Bias direction', 
                      override.aes = list(linetype=c(1,2),
                                          color=c('black', lighten('black', .6)))),
    linetype='none') +
  theme(legend.position = 'bottom')

## Delayed ---------------------------------

p_confidence_delayed <- 
  basic_curve_plot(curve_data = dat_confidence_delayed %>% filter(target_length >= 365 & target_length <= 435),
                   empirical_data = dat_delayed %>% 
                     filter(trial_type=='decision') %>% 
                     mutate(answer = recode(confidence, 'low'=0,'high'=1))) +
  ggtitle('Confidence task')

### Lowest confidence point -------------------

lowest_confidence_delayed <- get_lowest_conf(dat_confidence_delayed)
lowest_confidence_summary_delayed <- get_pse_summary(lowest_confidence_delayed)

# Add PSE
p_confidence_delayed <-
  add_pse_to_plot(base_plot = p_confidence_delayed, pse_data = lowest_confidence_summary_delayed, 
                  pse_position = 'bottom', reproduction_plot = FALSE) +
  # format_axis(., y_axis_position = 'left', y_limits = get_y_limits(dat_decision_delayed)) +
  ylab('Proportion "High" answers') +
  xlab('Target length') +
  geom_text(data=get_bf_d(data = lowest_confidence_delayed %>% select(-value)), 
            aes(x=400, y= 1.1, label=label), size=2.8, parse=TRUE, inherit.aes = FALSE)

# Add bias direction legend
p_confidence_delayed <-
  p_confidence_delayed +
  geom_point(aes(fill=bias_direction), alpha=0, show.legend = TRUE) +
  scale_fill_manual(name = 'Bias direction', 
                    values = c(1, 1),
                    breaks = c('long', 'short'),
                    labels = c('Long', 'Short')) +
  guides(
    color='none',
    fill=guide_legend(title='Bias direction', 
                      override.aes = list(linetype=c(1,2),
                                          color=c('black', lighten('black', .6)))),
    linetype='none') +
  theme(legend.position = 'bottom')

# Plot reproduction ---------------------------------------------------------

## Concurrent ---------------------------------

p_reproduction_concurrent <- 
  basic_curve_plot(curve_data = dat_reproduction_concurrent %>% filter(target_length >= 365 & target_length <= 435), 
                   empirical_data = dat_concurrent %>% filter(trial_type=='reproduction')) +
  ggtitle('Reproduction task')

# Get target length associated with reference reproduction
rep_ref_concurrent <- get_ref_rep(dat_reproduction_concurrent)
rep_ref_concurrent_summary <- get_pse_summary(rep_ref_concurrent)

p_reproduction_concurrent <-
  add_pse_to_plot(base_plot = p_reproduction_concurrent, pse_data = rep_ref_concurrent_summary, 
                  pse_position = 'bottom', reproduction_plot = TRUE) +
  ylim(332,490) +
  ylab('Length reproduction') +
  xlab('Target length') +
  theme(axis.title.x = element_blank(),
        legend.position = 'none') +
  geom_text(data=get_bf_d(data = rep_ref_concurrent %>% select(-data)), 
            aes(x=400, y= 470, label=label), size=2.8, parse=TRUE, inherit.aes = FALSE) 


## delayed ---------------------------------

p_reproduction_delayed <- 
  basic_curve_plot(curve_data = dat_reproduction_delayed %>% filter(target_length >= 365 & target_length <= 435), 
                   empirical_data = dat_delayed %>% filter(trial_type=='reproduction')) +
  ggtitle('Reproduction task')

# Get target length associated with reference reproduction
rep_ref_delayed <- get_ref_rep(dat_reproduction_delayed)
rep_ref_delayed_summary <- get_pse_summary(rep_ref_delayed)

# Add PSE 
p_reproduction_delayed <-
  add_pse_to_plot(base_plot = p_reproduction_delayed, pse_data = rep_ref_delayed_summary, 
                  pse_position = 'bottom', reproduction_plot = TRUE) +
  ylim(332,490) +
  ylab('Length reproduction') +
  xlab('Target length') +
  theme(axis.title.x = element_blank(),
        legend.position = 'none') +
  geom_text(data=get_bf_d(data = rep_ref_delayed %>% select(-data)), 
            aes(x=400, y= 470, label=label), size=2.8, parse=TRUE, inherit.aes = FALSE)

# Combined reproduction data ---------------------------------------------

# Combine raw data
dat_reproduction_combined <- 
  dat_reproduction_concurrent %>% 
  bind_rows(dat_reproduction_delayed)

# Combine predicted values
rep_ref_combined <- 
  rep_ref_concurrent %>% 
  bind_rows(rep_ref_delayed)

# Get reproduction equivalent to reference line
rep_ref_combined_summary <- get_pse_summary(rep_ref_combined)
# Plot reproduction fit and empirical data
p_reproduction_combined <- basic_curve_plot(curve_data = dat_reproduction_combined %>% filter(target_length >= 365 & target_length <= 435), 
                                            empirical_data = bind_rows(dat_concurrent, dat_delayed) %>% filter(trial_type=='reproduction'))

# Modify plot, change axis to include just about the tested range of target lines.
# The fitted line goes beyond but we remove the rest here
p_reproduction_combined <-
  add_pse_to_plot(base_plot = p_reproduction_combined, pse_data = rep_ref_combined_summary, 
                  pse_position = 'bottom', reproduction_plot = TRUE) +
  ylim(332,490) +
  ylab('Length reproduction') +
  xlab('Target length') +
  theme(axis.title.x = element_blank(),
        legend.position = 'none') +
  geom_text(data=get_bf_d(data = rep_ref_combined %>% select(-data)), 
            aes(x=400, y= 460, label=label), size=2.8, parse=TRUE, inherit.aes = FALSE) +
  ggtitle('Reproduction task')

# Add bias direction legend
p_reproduction_combined <-
  p_reproduction_combined +
  geom_point(aes(fill=bias_direction), alpha=0, show.legend = TRUE) +
  scale_fill_manual(name = 'Bias direction',
                    values = get_condition_colors(),
                    breaks = c('mullerlyer_long', 'mullerlyer_short'),
                    labels = c('Long', 'Short')) +
  scale_color_manual(name = 'Bias direction', 
                     values = get_condition_colors(), 
                     breaks = c('mullerlyer_long', 'mullerlyer_short'),
                     labels = c('Long', 'Short')) +
  guides(color = guide_legend(ncol = 2, nrow = 1, byrow = TRUE, 
                              override.aes = list(alpha=c(1,1), 
                                                  fill=c('black', lighten('black', .6)), 
                                                  color=c('black', lighten('black', .6)))),
         linetype = 'none') +
  theme(legend.position = 'bottom')


# Note that values are removed in reproduction plot because
# the fitted lines were estimated beyond the range of stimuli tested.
# For plotting only values around the tested range of target lines are included.
p_reproduction_combined
ggsave(here('plots/combined_reproduction_curve.png'), width = 7, height = 5, dpi = 300, scale=.68, device=png)

# Combine bias, confidence and reproduction plot ----------------------------------------

# Note that values are removed in reproduction plot because
# the fitted lines were estimated beyond the range of stimuli tested.
# For plotting only values around the tested range of target lines are included.
p_decision_concurrent / p_reproduction_concurrent / p_confidence_concurrent
ggsave(here('plots/concurrent_all_results.png'), width = 6, height = 10, dpi = 1200, scale=.8, device=png)

# Combine bias, confidence and reproduction plot ----------------------------------------

# Note that values are removed in reproduction plot because
# the fitted lines were estimated beyond the range of stimuli tested.
# For plotting only values around the tested range of target lines are included.
p_decision_delayed / p_reproduction_delayed / p_confidence_delayed
ggsave(here('plots/delayed_all_results.png'), width = 6, height = 10, dpi = 1200, scale=.8, device=png)

