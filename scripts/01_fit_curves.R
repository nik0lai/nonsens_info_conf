# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(readr, dplyr, tidyr, purrr, BayesFactor, bayestestR, ggplot2)

source('scripts/funcs.R')

# Data --------------------------------------------------------------------

dat <- bind_rows(read_csv('data/processed/filtered_concurrent.csv', col_types = cols()), 
                 read_csv('data/processed/filtered_delayed.csv', col_types = cols()))


# Decision data -----------------------------------------------------------

## Separate discrimination data ----
decision_data <- 
  dat %>% 
  filter(trial_type == 'decision') %>% 
  select(confidence_type, participant, bias_source, bias_direction, target_length, answer) %>% 
  mutate(answer = recode(answer, 'short' = 0, 'long' = 1))

# Fit logistic regression model for each experiment, bias source, bias dir condition
decision_glm_data <- 
  decision_data %>% 
  group_by(confidence_type, participant, bias_source, bias_direction) %>% 
  nest() %>%
  mutate(fit = map(data,  ~glm(answer ~ target_length, data = .x, family = 'binomial')))

## Cross validate fit ----

# Leave one out cross validation. Using all but one subject the 
# predicted answer for each target length is calculated and error
# is calculated against the left-out subject. Same is done for 
# null model (average across all target lengths).

# Get fit value
decision_glm_fitted_values <- 
  decision_glm_data %>% 
  ungroup() %>% 
  mutate(fitted_values = map(fit, ~ .x$fitted.values)) %>% 
  select(-fit) %>% 
  unnest(c(data, fitted_values)) 

# Calculate error fit
decision_crossvalidation <- 
  # Nest the data of each bias source and direction condition
  decision_glm_fitted_values %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  nest() %>% 
  ungroup() %>% 
  
  # Map over each nest to cross validate fit
  pmap(get_crossvalidation) %>% bind_rows()

## Test curve fit and null model error ----

# Test curve fit error against null model (average) error
decision_crossvalidation_bf <- 
  decision_crossvalidation %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  nest() %>% 
  mutate(ttest = map(data, ~ ttestBF(x = .x$error_curve, y = .x$error_null, paired = TRUE, rscale = .707))) %>% 
  ungroup()

# Format bf values and create label
decision_crossvalidation_bf <-
  decision_crossvalidation_bf %>%
  mutate(bf = unlist(map(ttest, ~describe_posterior(.x)$BF))) %>% 
  rowwise() %>% 
  mutate(bf_format = shorter_bf_value(bf),
         bf_label = paste0('bold(BF[10]) == "', bf_format, '"'))

## Plot curve fit and null model error -----

decision_crossvalidation %>% 
  pivot_longer(names_to = 'error', values_to = 'value', cols = c(error_curve, error_null)) %>% 
  
  ggplot(aes(x=interaction(bias_source, bias_direction, sep = '\n'), y=value)) +
  facet_wrap(. ~ confidence_type, ncol = 1) + 
  geom_point(aes(color=error), position = position_dodge(width = .3)) +
  stat_summary(aes(group=error), fun=mean, color='black', position = position_dodge(width = .3)) +
  stat_summary(aes(group=error), 
               fun=mean, 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               color='black', geom = 'errorbar', width = .3,
               position = position_dodge(width = .3)) +
  geom_text(data=decision_crossvalidation_bf, 
            aes(x=interaction(bias_source, bias_direction, sep = '\n'), 
                y = .3, label = bf_label), parse = TRUE) +
  theme(legend.position = 'bottom') +
  xlab('Condition') + ylab('MSE') +
  ggtitle('Decision task - Model fitting cross-validation') +
  ylim(-.01, .35) +
  scale_color_discrete(labels = c('null' = 'Mean', 'sigmoid' = 'Curve fit')) +
  labs(color='Model')

ggsave('plots/categorization_fit_crossvalidation.png', width = 10, height = 7, scale=.8)

## Predict full curve ----

decision_predicted_curve <- predict_full_fit(fits = decision_glm_data %>% select(confidence_type, participant, bias_source, bias_direction, data, fit))

# Save PSE data
unique(decision_predicted_curve$confidence_type) %>%
  walk(., ~
         filter(decision_predicted_curve, confidence_type == .x) %>%
         select(-c(data,fit)) %>%
         write_csv(., sprintf('data/processed/decision_predicted_curve_%s.csv', .x))
  )

# Confidence data ---------------------------------------------------------

# Separate confidence data
confidence_data <-
  dat %>% 
  filter(trial_type == 'decision') %>% 
  select(confidence_type, participant, bias_source, bias_direction, target_length, confidence) %>% 
  mutate(confidence = recode(confidence, 'low' = 0, 'high' = 1))

# Fit logistic regression model for each experiment, bias source, bias dir condition
confidence_lm_data <-
  confidence_data %>% 
  group_by(confidence_type, participant, bias_source, bias_direction) %>% 
  nest() %>%
  mutate(fit = map(data,  ~lm(confidence ~ poly(x = target_length, degree = 2, raw =TRUE), data = .x)))

## Cross validate fit ----

# Leave one out cross validation. Using all but one subject the 
# predicted answer for each target length is calculated and error
# is calculated against the left-out subject. Same is done for 
# null model (average across all target lengths).

# Get fit value
confidence_lm_fitted_values <- 
  confidence_lm_data %>% 
  ungroup() %>% 
  mutate(fitted_values = map(fit, ~ .x$fitted.values)) %>% 
  select(-fit) %>% 
  unnest(c(data, fitted_values)) 

# Calculate error fit
confidence_crossvalidation <- 
  # Nest the data of each bias source and direction condition
  confidence_lm_fitted_values %>% 
  rename(answer = confidence) %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  nest() %>% 
  ungroup() %>% 
  
  # Map over each nest to cross validate fit
  pmap(get_crossvalidation) %>% 
  bind_rows()

## Test curve fit and null model error ----

# Test curve fit error against null model (average) error
confidence_crossvalidation_bf <- 
  confidence_crossvalidation %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  nest() %>% 
  mutate(ttest = map(data, ~ ttestBF(x = .x$error_curve, y = .x$error_null, paired = TRUE, rscale = .707))) %>% 
  ungroup()

# Format bf values and create label
confidence_crossvalidation_bf <-
  confidence_crossvalidation_bf %>%
  mutate(bf = unlist(map(ttest, ~describe_posterior(.x)$BF))) %>% 
  rowwise() %>% 
  mutate(bf_format = shorter_bf_value(bf),
         bf_label = paste0('bold(BF[10]) == "', bf_format, '"'))

## Plot curve fit and null model error -----

confidence_crossvalidation %>% 
  pivot_longer(names_to = 'error', values_to = 'value', cols = c(error_curve, error_null)) %>% 
  
  ggplot(aes(x=interaction(bias_source, bias_direction, sep = '\n'), y=value)) +
  facet_wrap(. ~ confidence_type, ncol = 1) + 
  geom_point(aes(color=error), position = position_dodge(width = .3)) +
  stat_summary(aes(group=error), fun=mean, color='black', position = position_dodge(width = .3)) +
  stat_summary(aes(group=error), 
               fun=mean, 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               color='black', geom = 'errorbar', width = .3,
               position = position_dodge(width = .3)) +
  geom_text(data=confidence_crossvalidation_bf, 
            aes(x=interaction(bias_source, bias_direction, sep = '\n'), 
                y = .3, label = bf_label), parse = TRUE) +
  theme(legend.position = 'bottom') +
  xlab('Condition') + ylab('MSE') +
  ggtitle('Confidence task - Model fitting cross-validation') +
  ylim(-.01, .35) +
  scale_color_discrete(labels = c('null' = 'Mean', 'sigmoid' = 'Curve fit')) +
  labs(color='Model')

ggsave('plots/confidence_fit_crossvalidation.png', width = 10, height = 7, scale=.8)

## Predict full curve ----

confidence_predicted_curve <- predict_full_fit(fits = confidence_lm_data %>% select(confidence_type, participant, bias_source, bias_direction, data, fit))

# Save PSE data
unique(confidence_predicted_curve$confidence_type) %>%
  walk(., ~
         filter(confidence_predicted_curve, confidence_type == .x) %>%
         select(-c(data,fit)) %>%
         write_csv(., sprintf('data/processed/confidence_predicted_curve_%s.csv', .x))
  )
       
# Reproduction data -------------------------------------------------------

# Separate reproduction data
reproduction_data <-
  dat %>% 
  filter(trial_type == 'reproduction') %>% 
  select(confidence_type, participant, bias_source, bias_direction, target_length, answer) %>% 
  mutate(answer = abs(as.integer(answer)))

# Fit logistic regression model for each experiment, bias source, bias dir condition
reproduction_lm_data <- 
  reproduction_data %>% 
  group_by(confidence_type, participant, bias_source, bias_direction) %>% 
  nest() %>%
  mutate(fit = map(data,  ~lm(answer ~ target_length, data = .x)))

## Cross validate fit ----

# Leave one out cross validation. Using all but one subject the 
# predicted answer for each target length is calculated and error
# is calculated against the left-out subject. Same is done for 
# null model (average across all target lengths).

# Get fit value
reproduction_lm_fitted_values <- 
  reproduction_lm_data %>% 
  ungroup() %>% 
  mutate(fitted_values = map(fit, ~ .x$fitted.values)) %>% 
  select(-fit) %>% 
  unnest(c(data, fitted_values)) 

# Calculate error fit
reproduction_crossvalidation <- 
  # Nest the data of each bias source and direction condition
  reproduction_lm_fitted_values %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  nest() %>% 
  ungroup() %>% 
  
  # Map over each nest to cross validate fit
  pmap(get_crossvalidation) %>% 
  bind_rows()

## Test curve fit and null model error ----

# Test curve fit error against null model (average) error
reproduction_crossvalidation_bf <- 
  reproduction_crossvalidation %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  nest() %>% 
  mutate(ttest = map(data, ~ ttestBF(x = .x$error_curve, y = .x$error_null, paired = TRUE, rscale = .707))) %>% 
  ungroup()

# Format bf values and create label
reproduction_crossvalidation_bf <-
  reproduction_crossvalidation_bf %>%
  mutate(bf = unlist(map(ttest, ~describe_posterior(.x)$BF))) %>% 
  rowwise() %>% 
  mutate(bf_format = shorter_bf_value(bf),
         bf_label = paste0('bold(BF[10]) == "', bf_format, '"'))

## Plot curve fit and null model error -----

reproduction_crossvalidation %>% 
  pivot_longer(names_to = 'error', values_to = 'value', cols = c(error_curve, error_null)) %>% 
  
  ggplot(aes(x=interaction(bias_source, bias_direction, sep = '\n'), y=value)) +
  facet_wrap(. ~ confidence_type, ncol = 1) + 
  geom_point(aes(color=error), position = position_dodge(width = .3)) +
  stat_summary(aes(group=error), fun=mean, color='black', position = position_dodge(width = .3)) +
  stat_summary(aes(group=error), 
               fun=mean, 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               color='black', geom = 'errorbar', width = .3,
               position = position_dodge(width = .3)) +
  geom_text(data=reproduction_crossvalidation_bf, 
            aes(x=interaction(bias_source, bias_direction, sep = '\n'), 
                y = 5009, label = bf_label), parse = TRUE) +
  theme(legend.position = 'bottom') +
  xlab('Condition') + ylab('MSE') +
  ggtitle('Reproduction task - Model fitting cross-validation') +
  scale_color_discrete(labels = c('null' = 'Mean', 'sigmoid' = 'Curve fit')) +
  labs(color='Model')

ggsave('plots/reproduction_fit_crossvalidation.png', width = 10, height = 7, scale=.8)

## Predict full curve ----

reproduction_predicted_curve <- predict_full_fit(fits = reproduction_lm_data %>% select(confidence_type, participant, bias_source, bias_direction, data, fit))

# Save PSE data
unique(reproduction_predicted_curve$confidence_type) %>%
  walk(., ~
         filter(reproduction_predicted_curve, confidence_type == .x) %>%
         select(-c(data,fit)) %>%
         write_csv(., sprintf('data/processed/reproduction_predicted_curve_%s.csv', .x))
  )
