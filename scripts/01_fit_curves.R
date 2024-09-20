# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(readr, dplyr, tidyr, purrr, BayesFactor, bayestestR, ggplot2, stringr)

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

## Predict full curve ----

reproduction_predicted_curve <- predict_full_fit(fits = reproduction_lm_data %>% select(confidence_type, participant, bias_source, bias_direction, data, fit))

# Save PSE data
unique(reproduction_predicted_curve$confidence_type) %>%
  walk(., ~
         filter(reproduction_predicted_curve, confidence_type == .x) %>%
         select(-c(data,fit)) %>%
         write_csv(., sprintf('data/processed/reproduction_predicted_curve_%s.csv', .x))
  )
