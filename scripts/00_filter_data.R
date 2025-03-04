# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(readr, dplyr, tidyr, purrr)

# Custom functions
source('scripts/funcs.R')

# Data --------------------------------------------------------------------

# Combine both data sets because same filtering is done on both
dat <- bind_rows(read_csv('data/raw_concurrent.csv', col_types = cols()), 
                 read_csv('data/raw_delayed.csv', col_types = cols()))

# Count subjects pre-filtering --------------------------------------------

# confidence_type   bias_source     n
# 1 concurrent      baserate       51
# 2 concurrent      mullerlyer     36
# 3 concurrent      payoff         35
# 4 delayed         baserate       51
# 5 delayed         mullerlyer     36
# 6 delayed         payoff         36

dat %>% 
  select(confidence_type, participant, bias_source) %>% 
  distinct() %>% 
  filter(!is.na(participant)) %>% 
  group_by(confidence_type, bias_source) %>% 
  summarise(n = n())

# Total subjects per experiment

# confidence_type       n
# 1 concurrent        122
# 2 delayed           123

dat %>% 
  select(confidence_type, participant, bias_source) %>% 
  distinct() %>% 
  filter(!is.na(participant)) %>% 
  group_by(confidence_type) %>% 
  summarise(n = n())

# Count trials ------------------------------------------------------------

dat %>% 
  group_by(confidence_type, participant, bias_source) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  select(bias_source, count) %>% 
  distinct()

# Get SDT data ------------------------------------------------------------

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

# Filtering ---------------------------------------------------------------

# All the filtering is done by removing subjects with sensitivity (d') and
# bias (criterion) beyond four standard deviation of the grand mean of each
# experiment. That is, across the three bias source within each confidence
# type. Participants with sensitivity lower or equal to zero are also filtered.

# Participants that are outliers in one bias direction condition are 
# removed completely (both bias directions.)

sd_threshold <- 4

## low d' --------------

# Get low d' subjects
low_d_subs <- 
  sdt_data %>% 
  filter(d <= 0)

# Remove low d' subs (participants are removed sequentually so these
# outliers are not included in the next filtering)
dat <- dat %>% 
  anti_join(low_d_subs %>% select(confidence_type, participant, bias_source))
sdt_data <- sdt_data %>% 
  anti_join(low_d_subs %>% select(confidence_type, participant, bias_source))

## d' outliers --------------

# Here there are no d' outliers so no participant is removed

# Separate d' data
d_data <-
  sdt_data %>% 
  select(confidence_type, participant, bias_source, bias_direction, d) %>% 
  distinct() %>% 
  rename(value = d) %>% 
  ungroup() %>% 
  group_by(confidence_type) %>% 
  nest()

# Get d' outliers
d_outliers <- 
  d_data %>% 
  mutate(outliers = map(data, ~outlier_filtering(.x, sd_threshold))) %>% 
  select(-data) %>% 
  unnest(outliers) %>% 
  ungroup()

## reproduction outliers --------------

rep_data <-
  dat %>%
  filter(trial_type == 'reproduction') %>% 
  mutate(value = abs(as.integer(answer)) - as.integer(target_length)) %>% 
  group_by(confidence_type, participant, bias_source, bias_direction) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

# Get reproduction outliers
rep_outliers <- 
  rep_data %>%
  group_by(confidence_type) %>% 
  nest() %>% 
  mutate(outliers = map(data, ~outlier_filtering(.x, sd_threshold))) %>% 
  select(-data) %>% 
  unnest(outliers) %>% 
  ungroup()

# Remove outliers
dat <-
  dat %>% 
  anti_join(rep_outliers %>% select(confidence_type, participant, bias_source))

# Filtering summary -------------------------------------------------------

bind_rows(low_d_subs,
          d_outliers,
          rep_outliers)

# Count filtered subjects -------------------------------------------------
 
# confidence_type   bias_source     n
# 1 concurrent      baserate       50
# 2 concurrent      mullerlyer     35
# 3 concurrent      payoff         35
# 4 delayed         baserate       50
# 5 delayed         mullerlyer     35
# 6 delayed         payoff         35

dat %>% 
  select(confidence_type, participant, bias_source) %>% 
  distinct() %>% 
  filter(!is.na(participant)) %>% 
  group_by(confidence_type, bias_source) %>% 
  summarise(n = n())

# Save data ---------------------------------------------------------------

write_csv(dat %>% filter(confidence_type == 'concurrent') %>% select(-confidence_rt), 'data/processed/filtered_concurrent.csv')
write_csv(dat %>% filter(confidence_type == 'delayed'), 'data/processed/filtered_delayed.csv')
