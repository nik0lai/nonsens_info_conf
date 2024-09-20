# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr)  

source("scripts/metad/fit_metad_groupcorr.R")

# High - conf Stim 1
# Low  - conf Stim 1
# Low  - conf Stim 2
# High - conf Stim 2

# seed for reproducibility
set.seed(666)

# Functions ---------------------------------------------------------------

make_dataframes <- function(x) {
  x %>% 
    mutate(label = factor(label, levels =c('s1_2', 's1_1', 's2_1', 's2_2'))) %>% 
    arrange(participant, label) %>% 
    pivot_wider(names_from = participant, values_from = value) %>% 
    select(-label) %>%
    as.data.frame()
}

get_metad <- function(confidence_type, bias_source, data) {
  
  # model inputs
  nR_S1 <- list(filter(data, bias_direction == 'long', line_type=='long')$data[[1]],
                filter(data, bias_direction == 'short', line_type=='long')$data[[1]])
  
  nR_S2 <- list(filter(data, bias_direction == 'long', line_type=='short')$data[[1]],
                filter(data, bias_direction == 'short', line_type=='short')$data[[1]])
  
  # fit group meta d'
  output <- fit_metad_groupcorr(nR_S1 = nR_S1, nR_S2 = nR_S2)
  
  # save fit ----------------------------------------------------------------
  
  # make file name
  fit_file_name <- sprintf('data/metad/output_%s_%s.rds', confidence_type, bias_source)

  # save fit
  save('output', file = fit_file_name)
}

# Data --------------------------------------------------------------------

dat <- read_csv('data/processed/all_data_metad.csv')

# Format data

# Create data frame with stim/confidence combination for each condition
nest_data <- 
  dat %>% 
  group_by(confidence_type, bias_source, bias_direction, line_type) %>% 
  nest() %>% 
  mutate(data = map(data, make_dataframes)) %>% 
  group_by(confidence_type, bias_source) %>% 
  nest()
  
# Fit meta d' ---------------------------------------------------------------

# if you talk the talk, you have to walk the walk
pwalk(nest_data, get_metad)

