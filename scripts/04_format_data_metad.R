# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr)  

# Data --------------------------------------------------------------------

# each experiment data
dat_concurrent <- read_csv('data/processed/filtered_concurrent.csv', col_types = cols())
dat_delayed <- read_csv('data/processed/filtered_delayed.csv', col_types = cols())

# Combine data
all_data <- bind_rows(dat_concurrent, dat_delayed)
rm(dat_concurrent, dat_delayed)

# Filter data -------------------------------------------------------------

# keep only decision trials and remove trials where the
# target length is equal to the reference
all_data <- all_data %>% 
  filter(trial_type == 'decision', line_type != 'equal')

# Format data -------------------------------------------------------------

# Create labels
all_data <- 
  all_data %>% 
  mutate(stim_label = case_when(answer == 'long' ~ 's1',
                                answer == 'short' ~ 's2'),
         conf_label = case_when(confidence == 'high' ~ '2',
                                confidence == 'low' ~ '1'),
         label = paste0(stim_label, '_', conf_label))

# Count labels
metad_data <- 
  all_data %>% 
  group_by(confidence_type, participant, bias_source, bias_direction, line_type, label) %>% 
  reframe(count=n())

# Some stim/conf combinations were not used, set them to zero
metad_data <- 
  metad_data %>% 
  pivot_wider(names_from = label, values_from = count, values_fill = 0) %>% 
  pivot_longer(names_to = 'label', values_to = 'value', cols = matches('s[12]'))


# Save data ---------------------------------------------------------------

write_csv(metad_data, 'data/processed/all_data_metad.csv')

