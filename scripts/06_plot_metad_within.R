# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, rjags, ggpubr, ggmcmc, BayesFactor, effectsize, bayestestR, tidybayes, patchwork)  

set.seed(666)

# Functions ---------------------------------------------------------------

# custom made functions and colors for plotting
source('scripts/funcs.R')

# loads the metad' fits 
load_metad_output <- function(file_name) {
  # get condition name from file name
  cond_data <- str_split(str_extract(file_name, '(?<=output_).*(?=\\.)'), pattern = '_', simplify = TRUE)
  # set variables with condition info
  conf_type <- cond_data[1]
  bias_source <- cond_data[2]
  
  # read fit
  assign('fit', get(load(file = file_name, envir = environment())))  
  # get posteriors
  draws <- ggs(fit) %>% 
    filter(Parameter %in% c('mu_logMratio[1]', 'mu_logMratio[2]', 'rho', 'cohen_d'))
  # add cond info
  draws <- 
    draws %>% 
    mutate(confidence_type = conf_type, bias_source = bias_source)
  # format df
  draws <- 
    draws %>% 
    mutate(Parameter = recode(Parameter, `mu_logMratio[1]`='logMratio_long', `mu_logMratio[2]`='logMratio_short'))
  
  return(draws)
}

# check r-hat values
check_rhat <- function(file_name) {
  # get condition name from file name
  cond_data <- str_split(str_extract(file_name, '(?<=output_).*(?=\\.)'), pattern = '_', simplify = TRUE)
  # set variables with condition info
  conf_type <- cond_data[1]
  bias_source <- cond_data[2]
  
  # read fit
  assign('fit', get(load(file = file_name, envir = environment())))  
  
  # Rhat 
  value <- gelman.diag(fit, confidence = 0.95)
  data.frame(conv = value$psrf) %>% 
    as_tibble(rownames = 'name') %>% 
    select(name, conv.Point.est.) %>% 
    filter(str_detect(name, 'mu_logMratio')) %>% 
    mutate(confidence_type = conf_type, bias_source=bias_source)
}


# Check r-hat -------------------------------------------------------------

dir('data/metad/', pattern = 'rds', full.names = TRUE) %>% 
  map(check_rhat)

# Data --------------------------------------------------------------------

# Get files
files <-
  dir('data/metad/', pattern = 'rds', full.names = TRUE)

# Load fit
draws_df <- map(files, load_metad_output)

# Bind dfs
draws_df <- draws_df %>% bind_rows()

# Get m-ratio -------------------------------------------------------------

# Create factors to arrange plot
draws_logmratio <-
  draws_df %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff'))) %>% 
  filter(str_detect(Parameter, 'logMratio')) %>% 
  rename(bias_direction=Parameter) %>% 
  mutate(bias_direction = str_remove(bias_direction, 'logMratio_'))

# Get cohen-d -------------------------------------------------------------

# Get sample of cohen's d
draws_df_cohens <- 
  draws_df %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff'))) %>% 
  filter(Parameter == 'cohen_d') 

# Nest data to run Savage-Dickey test on each nest
nested_cohend_data <- 
  draws_df_cohens %>% 
  filter(Parameter == 'cohen_d') %>% 
  select(confidence_type, bias_source, value) %>% 
  group_by(confidence_type, bias_source) %>% 
  nest()

# Generate prior distribution
prior <- distribution_cauchy(n=14000, location = 0, scale=.707)

# Get BF using Savage-Dickey density ratio
bf_data <-
  nested_cohend_data %>%
  mutate(bf = exp(unlist(map(data, ~bayesfactor_pointnull(posterior=.x$value, prior=prior)$log_BF))))

# Summary data for plotting
cohend_data_summ <- 
  draws_df_cohens %>% 
  filter(Parameter == 'cohen_d') %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff'))) %>% 
  group_by(confidence_type, bias_source) %>% 
  reframe(cohen_d = round(mean(value), 2))

# Labels for plotting BF and cohen's d
bf_data <-
  bf_data %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff'))) %>% 
  full_join(cohend_data_summ) %>% 
  mutate(
    bf_lab = paste0('bold(BF[10]) == "', shorter_bf_value(bf), '"'),
    d_lab = paste0('italic(d) == ', round(cohen_d,2)),
    label = paste0('atop(', bf_lab, ', ', 
                   d_lab,')')
  )

# Plot ----------------------------------------------------------`----------

# log m-ratio summary 
draws_logmratio_summary <- 
  draws_logmratio %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  reframe(value=mean(value))

# high density interval
hdi <- 
  draws_logmratio %>% 
  mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff'))) %>% 
  group_by(confidence_type, bias_source, bias_direction) %>% 
  nest() %>% 
  mutate(hdi = map(data, ~hdi(.x$value))) %>% 
  mutate(hdi_lb = unlist(map(hdi, ~.x[,1])),
         hdi_ub = unlist(map(hdi, ~.x[,2]))) %>% 
  select(-c(data, hdi))

# Base distribution plot by bias source and bias direction
p_mcmc <-
  draws_logmratio %>%
  ggplot(aes(exp(value), group = bias_direction)) +
  facet_wrap(. ~ confidence_type + bias_source, 
             labeller = labeller(confidence_type = function(x) paste(str_to_title(x), 'confidence'), 
                                 bias_source = label_bias_source, .multi_line = FALSE)) +
  geom_histogram(aes(fill=interaction(bias_source, bias_direction, sep = '_'),
                     color=interaction(bias_source, bias_direction, sep = '_')),
                 binwidth = 0.03,  colour = "grey", alpha = 0.5, 
                 position = 'identity', show.legend = TRUE)  +
  scale_fill_manual(values=get_condition_colors()) +
  scale_color_manual(values=get_condition_colors()) +
  theme(legend.position = 'none') +
  geom_segment(data=draws_logmratio_summary, 
               aes(x=exp(value), y=-Inf, yend=Inf, color=interaction(bias_source, bias_direction, sep = '_')),
               linewidth=1, linetype=2) +
  geom_segment(data = hdi %>% filter(bias_direction=='long'), 
               aes(x=exp(hdi_lb), xend=exp(hdi_ub), y = 200, yend=200, 
                   color=interaction(bias_source, bias_direction, sep = '_')),
               linewidth=2, show.legend = FALSE) +
  geom_segment(data = hdi %>% filter(bias_direction=='short'),
               aes(x=exp(hdi_lb), xend=exp(hdi_ub), y = 0, yend=0,
                   color=interaction(bias_source, bias_direction, sep = '_')),
               linewidth=2, show.legend = FALSE) +
  ylab('Posterior sample count') + xlab('M-ratio')

# aesthetics
p_mcmc <-
  p_mcmc +
  scale_linetype_discrete(labels = c('Long', 'Short')) + 
  guides(
    color = 'none',
    fill = 'none',
    linetype = guide_legend(title = 'Bias direction', 
                            override.aes = list(linetype=c(0,0),
                                                fill=c('black', lighten('black', .6)),
                                                color=c('black', lighten('black', .6)))))

# add BF to plot
p_mcmc <-
  p_mcmc + 
  geom_text(data=bf_data, 
            aes(x=c(1.04, # concurrent payoff (1, 3)
                    1.05, # concurrent muller-lyer (2, 2)
                    1.05, # concurrent, payoff
                    .9, # delayed, payoff (2, 3) 
                    1, # delayed base rate (2, 1) 
                    1.05 # concurrent muller-lyer (1,2 )
            ), y= 7800, label=label), 
            size=2.8, parse=TRUE, inherit.aes = FALSE)


p_mcmc
# save plot
ggsave('plots/mratio_all_conditions_within.png',
       device = png,
       width = 8, height = 5, dpi=600, scale=.9)

