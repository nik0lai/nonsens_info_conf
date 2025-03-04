# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(rmarkdown, job)  

# Each job call will submit a background job. 
# If the entire script is run all jobs will 
# run simultaneously. Mind your RAM.

# Decision task -----------------------------------------------------------

job::job({
  render('scripts/ordinal_model_scripts/ordinal_modelling_decision.Rmd', param=list(task='decision', exp_data='both'), output_file = 'output/ordinal_models_decision_both.html')
})

# Confidence --------------------------------------------------------------

job::job({
  render('scripts/ordinal_model_scripts/ordinal_modelling_decision.Rmd', param=list(task='confidence', exp_data='both'), output_file = 'output/ordinal_models_confidence_both.html')
})

# Reproduction ------------------------------------------------------------

job::job({
  render('scripts/ordinal_model_scripts/ordinal_modelling_reproduction.Rmd', param=list(task='reproduction', exp_data='both'), output_file = 'output/ordinal_models_reproduction_both.html')
})

# Decision and confidence -------------------------------------------------

job::job({
  render('scripts/ordinal_model_scripts/ordinal_modelling_diff_dec_conf.Rmd', param=list(exp_data='exp1'), output_file = 'output/ordinal_modelling_diff_dec_conf_exp1.html')
})
job::job({
  render('scripts/ordinal_model_scripts/ordinal_modelling_diff_dec_conf.Rmd', param=list(exp_data='exp2'), output_file = 'output/ordinal_modelling_diff_dec_conf_exp2.html')
})
job::job({
  render('scripts/ordinal_model_scripts/ordinal_modelling_diff_dec_conf.Rmd', param=list(exp_data='both'), output_file = 'output/ordinal_modelling_diff_dec_conf_both.html')
})

