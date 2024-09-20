# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(rmarkdown)  

render('scripts/ordinal_modelling_decision.Rmd', param=list(task='decision'), output_file = 'ordinal_models_decision.html')
render('scripts/ordinal_modelling_decision.Rmd', param=list(task='confidence'), output_file = 'ordinal_models_confidence.html')
render('scripts/ordinal_modelling_reproduction.Rmd', output_file = 'ordinal_models_reproduction.html')
