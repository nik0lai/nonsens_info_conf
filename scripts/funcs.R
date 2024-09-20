p_load(tidyr)


# Labels ------------------------------------------------------------------

# Bias source label
label_bias_source  <-  c('baserate' = 'Base-rate', 
                      'mullerlyer' = 'Müller-Lyer',
                      'payoff' = 'Payoff')
# Bias direction label
label_bias_direction <-  c('long'='Long', 'short'='Short')

# Experiment
label_experiments <- c('concurrent'='Concurrent confidence', 'delayed'='Delayed confidence')

# Bias source and bias direction label
label_bias_source_direction <- c('mullerlyer_short'='Müller-Lyer biased-to-short', 
                                 'mullerlyer_long'='Müller-Lyer biased-to-long', 
                                 'baserate_short'='Base rate biased-to-short', 
                                 'baserate_long'='Base rate biased-to-long', 
                                 'payoff_short'='Payoff biased-to-short', 
                                 'payoff_long'='Payoff biased-to-long')

# Functions ---------------------------------------------------------------

# Calculate signal detection theory criterion and d'
sdt_calc <- 
  function (hits, misses, crrej, far) 
  {
    if (0 %in% c(hits, misses, crrej, far)) {
      hits = hits + 0.5
      misses = misses + 0.5
      crrej = crrej + 0.5
      far = far + 0.5
    }
    
    # Get hit and false alarm rate 
    hr <- hits/(hits + misses)
    fr <- far/(far + crrej)
    
    # Get SDT measures
    lambda <- qnorm(fr)
    d <- qnorm(hr) - lambda
    ccrit <- (qnorm(hr) + qnorm(fr))/2
    beta <- d * (-lambda - d * 1/2)
    return(tibble(hr, fr, lambda, d, ccrit, beta))
  }

# This function takes a dataframe with at least one column
# named value and a standard deviation threshold. It will 
# output the rows that lay outside the mean + the number of
# standard deviations input
outlier_filtering <- function(data, sd_threshold) {
  
  # Calculate thresholds and mark outliers
  outliers <- 
    data %>%
    mutate(mean = mean(value),
           upper_boundary = mean + (sd(value) * sd_threshold),
           lower_boundary = mean - (sd(value) * sd_threshold)) %>% 
    mutate(outlier = value > upper_boundary | value < lower_boundary) %>% 
    filter(outlier)
  
  return(outliers)
  
}

predict_full_fit <- function(fits) {
  
  # target length values to predict
  possible_target_lengths <- seq(365, 435, .1)
  
  # predict values using fit
  fits %>% 
    mutate(fit_curve = 
             map(fit, ~ as_tibble(predict(object = .x, 
                                          newdata = data.frame(target_length = possible_target_lengths), 
                                          type = 'response')) %>% 
                   mutate(target_length = possible_target_lengths)
             )) %>% 
    ungroup() %>% 
    unnest(cols = fit_curve)
  
}

get_fit_error <- function(empirical, predicted) {
  # Calculate error
  full_join(
    # Empirical data
    empirical %>% group_by(target_length) %>% summarise(answer = mean(answer)),
    # Curve fit prediction
    predicted %>% group_by(target_length) %>% summarise(curve_fit = mean(fitted_values)),
    by = 'target_length'
  ) %>% 
    # Add null model prediction
    mutate(null_model = predicted %>% summarise(null_model = mean(answer)) %>% pull()) %>% 
    # Calculate error
    mutate(error_curve = (answer-curve_fit)^2,
           error_null = (answer-null_model)^2) %>% 
    # Summary
    summarise(across(matches('error'), ~mean(.x)))
}

get_crossvalidation <- function(...) {
  
  # Get current nest data and condition info
  current <- tibble(...)
  c_confidence_type <- unique(current$confidence_type)
  c_bias_source <- unique(current$bias_source)
  c_bias_direction <- unique(current$bias_direction)
  c_data <- current$data
  
  # Map over subjects
  map(unique(c_data$participant), function(x) {
    
    # Predicted data (fitted values of all subjects but one)
    predicted_data <- c_data %>% filter(participant!=x)
    # Empirical data (empirical responses of one subject)
    empirical_data <- c_data %>% filter(participant==x)
    
    # Get fit error
    get_fit_error(predicted = predicted_data, empirical = empirical_data) %>% 
      mutate(participant = x)
    
  }) %>% 
    # Combine each subject error and add condition info
    bind_rows() %>% 
    mutate(confidence_type = c_confidence_type,
           bias_source = c_bias_source,
           bias_direction = c_bias_direction)
}

shorter_bf_value <- function(bf) {
  if (bf < 3) {
    bf <- round(bf, 2)
  } else if (bf > 1) {
    if (nchar(round(bf)) > 3) {
      bf <- formatC(bf, format = "e", digits = 1)
    } else if (round(bf) > 1) {
      bf <- round(bf)
    } else {
      bf <- round(bf, 2)
    }
  }
  return(as.character(bf))
}

factor_bias_source <- function(data) {
  data %>% mutate(bias_source = factor(bias_source, levels = c('mullerlyer', 'baserate', 'payoff')))  
}  

# Get colors for each condition
get_condition_colors = function(sep='_') {
  p_load(colorspace)
  
  # Get base color
  base_colors <- gg_color_hue(3)
  
  # Assign colors and brightness factor
  color_ml <- 2
  color_br <- 1
  color_po <- 3
  dark_factor <- .4
  light_factor <- .2
  
  # Dictionary with colors
  condition_colors <- c(
    'mullerlyer' = base_colors[color_ml],
    'baserate' = base_colors[color_br],
    'payoff' = base_colors[color_po],
    
    'mullerlyer.long' = darken(base_colors[color_ml], dark_factor),
    'mullerlyer.short' = lighten(base_colors[color_ml], light_factor),
    'baserate.long' = darken(base_colors[color_br], dark_factor),
    'baserate.short' = lighten(base_colors[color_br], light_factor),
    'payoff.long' = darken(base_colors[color_po], dark_factor),
    'payoff.short' = lighten(base_colors[color_po], light_factor),
    'simulated.long' = darken('violet', dark_factor),
    'simulated.short' = lighten('violet', light_factor)
    
  )
  
  # Apply separator
  return(set_names(condition_colors, str_replace(names(condition_colors), '\\.', sep)))
  
}

basic_curve_plot <- function(curve_data, empirical_data) {
  library('ggplot2')
  
  line_summary <- 
    curve_data %>% 
    group_by(bias_source, bias_direction, target_length) %>% 
    summarise(value = mean(value)) %>% 
    ungroup()
  
  points_summary <-
    empirical_data %>% 
    mutate(answer = as.integer(answer)) %>% 
    group_by(participant, bias_source, bias_direction, target_length) %>% 
    summarise(answer = mean(answer)) %>% 
    group_by(bias_source, bias_direction, target_length) %>% 
    summarise(se = sqrt(var(answer)/length(answer)), 
              answer = mean(answer))
  
  # Plot curve fitted values============================================
  p <-
    curve_data %>% 
    ggplot(., aes(x=target_length, y=value, group = interaction(participant, bias_direction))) +
    facet_wrap(. ~ bias_source, labeller = labeller(bias_source  = get_subject_count_labels(curve_data))) +
    scale_linetype_manual(values = c('short'=1, 'long'=5)) +
    # Single-subject fitted curves
    geom_line(aes(color=bias_source, linetype=bias_direction), alpha=.05, show.legend = FALSE) +
    # bias_source summary
    geom_line(data = line_summary, 
              aes(x=target_length, y=value, 
                  group=bias_direction, 
                  linetype=bias_direction,
                  color=interaction(bias_source, bias_direction ,sep = '_')), 
              size=.6,
              show.legend = TRUE) +
    scale_color_manual(values = get_condition_colors()) 
  
  # Plot real data average and error bars ============================================
  p <-
    p +
    geom_errorbar(data = points_summary,
                  aes(x=target_length, y=answer, 
                      ymin=answer-se, ymax=answer+se,
                      group=interaction(bias_direction, target_length),
                      color=interaction(bias_source, bias_direction, sep = '_')),
                  width=4,
                  show.legend = FALSE) +
    geom_point(data = points_summary, 
               aes(x=target_length, y=answer, 
                   group=interaction(bias_direction, target_length), 
                   color=interaction(bias_source, bias_direction, sep = '_')),
               show.legend = TRUE) 
  
  # assign linetype
  p <- p + 
    scale_linetype_manual(breaks = c('short', 'long'), labels=c('Short', 'Long'), values = c(2, 1))
  
  return(p)
}

get_subject_count <- function(data) {
  data %>%
    group_by(participant, bias_source) %>% 
    summarise(count = n()) %>% 
    group_by(bias_source) %>% 
    summarise(n = n())
}

get_subject_count_labels <- function(data, inverted=FALSE) {
  d <- get_subject_count(data) %>% 
    full_join(enframe(label_bias_source), by = c('bias_source' = 'name')) %>% 
    mutate(label = paste0(value, ' (N=', n,')')) %>% 
    select(bias_source, label) %>% 
    deframe()
  
  if (inverted) {
    deframe(select(enframe(d), 2:1))
  } else {
    d
  }
  
}

# Colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Get PSE
get_pse <- function(curve_data) {
  curve_data %>% 
    group_by(confidence_type, participant, bias_source, bias_direction) %>% 
    nest() %>% 
    mutate(target_length = unlist(map(data, ~approx(x = .x$value, y = .x$target_length, xout = .5)$y)))
  
}

get_pse_summary <- function(pse_data) {
  pse_data %>% 
    group_by(bias_source, bias_direction) %>% 
    filter(!is.na(target_length)) %>% 
    summarise(threshold = mean(target_length),
              sd = sd(target_length),
              se = sd/sqrt(n()))
}

add_pse_to_plot <- function(base_plot, pse_data, pse_position, reproduction_plot) {
  
  # Plot pse/lowest confidence estimation at top or bottom of plot
  if (pse_position == 'top') {
    if (reproduction_plot) {
      y_pos_pse_estimate <- 500
      y_end <- y_pos_pse_estimate + 18
    } else {
      y_pos_pse_estimate <- .9
      y_end <- 1
    }
  } else if (pse_position == 'bottom') {
    if (reproduction_plot) {
      y_pos_pse_estimate <- 350
      y_end <- y_pos_pse_estimate - 18
    } else {
      y_pos_pse_estimate <- .1
      y_end <-  -.05
    }
    
  } 
  
  if (reproduction_plot) {
    base_plot +
      geom_segment(data=pse_data, 
                   aes(x=threshold, xend=threshold, y=y_pos_pse_estimate,yend=y_end, 
                       color=interaction(bias_source, bias_direction, sep = '_'), linetype=bias_direction), 
                   inherit.aes = FALSE, show.legend = FALSE, size=.5) +
      geom_point(data=pse_data, aes(x=threshold, y=y_pos_pse_estimate,
                                    color=interaction(bias_source, bias_direction, sep = '_')), 
                 inherit.aes = FALSE, show.legend = FALSE, size=1) +
      geom_errorbarh(data=pse_data, aes(xmax=threshold+se, xmin=threshold-se, y=y_pos_pse_estimate, 
                                        color=interaction(bias_source, bias_direction, sep = '_')), 
                     height=6, size=1, inherit.aes = FALSE, show.legend = FALSE) 
    
  } else {
    base_plot +
      geom_segment(data=pse_data, 
                   aes(x=threshold, xend=threshold, y=y_pos_pse_estimate,yend=y_end, 
                       color=interaction(bias_source, bias_direction, sep = '_'), linetype=bias_direction), 
                   inherit.aes = FALSE, show.legend = FALSE, size=.5) +
      geom_point(data=pse_data, aes(x=threshold, y=y_pos_pse_estimate,
                                    color=interaction(bias_source, bias_direction, sep = '_')), 
                 inherit.aes = FALSE, show.legend = FALSE, size=1) +
      geom_errorbarh(data=pse_data, aes(xmax=threshold+se, xmin=threshold-se, y=y_pos_pse_estimate, 
                                        color=interaction(bias_source, bias_direction, sep = '_')), 
                     height=.05, inherit.aes = FALSE, show.legend = FALSE)
  }
}

get_bf_d <- function(data, rscale=.707) {
  library('BayesFactor')
  library('effectsize')
  
  data %>% 
    pivot_wider(names_from = bias_direction, values_from = target_length) %>% 
    filter(if_all(c(short,long), ~!is.na(.x))) %>% 
    group_by(bias_source) %>% 
    select(-participant,-matches('confidence_type')) %>% 
    nest(data = c(short, long)) %>%
    mutate(
      bf_ttest = map(data, ~ ttestBF(x = .x$short, y = .x$long, paired = TRUE, rscale = rscale, nullInterval = c(-Inf, 0))),
      effSize = map(data, ~ tibble(cohens_d(x = .x$short, y = .x$long, data = .x, paired = TRUE)))
    ) %>% 
    mutate(
      bf = round(exp(unlist(map(bf_ttest, ~ .x@bayesFactor$bf[2]))), 3),
      d = unlist(map(effSize, "Cohens_d"))
    ) %>% 
    mutate(bf = shorter_bf_value(bf)) %>% 
    mutate(
      bf_lab = paste0('bold(BF[10]) == "', bf, '"'),
      d_lab = paste0('italic(d) == ', round(d,2)),
      label = paste0('atop(', bf_lab, ', ', 
                     d_lab,')')
    )
  
}

# This is the same of the previous function with the exception that
# long is x and short is y. In the curve fitting results the direction
# of the effects was the opposite compared to the SDT results
get_bf_d_sdt <- function(data, cond_order, one_tail=TRUE, rscale=.707) {
  library('BayesFactor')
  library('effectsize')
  
  ttest_wrapper <- function(data, rscale, x, y) {
    x <- data[[x]]
    y <- data[[y]]
    ttestBF(x = x, y = y, paired = TRUE, rscale = rscale, nullInterval = c(-Inf, 0))
  }
  
  if (!one_tail) {
    ttest_wrapper <- function(data, rscale, x, y) {
      x <- data[[x]]
      y <- data[[y]]
      ttestBF(x = x, y = y, paired = TRUE, rscale = rscale)
    }
  }
  
  effSize_wrapper <- function(data, x, y) {
    x <- data[[x]]
    y <- data[[y]]
    cohens_d(x = x, y = y, paired = TRUE)
  }
  
  tmp <- data %>% 
    pivot_wider(names_from = bias_direction, values_from = target_length) %>% 
    filter(if_all(c(short,long), ~!is.na(.x))) %>% 
    group_by(bias_source) %>% 
    select(-participant,-matches('confidence_type')) %>% 
    nest(data = c(short, long)) %>%
    mutate(bf_ttest = map(data, ~ttest_wrapper(.x, rscale, cond_order[1], cond_order[2])),
           effSize = map(data, ~effSize_wrapper(.x, cond_order[1], cond_order[2]))) 
  
  if (one_tail) {
    tmp <- tmp %>% 
      mutate(
        bf = round(exp(unlist(map(bf_ttest, ~ .x@bayesFactor$bf[2]))), 3),
        d = unlist(map(effSize, "Cohens_d"))
      )
  } else {
    tmp <- tmp %>% 
      mutate(
      bf = round(exp(unlist(map(bf_ttest, ~ .x@bayesFactor$bf))), 3),
      d = unlist(map(effSize, "Cohens_d"))
    ) 
  }
  
  tmp %>%
    mutate(bf = shorter_bf_value(bf)) %>% 
    mutate(
      bf_lab = paste0('bold(BF[10]) == "', bf, '"'),
      d_lab = paste0('italic(d) == ', round(d,2)),
      label = paste0('atop(', bf_lab, ', ', 
                     d_lab,')')
    )
  
}

get_lowest_conf <- function(data) {
  data %>% 
    group_by(confidence_type, participant, bias_source, bias_direction) %>% 
    filter(value == min(value)) %>% 
    group_by(confidence_type, participant, bias_source, bias_direction, value) %>% 
    summarise(target_length = mean(target_length))
  
}

get_ref_rep <- function(curve_data) {
  curve_data %>% 
    group_by(confidence_type, participant, bias_source, bias_direction) %>% 
    nest() %>% 
    mutate(target_length = unlist(map(data, ~approx(x = .x$value, y = .x$target_length, xout = 400)$y)))
}

basic_point_plot <- function(data, bf_d_y_position, one_tail=TRUE) {
  
  # Create labels with sample size
  label_sample_size <- 
    data %>% 
    select(participant, bias_source, bias_direction) %>% 
    distinct() %>% 
    group_by(bias_source, bias_direction) %>% 
    summarise(count = n()) %>% 
    full_join(enframe(label_bias_source), by = c('bias_source' = 'name')) %>% 
    mutate(label = paste0(value, ' (N=', count, ')')) %>% 
    select(bias_source, label) %>% 
    distinct() %>% 
    deframe()
  
  # Calculate BF and effect sizes
  bf_d <-
    data_to_plot %>%
    select(-c(condition)) %>% 
    rename(target_length = value) %>% 
    get_bf_d_sdt(cond_order = c('long', 'short'), one_tail)

  # Make plot
  p <- data_to_plot %>% 
    ggplot(., aes(x=bias_direction, y=value)) +
    facet_wrap(. ~ bias_source, labeller = labeller(bias_source = label_sample_size)) + 
    geom_line(aes(group=participant, color=bias_source), show.legend = FALSE) +
    geom_point(aes(color=condition), show.legend = FALSE) +
    scale_color_manual(values = get_condition_colors()) +
    scale_x_discrete(labels = label_bias_direction) +
    stat_summary(fun=mean, color='black', geom='point', size=2) +
    stat_summary(fun=mean,
                 fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
                 fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)),
                 color='black', geom='errorbar', width=.2, size=.5)
  
  p <- p +
    geom_text(data=bf_d, 
              aes(x=1.5, y=bf_d_y_position, label=label), size=2.8, 
              parse=TRUE, inherit.aes = FALSE)
  
  
  p <- p +
    xlab('Bias direction')
  
  return(p)
  
}
