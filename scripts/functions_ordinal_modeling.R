# Gets unconstrained comparison BF and posterior samples
get_bf_samp <- function(y, x, gMap, rscale, no_iter, filter_iter) {
  
  # Compute BF
  out_bf <- nWayAOV(y, x, gMap, rscale = rep(rscale, length(unique(gMap))))
  # Sample posterior
  out_samp <- nWayAOV(y, x, gMap, rscale = rep(rscale, length(unique(gMap)))
                      , posterior = TRUE, iterations = no_iter)
  # Filter first n% of the iterations
  out_samp <- out_samp[(nrow(out_samp) * filter_iter) : nrow(out_samp),]
  # return bf and sample
  return(list(out_bf, out_samp))
}

# Make contrast image to check that coding looks fine
make_condition_contrast_image <- function(contrast) {
  
  contrast %>% 
    as_tibble() %>% 
    select(matches('x[0-9]{1}$', perl = TRUE)) %>% 
    as.matrix() %>% 
    t() %>% 
    image()
  
}

# Get gMap based on contrast columns
get_gMap <- function(contrast) {
  # This gMap is used if no sub-effect
  gMap <- 1:(length(grep('x[0-9]{1}$', colnames(contrast), value = TRUE)))
  # Include participant contrast to gMap
  gMap <- c(rep(0, length(grep('x[0-9]{2,3}$', colnames(contrast), value = TRUE))), gMap)
  return(gMap)
}



# Contrast functions ------------------------------------------------------

get_id_contrast <- function(id_col) {
  
  # get unique ids (there are two per participant)
  unique_ids <- id_col %>% pull(id) %>% unique()
  # make id contrast matrix
  id_contrast <- 
    
    map2(unique_ids, seq(length(unique_ids)),
         ~ id_col %>% 
           transmute(as.integer(id == .x)) %>% 
           # The column names start from 10 because the first 10 
           # names are reserved for the contrast indicating the 
           # bias condition.
           set_names(paste0('x', .y + 10))
    ) %>% 
    bind_cols()
  
  # Convert to matrix to combine with other
  # contrast matrices
  id_contrast <- 
    as.matrix(id_contrast)
  
  return(id_contrast)
  
}


get_contrast_A <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  x2 <- ifelse(dat$cond_code == "mulo", val_long, 
               ifelse(dat$cond_code == "mush", val_short, 0))    # ML bias direction effect
  # Base rate
  x3 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  x4 <- ifelse(dat$cond_code == "balo", val_long, 
               ifelse(dat$cond_code == "bash", val_short, 0))    # BR bias direction effect
  # Payoff
  # The payoff condition is used as the intercept so it's not indicated in
  # contrast matrix and instead is always the 'mu' column in the sample data.
  x5 <- ifelse(dat$cond_code == "palo", val_long, 
               ifelse(dat$cond_code == "pash", val_short, 0))    # PO bias direction effect
  
  # Combine all contrast columns into one matrix
  X_a <- cbind(x1, x2, x3, x4, x5)
  
  # Add id contrast
  X_a <- 
    cbind(
      id_contrast,
      X_a)
  
  return(X_a)
}

get_contrast_B <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  # Base rate
  x2 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  # Bias direction irrespective of bias source
  x3 <- ifelse(dat$bias_direction == 'long', val_long,       # bias direction effect
               ifelse(dat$bias_direction == 'short', val_short, 0))
  
  # Combine all contrast columns into one matrix
  X_b <- cbind(x1, x2, x3)
  
  # Add id contrast
  X_b <- 
    cbind(
      id_contrast,
      X_b)
  
  return(X_b)
}

get_contrast_C <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  # Base rate
  x2 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  # Muller-Lyer & Base rate bias direction
  x3 <- ifelse(dat$cond_code %in% c("mulo", "balo"), val_long,  
               ifelse(dat$cond_code %in% c("mush", "bash"), val_short, 0)) # bias direction effect
  # Payoff
  # The payoff condition is used as the intercept so it's not indicated in
  # contrast matrix and instead is always the 'mu' column in the sample data.
  x4 <- ifelse(dat$cond_code == "palo", val_long, 
               ifelse(dat$cond_code == "pash", val_short, 0))    # PO bias direction effect
  
  # Combine all contrast columns into one matrix
  X_c <- cbind(x1, x2, x3, x4)
  
  # Add id contrast
  X_c <- 
    cbind(
      id_contrast,
      X_c)
  
  return(X_c)
}

get_contrast_D <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  # Muller-Lyer & Payoff bias direction
  x2 <- ifelse(dat$cond_code %in% c("mulo", "palo"), val_long,  
               ifelse(dat$cond_code %in% c("mush", "pash"), val_short, 0)) # bias direction effect
  
  # Base rate
  x3 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  x4 <- ifelse(dat$cond_code == "balo", val_long, 
               ifelse(dat$cond_code == "bash", val_short, 0))    # BR bias direction effect
  
  # Combine all contrast columns into one matrix
  X_d <- cbind(x1, x2, x3, x4)
  
  # Add id contrast
  X_d <- 
    cbind(
      id_contrast,
      X_d)
  
  return(X_d)
}

get_contrast_E <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  x2 <- ifelse(dat$cond_code == "mulo", val_long, 
               ifelse(dat$cond_code == "mush", val_short, 0))    # ML bias direction effect
  # Base rate
  x3 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  
  # Combine all contrast columns into one matrix
  X_e <- cbind(x1, x2, x3)
  
  # Add id contrast
  X_e <- 
    cbind(
      id_contrast,
      X_e)
  
  return(X_e)
}

get_contrast_F <- function(dat) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  # Base rate
  x2 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  
  # Combine all contrast columns into one matrix
  X_f <- cbind(x1, x2)
  
  # Add id contrast
  X_f <- 
    cbind(
      id_contrast,
      X_f)
  
  return(X_f)
}

get_contrast_G <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  x2 <- ifelse(dat$cond_code == "mulo", val_long, 
               ifelse(dat$cond_code == "mush", val_short, 0))    # ML bias direction effect
  # Base rate
  x3 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  x4 <- ifelse(dat$cond_code == "balo", val_long, 
               ifelse(dat$cond_code == "bash", val_short, 0))    # BR bias direction effect
  
  # Combine all contrast columns into one matrix
  X_g <- cbind(x1, x2, x3, x4)
  
  # Add id contrast
  X_g <- 
    cbind(
      id_contrast,
      X_g)
  
  return(X_g)
}

get_contrast_H <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  x2 <- ifelse(dat$cond_code == "mulo", val_long, 
               ifelse(dat$cond_code == "mush", val_short, 0))    # ML bias direction effect
  # Base rate
  x3 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  # Payoff
  # The payoff condition is used as the intercept so it's not indicated in
  # contrast matrix and instead is always the 'mu' column in the sample data.
  x4 <- ifelse(dat$cond_code == "palo", val_long, 
               ifelse(dat$cond_code == "pash", val_short, 0))    # PO bias direction effect
  
  
  # Combine all contrast columns into one matrix
  X_h <- cbind(x1, x2, x3, x4)
  
  # Add id contrast
  X_h <- 
    cbind(
      id_contrast,
      X_h)
  
  return(X_h)
}


get_contrast_I <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  
  x2 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  x3 <- ifelse(dat$cond_code == "balo", val_long, 
               ifelse(dat$cond_code == "bash", val_short, 0))    # BR bias direction effect
  # Payoff
  # The payoff condition is used as the intercept so it's not indicated in
  # contrast matrix and instead is always the 'mu' column in the sample data.
  x4 <- ifelse(dat$cond_code == "palo", val_long, 
               ifelse(dat$cond_code == "pash", val_short, 0))    # PO bias direction effect
  
  
  # Combine all contrast columns into one matrix
  X_i <- cbind(x1, x2, x3, x4)
  
  # Add id contrast
  X_i <- 
    cbind(
      id_contrast,
      X_i)
  
  return(X_i)
}

get_contrast_J <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  # Base rate
  x2 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  # Payoff
  x3 <- ifelse(dat$cond_code == "palo", val_long, 
               ifelse(dat$cond_code == "pash", val_short, 0))    # ML bias direction effect
  
  # Combine all contrast columns into one matrix
  X_j <- cbind(x1, x2, x3)
  
  # Add id contrast
  X_j <- 
    cbind(
      id_contrast,
      X_j)
  
  return(X_j)
}

get_contrast_K <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  # Base rate
  x2 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  x3 <- ifelse(dat$cond_code == "balo", val_long, 
               ifelse(dat$cond_code == "bash", val_short, 0))    # ML bias direction effect
  
  # Combine all contrast columns into one matrix
  X_k <- cbind(x1, x2, x3)
  
  # Add id contrast
  X_k <- 
    cbind(
      id_contrast,
      X_k)
  
  return(X_k)
}

get_contrast_L <- function(dat, val_long, val_short) {
  
  # Muller-Lyer
  x1 <- ifelse(dat$cond_code %in% c("mulo", "mush"), 1, 0) # ML intercept
  # Muller-Lyer bias direction
  x2 <- ifelse(dat$cond_code == "mulo", val_long, 
               ifelse(dat$cond_code == "mush", val_short, 0))    # BR bias direction effect
  # Base rate
  x3 <- ifelse(dat$cond_code %in% c("balo", "bash"), 1, 0) # BR intercept
  # Base rate & Payoff bias direction
  x4 <- ifelse(dat$cond_code %in% c("balo", "palo"), val_long,  
               ifelse(dat$cond_code %in% c("bash", "pash"), val_short, 0)) # bias direction effect
  
  
  
  # Combine all contrast columns into one matrix
  X_l <- cbind(x1, x2, x3, x4)
  
  # Add id contrast
  X_l <- 
    cbind(
      id_contrast,
      X_l)
  
  return(X_l)
}


