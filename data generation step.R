###############   ✅ step1: generating data === ##################

# Robust safe_extract: Always returns length 1 numeric
safe_extract <- function(x, i, name = NULL) {
  if (is.null(x) || length(x) == 0) return(as.numeric(NA))
  if (!is.null(name) && name %in% names(x)) {
    return(as.numeric(x[name]))  # Named, length 1
  } else if (length(x) >= i) {
    return(as.numeric(x[i]))  # Positional fallback
  }
  return(as.numeric(NA))
}
estimate_true_effects <- function(data_out) {
  n <- nrow(data_out)
  
  # Kaplan-Meier for potential outcomes
  km_fit_0 <- survfit(Surv(time = data_out$T0, event = rep(1, n)) ~ 1)
  km_fit_1 <- survfit(Surv(time = data_out$T1, event = rep(1, n)) ~ 1)
  
  time_0 <- km_fit_0$time
  surv_0 <- km_fit_0$surv
  time_1 <- km_fit_1$time
  surv_1 <- km_fit_1$surv
  
  quantile_times <- quantile(data_out$Y, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)
  surv_probs_0 <- summary(km_fit_0, times = quantile_times)$surv
  surv_probs_1 <- summary(km_fit_1, times = quantile_times)$surv
  
  for (i in seq_along(surv_probs_0)) {
    if (is.na(surv_probs_0[i])) surv_probs_0[i] <- tail(surv_0, 1)
    if (is.na(surv_probs_1[i])) surv_probs_1[i] <- tail(surv_1, 1)
  }
  
  ARR_true_ATE <- (1 - surv_probs_0) - (1 - surv_probs_1)
  if (length(ARR_true_ATE) < 5) ARR_true_ATE <- c(ARR_true_ATE, rep(NA, 5 - length(ARR_true_ATE)))
  names(ARR_true_ATE) <- paste0("ARR_true_ATE_", c(10, 25, 50, 75, 90))
  
  
  true_ATE_median <- get_median_surv(km_fit_1) - get_median_surv(km_fit_0)
  true_ATE_KM <- trapz(time_1, surv_1) - trapz(time_0, surv_0)
  
  # ATT
  treated_indices <- which(data_out$Treatment == 1)
  T1_treated <- data_out$T1[treated_indices]
  T0_treated <- data_out$T0[treated_indices]
  
  km_ATT_1 <- survfit(Surv(T1_treated, rep(1, length(T1_treated))) ~ 1)
  km_ATT_0 <- survfit(Surv(T0_treated, rep(1, length(T0_treated))) ~ 1)
  
  median_ATT_T1 <- get_median_surv(km_ATT_1)
  median_ATT_T0 <- get_median_surv(km_ATT_0)
  true_ATT_median <- median_ATT_T1 - median_ATT_T0
  
  event_probs_ATT_1 <- 1 - summary(km_ATT_1, times = quantile_times)$surv
  event_probs_ATT_0 <- 1 - summary(km_ATT_0, times = quantile_times)$surv
  
  for (i in seq_along(event_probs_ATT_1)) {
    if (is.na(event_probs_ATT_1[i])) event_probs_ATT_1[i] <- 1 - tail(km_ATT_1$surv, 1)
    if (is.na(event_probs_ATT_0[i])) event_probs_ATT_0[i] <- 1 - tail(km_ATT_0$surv, 1)
  }
  
  ARR_true_ATT <- event_probs_ATT_0 - event_probs_ATT_1
  
  if (length(ARR_true_ATT) < 5) ARR_true_ATT <- c(ARR_true_ATT, rep(NA, 5 - length(ARR_true_ATT)))
  
  names(ARR_true_ATT) <- paste0("ARR_true_ATT_", c(10, 25, 50, 75, 90))
  
  true_ATT_KM <- trapz(km_ATT_1$time, km_ATT_1$surv) - trapz(km_ATT_0$time, km_ATT_0$surv)
  
  list(
    true_ATE_KM = true_ATE_KM,
    true_ATE_median = true_ATE_median,
    true_ATT_KM = true_ATT_KM,
    true_ATT_median = true_ATT_median,
    ARR_true_ATE = ARR_true_ATE,
    ARR_true_ATT = ARR_true_ATT
  )
}



# Universal function to get median survival from a KM object

get_median_surv <- function(km_fit) {
  # True Kaplan-Meier median
  if (km_fit$surv[1] <= 0.5) return(km_fit$time[1])
  idx <- which(km_fit$surv <= 0.5)[1]
  if (is.na(idx)) return(NA)  # median not reached
  return(km_fit$time[idx])
}


get_median_time <- function(surv_probs, times) {
  if (all(is.na(surv_probs))) return(NA)       
  if (all(surv_probs > 0.5)) {               
    return(trapz(times, surv_probs))         
  }
  idx <- which(surv_probs <= 0.5)[1]
  return(times[idx])
}


run_simulation <- function(n = 10000, lambda = 0.00002, eta = 2, censoring_p = 0.6, HR = 1.10, target_treat_prop = 0.10) {
  tryCatch({
    
    X <- data.frame(
      X_1 = rnorm(n = n , mean = 0, sd = 1),
      X_2 = rnorm(n = n , mean = 0, sd = 1),
      X_3 = rnorm(n = n , mean = 0, sd = 1),
      X_4 = rnorm(n = n , mean = 0, sd = 1),
      X_5 = rnorm(n = n , mean = 0, sd = 1),
      X_6 = rnorm(n = n , mean = 0, sd = 1),
      X_7 = rnorm(n = n , mean = 0, sd = 1),
      X_8 = rnorm(n = n , mean = 0, sd = 1),
      X_9 = rnorm(n = n , mean = 0, sd = 1), 
      X_10 = rnorm(n = n , mean = 0, sd = 1)
    )
    
    # Coefficients for treatment assignment and outcome model
    alpha_0_treat <- 0.10
    alpha_W <- log(1.25)
    alpha_M <- log(1.5)
    alpha_S <- log(1.75)
    alpha_VS <- log(2)
    beta_treat <- log(HR)
    
    # Treatment assignment 
    log_odds_treat <- alpha_0_treat + alpha_W*X$X_1 + alpha_M*X$X_2 + alpha_S*X$X_3 + 
      alpha_W*X$X_4 + alpha_M*X$X_5 + alpha_S*X$X_6 + alpha_VS*X$X_7
    prob_T <- 1 / (1 + exp(-log_odds_treat))
    
    T_i <- rbinom(n, 1, prob_T)
    
    # outcome model
    log_odds_out <- beta_treat*T_i + alpha_W*X$X_4 + alpha_M*X$X_5 +
      alpha_S*X$X_6 + alpha_VS*X$X_7 + alpha_W*X$X_8 + alpha_M*X$X_9 + alpha_S*X$X_10
    p_out <- 1 / (1 + exp(-log_odds_out))
    
    Y <- rbinom(n, 1, p_out)
    
    # Generate survival times 
    U <- runif(n)
    denominator <- lambda * exp(log_odds_out) + 1e-10
    T_i_surv <- (-log(U) / denominator)^(1 / eta)
    
    # potential outcome
    U_pot <- runif(n)  
    log_odds_out_treated <- beta_treat*1 + alpha_W*X$X_4 + alpha_M*X$X_5 +
      alpha_S*X$X_6 + alpha_VS*X$X_7 + alpha_W*X$X_8 + alpha_M*X$X_9 + alpha_S*X$X_10
    
    log_odds_out_untreated <- beta_treat*0 + alpha_W*X$X_4 + alpha_M*X$X_5 +
      alpha_S*X$X_6 + alpha_VS*X$X_7 + alpha_W*X$X_8 + alpha_M*X$X_9 + alpha_S*X$X_10
    
    T0_potential <- (-log(U_pot) / (lambda * exp(log_odds_out_untreated)))^(1 / eta)
    T1_potential <- (-log(U_pot) / (lambda * exp(log_odds_out_treated)))^(1 / eta)
    
    true_ATE <- mean(T1_potential - T0_potential)
    
    # censoring time
    censor_time <- runif(n, 0, quantile(T_i_surv, censoring_p, na.rm = TRUE))
    T_i_censored <- pmin(T_i_surv, censor_time)
    event <- as.numeric(T_i_surv <= censor_time)
    
    # Data preparation
    data_out <- data.frame(
      Y = T_i_censored,
      event = event,
      Treatment = T_i,
      T0 = T0_potential, 
      T1 = T1_potential,
      X1 = X$X_1, X2 = X$X_2, X3 = X$X_3, X4 = X$X_4, X5 = X$X_5, X6 = X$X_6, 
      X7 = X$X_7, X8 = X$X_8, X9 = X$X_9, X10 = X$X_10
    )
    
    # Compute mean observed survival time (regardless of censoring)
    mean_surv_treated_observed <- mean(data_out$Y[data_out$Treatment == 1], na.rm = TRUE)
    mean_surv_control_observed <- mean(data_out$Y[data_out$Treatment == 0], na.rm = TRUE)
    
    # ATE ATT from mean observed survival time
    ATE_mean_observed <- mean_surv_treated_observed - mean_surv_control_observed
    
    ATT_mean_observed <- mean_surv_treated_observed
    
    
    # Get true effects
    true_effects <- estimate_true_effects(data_out)
    
    # Fit the Propensity Score Model
    ps_model <- glm(Treatment ~ X4 + X5 + X6 + X7 + X8 + X9 + X10,
                    data = data_out, family = binomial)
    
    data_out$ps <- predict(ps_model, type = "response")
    data_out$logit_ps <- predict(ps_model, type = "link")
    