######################################################## IPTW metods

# Unstabilized IPTW weights
data_out$w_iptw <- with(data_out, Treatment / ps + (1 - Treatment) / (1 - ps))

# Estimate marginal treatment probability
p_treated <- mean(data_out$Treatment)

# Stabilized IPTW weights
data_out$w_stab <- with(data_out, 
                        Treatment * p_treated / ps + 
                          (1 - Treatment) * (1 - p_treated) / (1 - ps))


# ATE from stabilized IPTW 

# Separate KM for each group
km_treated_stab <- survfit(Surv(Y, event) ~ 1, 
                           data = subset(data_out, Treatment == 1),
                           weights = subset(data_out, Treatment == 1)$w_stab)

km_control_stab <- survfit(Surv(Y, event) ~ 1, 
                           data = subset(data_out, Treatment == 0),
                           weights =subset(data_out, Treatment == 0)$w_stab)

# Mean survival time
auc_treated_stab <- trapz(km_treated_stab$time, km_treated_stab$surv)
auc_control_stab <- trapz(km_control_stab$time, km_control_stab$surv)
ATE_mean_stab <- auc_treated_stab - auc_control_stab

# Median Survival time

ATE_median_stab <- get_median_surv(km_treated_stab) - get_median_surv(km_control_stab)


# ARR at specific time points
percentile_points <- quantile(data_out$Y, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
time_grid <- as.numeric(percentile_points)

# Helper function to get survival probabilities at specific time points from a survfit object
get_surv_probs_at_times <- function(km_fit, times) {
  # If it's a stratified KM, use surv and time vectors directly
  if (!is.null(km_fit$time) & !is.null(km_fit$surv)) {
    return(approx(km_fit$time, km_fit$surv, xout = times, method = "constant", rule = 2)$y)
  }
  
  # Otherwise, fallback to summary for stratified KM
  km_summary <- summary(km_fit, times = times)
  return(km_summary$surv)
}


s1_vec_stab <- get_surv_probs_at_times(km_treated_stab, time_grid)
s0_vec_stab <- get_surv_probs_at_times(km_control_stab, time_grid)
ARR_stab <- s1_vec_stab - s0_vec_stab
names(ARR_stab) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))



# ATE from Unstabilized IPTW 

km_treated_unstab <- survfit(Surv(Y, event) ~ 1,
                             data = subset(data_out, Treatment == 1),
                             weights = subset(data_out, Treatment == 1)$w_iptw) 
km_control_unstab <- survfit(Surv(Y, event) ~ 1,
                             data = subset(data_out, Treatment == 0),
                             weights = subset(data_out, Treatment == 0)$w_iptw) 


# mean survival time
ATE_mean_unstab <- trapz(km_treated_unstab$time, km_treated_unstab$surv) - 
  trapz(km_control_unstab$time, km_control_unstab$surv) 

# Median survival time
ATE_median_unstab <- get_median_surv(km_treated_unstab) - get_median_surv(km_control_unstab)

# ARR

s1_vec_unstab <- get_surv_probs_at_times(km_treated_unstab, time_grid) 
s0_vec_unstab <- get_surv_probs_at_times(km_control_unstab, time_grid) 
ARR_unstab <- s1_vec_unstab - s0_vec_unstab
names(ARR_unstab) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))



# ATT Estimation with Unstabilized ATT Weights 
treated_data <- subset(data_out, Treatment == 1)
untreated_data <- subset(data_out, Treatment == 0)

weights_att_unstab <- untreated_data$ps / (1 - untreated_data$ps)

km_treated_att_unstab <- survfit(Surv(Y, event) ~ 1, data = treated_data, weights = rep(1, nrow(treated_data)))
km_control_att_unstab <- survfit(Surv(Y, event) ~ 1, data = untreated_data, weights = weights_att_unstab)

ATT_mean_unstab <- trapz(km_treated_att_unstab$time, km_treated_att_unstab$surv) - 
  trapz(km_control_att_unstab$time, km_control_att_unstab$surv)
ATT_median_unstab <- get_median_surv(km_treated_att_unstab) - get_median_surv(km_control_att_unstab)


s1_vec_att_unstab <- get_surv_probs_at_times(km_treated_att_unstab, time_grid) 
s0_vec_att_unstab <- get_surv_probs_at_times(km_control_att_unstab, time_grid) 
ARR_att_unstab <- s1_vec_att_unstab - s0_vec_att_unstab
names(ARR_att_unstab) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))


# ATT with Stabilized Weights 

data_out$w_att_stab <- ifelse(
  data_out$Treatment == 1,
  1,
  (p_treated / (1 - p_treated)) * (data_out$ps / (1 - data_out$ps))
)

treated_data <- subset(data_out, Treatment == 1)
untreated_data <- subset(data_out, Treatment == 0)

weights_att_stab <- subset(data_out, Treatment == 0)$w_att_stab

km_treated_att_stab <- survfit(Surv(Y, event) ~ 1, data = treated_data, weights = rep(1, nrow(treated_data)))
km_control_att_stab <- survfit(Surv(Y, event) ~ 1, data = untreated_data, weights = weights_att_stab)

ATT_mean_stab <- trapz(km_treated_att_stab$time, km_treated_att_stab$surv) - 
  trapz(km_control_att_stab$time, km_control_att_stab$surv)


ATT_median_stab <- get_median_surv(km_treated_att_stab) - get_median_surv(km_control_att_stab)


s1_vec_att_stab <- get_surv_probs_at_times(km_treated_att_stab, time_grid)
s0_vec_att_stab <- get_surv_probs_at_times(km_control_att_stab, time_grid)
ARR_att_stab <- s1_vec_att_stab - s0_vec_att_stab
names(ARR_att_stab) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))


## Final Results
ATE_iptw_results <- list(
  ATE_mean_stab = ATE_mean_stab,
  ATE_median_stab = ATE_median_stab,
  ATE_ARR_stab = ARR_stab,
  
  ATE_mean_unstab = ATE_mean_unstab,
  ATE_median_unstab = ATE_median_unstab,
  ATE_ARR_unstab = ARR_unstab 
)

ATT_iptw_results <- list(
  ATT_mean_stab = ATT_mean_stab,
  ATT_median_stab = ATT_median_stab,
  ATT_ARR_stab = ARR_att_stab,
  
  ATT_mean_unstab = ATT_mean_unstab,
  ATT_median_unstab = ATT_median_unstab,
  ATT_ARR_unstab = ARR_att_unstab
)





print(ATE_iptw_results)
print(ATT_iptw_results)

