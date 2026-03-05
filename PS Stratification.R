
####################################################### stratification 

# Create 5 strata based on quintiles of the propensity score
data_out$stratum <- cut(data_out$ps,
                        breaks = quantile(data_out$ps, probs = seq(0, 1, length.out = 6)),
                        include.lowest = TRUE,
                        labels = FALSE)

# Step 3: Compute stratum weights
K <- 5
p_j_ATE <- rep(1 / K, K)

treated <- data_out %>% filter(Treatment == 1)
n_treated <- nrow(treated)

library(tidyr)

p_j_ATT <- treated %>%
  group_by(stratum) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(weight = n / n_treated) %>%
  right_join(tibble(stratum = 1:K), by = "stratum") %>%
  mutate(weight = replace_na(weight, 0)) %>%
  arrange(stratum) %>%
  pull(weight)

# Step 4: Estimate stratum-specific Kaplan-Meier survival curves
# CHANGE: Added computation of KM curves before interpolation

km_untreated_list <- list()
km_treated_list <- list()

for (j in 1:K) {
  data_stratum <- data_out[data_out$stratum == j, ]
  
  # Untreated group in stratum j
  untreated <- data_stratum[data_stratum$Treatment == 0, ]
  if (nrow(untreated) > 0) {
    km_untreated_list[[j]] <- survfit(Surv(Y, event) ~ 1, data = untreated)
  } else {
    dummy_survfit <- survfit(Surv(rep(1, 2), rep(0, 2)) ~ 1)
    km_untreated_list[[j]] <- dummy_survfit
  }
  
  # Treated group in stratum j
  treated <- data_stratum[data_stratum$Treatment == 1, ]
  if (nrow(treated) > 0) {
    km_treated_list[[j]] <- survfit(Surv(Y, event) ~ 1, data = treated)
  } else {
    dummy_survfit <- survfit(Surv(rep(1, 2), rep(0, 2)) ~ 1)
    km_treated_list[[j]] <- dummy_survfit
  }
}

# Step 5: Define time grid based on percentiles of observed survival times
# CHANGE: Custom time points at selected percentiles
all_times <- data_out$Y
percentile_points <- quantile(all_times, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
time_grid <- as.numeric(percentile_points)

# Step 6: Interpolation function
interp_km <- function(km_fit, time_grid) {
  if (!all(c("time", "surv") %in% names(km_fit))) {
    return(rep(NA, length(time_grid)))  # Return NA if input is not valid
  }
  approx(x = km_fit$time, y = km_fit$surv, xout = time_grid, method = "constant", rule = 2)$y
}

# Step 7: Compute weighted survival curves
S0_ATE <- S1_ATE <- S0_ATT <- S1_ATT <- rep(0, length(time_grid))

for (j in 1:K) {
  s0j <- interp_km(km_untreated_list[[j]], time_grid)
  s1j <- interp_km(km_treated_list[[j]], time_grid)
  
  S0_ATE <- S0_ATE + p_j_ATE[j] * s0j
  S1_ATE <- S1_ATE + p_j_ATE[j] * s1j
  
  S0_ATT <- S0_ATT + p_j_ATT[j] * s0j
  S1_ATT <- S1_ATT + p_j_ATT[j] * s1j
}

# Step 8: ATE metrics
mean_surv_S0_ATE <- trapz(time_grid, S0_ATE)
mean_surv_S1_ATE <- trapz(time_grid, S1_ATE)
ATE_mean_strat <- mean_surv_S1_ATE - mean_surv_S0_ATE

# CHANGE: ARR calculated at each percentile time point
ARR_strat_ATE <- S1_ATE - S0_ATE
names(ARR_strat_ATE) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))

# Function to get median survival time from a survival curve
median_S0_list <- sapply(km_untreated_list, get_median_surv)
median_S1_list <- sapply(km_treated_list, get_median_surv)
# Weighted medians for ATE and ATT
median_S0_ATE <- weighted.mean(median_S0_list, w = p_j_ATE, na.rm = TRUE)
median_S1_ATE <- weighted.mean(median_S1_list, w = p_j_ATE, na.rm = TRUE)

ATE_median_strat <- median_S1_ATE - median_S0_ATE



# Step 9: ATT metrics
mean_surv_S0_ATT <- trapz(time_grid, S0_ATT)
mean_surv_S1_ATT <- trapz(time_grid, S1_ATT)
ATT_mean_strat <- mean_surv_S1_ATT - mean_surv_S0_ATT

# CHANGE: ARR calculated at each percentile time point
ARR_strat_ATT <- S1_ATT - S0_ATT
names(ARR_strat_ATT) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))

median_S0_ATT <- weighted.mean(median_S0_list, w = p_j_ATT, na.rm = TRUE)
median_S1_ATT <- weighted.mean(median_S1_list, w = p_j_ATT, na.rm = TRUE)


ATT_median_strat <- median_S1_ATT - median_S0_ATT


# Final output
results_strat <- list(
  time_points = time_grid,
  ATE = list(
    ATE_mean_strat = ATE_mean_strat,
    ATE_median_strat =  ATE_median_strat,
    ARR_strat_ATE = ARR_strat_ATE
  ),
  ATT = list(
    ATT_mean_strat = ATT_mean_strat,
    ATT_median_strat = ATT_median_strat,
    ARR_strat_ATT = ARR_strat_ATT
  )
)

print(results_strat)

