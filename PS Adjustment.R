########################################################### ps adjustment


# Fit Cox Model Adjusted for Treatment and PS 
cox_ps_model <- coxph(Surv(time = Y, event = event) ~ Treatment + ps,
                      data = data_out)


#ATE Estimation
# Counterfactual under treatment
newdata_1 <- data_out
newdata_1$Treatment <- 1

# Counterfactual under control
newdata_0 <- data_out
newdata_0$Treatment <- 0

#Predict survival for every subject
sf1 <- survfit(cox_ps_model, newdata = newdata_1)
sf0 <- survfit(cox_ps_model, newdata = newdata_0)

time_grid <- sf1$time

#Marginal survival curves
S1_marginal <- colMeans(sf1$surv)
S0_marginal <- colMeans(sf0$surv)

#ATE: mean survival (RMST)
ATE_mean_adjust <- trapz(time_grid, S1_marginal) - trapz(time_grid, S0_marginal)

#ATE: median survival
median_S1 <- time_grid[which(S1_marginal <= 0.5)[1]]
median_S0 <- time_grid[which(S0_marginal <= 0.5)[1]]

ATE_median_adjust <- median_S1 - median_S0


# ATE ARR at survival time percentiles
get_surv_probs_at_times <- function(S, times, grid) {
  approx(x = grid, y = S, xout = times, rule = 2)$y
}

percentile_times <- quantile(data_out$Y, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)
S1_ate <- get_surv_probs_at_times(S1_marginal, percentile_times, time_grid) 
S0_ate <- get_surv_probs_at_times(S0_marginal, percentile_times, time_grid)
ARR_ATE_adjust <- S1_ate - S0_ate
names(ARR_ATE_adjust) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))


# Results: ATE
results_ATE_adjust <- list(
  ATE_mean_adjust = ATE_mean_adjust,
  ATE_median_adjust = ATE_median_adjust,
  ATE_ARR_adjust = ARR_ATE_adjust
  
)
print(results_ATE_adjust)

# ATT Estimation

treated_idx <- data_out$Treatment == 1

S1_ATT <- colMeans(sf1$surv[treated_idx, , drop = FALSE])
S0_ATT <- colMeans(sf0$surv[treated_idx, , drop = FALSE])

ATT_mean_adjust <- trapz(time_grid, S1_ATT) - trapz(time_grid, S0_ATT)

median_S1_ATT <- time_grid[which(S1_ATT <= 0.5)[1]]
median_S0_ATT <- time_grid[which(S0_ATT <= 0.5)[1]]

ATT_median_adjust <- median_S1_ATT - median_S0_ATT

# ATT ARR at survival time percentiles

S1_att <- get_surv_probs_at_times(S1_ATT, percentile_times, time_grid) 
S0_att <- get_surv_probs_at_times(S0_ATT, percentile_times, time_grid) 
ARR_ATT_adjust <- S1_att - S0_att
names(ARR_ATT_adjust) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))



# Results: ATT
results_ATT_adjust <- list(
  ATT_mean_adjust = ATT_mean_adjust,
  ATT_median_adjust = ATT_median_adjust,
  ATT_ARR_adjust = ARR_ATT_adjust
)
print(results_ATT_adjust)

