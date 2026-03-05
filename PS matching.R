sd_logit_ps <- sd(data_out$logit_ps)
match_obj <- matchit(Treatment ~ 1,
                     data = data_out,
                     method = "nearest",
                     distance = data_out$logit_ps,
                     caliper = 0.2 * sd_logit_ps,
                     ratio = 1,
                     replace = FALSE)

matched_data <- match.data(match_obj)

# KM Curves for Treated and Control in Matched Data
km_treated_match <- survfit(Surv(Y, event) ~ 1, data = matched_data[matched_data$Treatment == 1, ])
km_control_match <- survfit(Surv(Y, event) ~ 1, data = matched_data[matched_data$Treatment == 0, ])

# Mean Survival Time (AUC using Trapezoidal Rule)
mean_surv_treated_match <- trapz(km_treated_match$time, km_treated_match$surv)
mean_surv_control_match <- trapz(km_control_match$time, km_control_match$surv)

ATE_mean_matched <- mean_surv_treated_match - mean_surv_control_match
ATT_mean_matched <- ATE_mean_matched  # Same, since matching estimates ATT


# Median Survival Time (using KM curve interpolation)
median_treated_match <- get_median_surv(km_treated_match)
median_control_match <- get_median_surv(km_control_match)

ATT_median_matched <- median_treated_match - median_control_match




# Define quantile-based time points
quantile_times <- quantile(data_out$Y, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)

# Function to interpolate KM survival at a given time point
get_surv_at_t0 <- function(survfit_obj, t0) {
  times <- survfit_obj$time
  survs <- survfit_obj$surv
  
  if (t0 %in% times) return(survs[which(times == t0)[1]])
  if (t0 > max(times)) return(tail(survs, 1))
  
  return(approx(times, survs, xout = t0, method = "linear", rule = 2)$y)
}

# Get survival at each quantile time point
s1_quantiles_match <- sapply(quantile_times, function(t) get_surv_at_t0(km_treated_match, t))
s0_quantiles_match <- sapply(quantile_times, function(t) get_surv_at_t0(km_control_match, t))

# Define ARR = Survival(Treated) - Survival(Control) at each time point
ATT_ARR_matched <- s1_quantiles_match - s0_quantiles_match
names(ATT_ARR_matched) <- paste0("ARR_at_p", c(10, 25, 50, 75, 90))




# Output Lists
ATT_results_match <- list(
  ATT_mean_match = ATT_mean_matched,
  ATT_median_match =  ATT_median_matched,
  ATT_ARR_match = ATT_ARR_matched
)


# Print output
print(ATT_results_match)

