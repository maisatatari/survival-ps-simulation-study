################################### step 3 loop ###############

safe_extract <- function(x, i) {
  if (is.null(x) || length(x) < i) return(as.numeric(NA))
  return(x[i])
}

# Simulation parameters
HR <- c(1, 1.10, 1.25, 1.5, 1.75, 2)
censoring_p <- seq(0.1, 0.9, by = 0.1)
target_treat_prop <- c(0.05, 0.10, 0.25)
M <- 1000

run_all_simulations <- function(hr, cens, tp, i) {
  # Run simulation - returns a list with data and true effects
  data_sim_obj <- tryCatch(
    run_simulation(HR = hr, censoring_p = cens, target_treat_prop = tp),
    error = function(e) {
      warning(paste("Data simulation failed for HR =", hr, ", censoring =", cens, ", tp =", tp, ", i =", i, "\n", e$message))
      return(NULL)
    }
  )
  if (is.null(data_sim_obj)) {
    return(NULL)
  }
  
  print(data_sim_obj$True)
  # Use the true values from the simulation object directly
  true_vals <- data_sim_obj$True
  # Debug print to check they are not NULL
  cat("DEBUG: True values for HR =", hr, ", censoring =", cens, ", tp =", tp, ", sim =", i, "\n")
  print(true_vals)
  
  true_ATE_KM     <- true_vals$ATE_mean
  true_ATE_median <- true_vals$ATE_median
  true_ATT_KM     <- true_vals$ATT_mean
  true_ATT_median <- true_vals$ATT_median
  
  ARR_true_ATE <- true_vals$ARR_ATE
  ARR_true_ATT <- true_vals$ARR_ATT
  
  
  # Now estimate effects on the extracted data frame
  est_match  <- data_sim_obj$final_results$Matching
  est_strat  <- data_sim_obj$final_results$Stratification
  est_iptw   <- data_sim_obj$final_results$IPTW
  est_ps_adj <- data_sim_obj$final_results$PS_adjust
  
  est_ps_adj <- data_sim_obj$final_results$PS_adjust
  
  
  result <- tryCatch({
    data.frame(
      sim = i,
      hr = hr,
      cens = cens,
      tp = tp,
      
      # Matching
      ATT_mean_match = if (!is.null(est_match)) est_match$ATT_mean_match  else as.numeric(NA),
      ATT_median_match = if (!is.null(est_match)) est_match$ATT_median_match else as.numeric(NA),
      ARR_10_match = if (!is.null(est_match)) safe_extract(est_match$ATT_ARR_match, 1) else as.numeric(NA),
      ARR_25_match = if (!is.null(est_match)) safe_extract(est_match$ATT_ARR_match, 2) else as.numeric(NA),
      ARR_50_match = if (!is.null(est_match)) safe_extract(est_match$ATT_ARR_match, 3) else as.numeric(NA),
      ARR_75_match = if (!is.null(est_match)) safe_extract(est_match$ATT_ARR_match, 4) else as.numeric(NA),
      ARR_90_match = if (!is.null(est_match)) safe_extract(est_match$ATT_ARR_match, 5) else as.numeric(NA),
      
      # Stratification ATE
      ATE_mean_strat = if (!is.null(est_strat)) est_strat$ATE_mean_strat else as.numeric(NA),
      ATE_median_strat = if (!is.null(est_strat)) est_strat$ATE_median_strat else as.numeric(NA),
      ARR_10_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATE_strat, 1) else as.numeric(NA),
      ARR_25_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATE_strat, 2) else as.numeric(NA),
      ARR_50_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATE_strat, 3) else as.numeric(NA),
      ARR_75_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATE_strat, 4) else as.numeric(NA),
      ARR_90_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATE_strat, 5) else as.numeric(NA),
      
      # Stratification ATT
      ATT_mean_strat = if (!is.null(est_strat)) est_strat$ATT_mean_strat else as.numeric(NA),
      ATT_median_strat = if (!is.null(est_strat)) est_strat$ATT_median_strat else as.numeric(NA),
      ATT_10_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATT_strat, 1) else as.numeric(NA),
      ATT_25_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATT_strat, 2) else as.numeric(NA),
      ATT_50_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATT_strat, 3) else as.numeric(NA),
      ATT_75_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATT_strat, 4) else as.numeric(NA),
      ATT_90_strat = if (!is.null(est_strat)) safe_extract(est_strat$ARR_ATT_strat, 5) else as.numeric(NA),
      
      # IPTW unstabilized ATE/ATT
      ATE_mean_iptw_unstab = if (!is.null(est_iptw)) est_iptw$ATE_mean_unstab else as.numeric(NA),
      ATE_median_iptw_unstab = if (!is.null(est_iptw)) est_iptw$ATE_median_unstab else as.numeric(NA),
      ARR_10_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_unstab, 1) else as.numeric(NA),
      ARR_25_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_unstab, 2) else as.numeric(NA),
      ARR_50_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_unstab, 3) else as.numeric(NA),
      ARR_75_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_unstab, 4) else as.numeric(NA),
      ARR_90_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_unstab, 5) else as.numeric(NA),
      ATT_mean_iptw_unstab = if (!is.null(est_iptw)) est_iptw$ATT_mean_unstab else as.numeric(NA),
      ATT_median_iptw_unstab = if (!is.null(est_iptw)) est_iptw$ATT_median_unstab else as.numeric(NA),
      ATT_10_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_unstab, 1) else as.numeric(NA),
      ATT_25_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_unstab, 2) else as.numeric(NA),
      ATT_50_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_unstab, 3) else as.numeric(NA),
      ATT_75_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_unstab, 4) else as.numeric(NA),
      ATT_90_iptw_unstab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_unstab, 5) else as.numeric(NA),
      
      # IPTW stabilized ATE/ATT
      ATE_mean_iptw_stab = if (!is.null(est_iptw)) est_iptw$ATE_mean_stab else as.numeric(NA),
      ATE_median_iptw_stab = if (!is.null(est_iptw)) est_iptw$ATE_median_stab else as.numeric(NA),
      ARR_10_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_stab, 1) else as.numeric(NA),
      ARR_25_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_stab, 2) else as.numeric(NA),
      ARR_50_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_stab, 3) else as.numeric(NA),
      ARR_75_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_stab, 4) else as.numeric(NA),
      ARR_90_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATE_ARR_stab, 5) else as.numeric(NA),
      ATT_mean_iptw_stab = if (!is.null(est_iptw)) est_iptw$ATT_mean_stab else as.numeric(NA),
      ATT_median_iptw_stab = if (!is.null(est_iptw)) est_iptw$ATT_median_stab else as.numeric(NA),
      ATT_10_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_stab, 1) else as.numeric(NA),
      ATT_25_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_stab, 2) else as.numeric(NA),
      ATT_50_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_stab, 3) else as.numeric(NA),
      ATT_75_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_stab, 4) else as.numeric(NA),
      ATT_90_iptw_stab = if (!is.null(est_iptw)) safe_extract(est_iptw$ATT_ARR_stab, 5) else as.numeric(NA),
      
      # PS Adjustment
      ATE_mean_ps_adj = if (!is.null(est_ps_adj)) est_ps_adj$ATE_mean_adjust else as.numeric(NA),
      ATE_median_ps_adj = if (!is.null(est_ps_adj)) est_ps_adj$ATE_median_adjust else as.numeric(NA),
      ARR_10_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATE_ARR_adjust, 1) else as.numeric(NA),
      ARR_25_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATE_ARR_adjust, 2) else as.numeric(NA),
      ARR_50_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATE_ARR_adjust, 3) else as.numeric(NA),
      ARR_75_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATE_ARR_adjust, 4) else as.numeric(NA),
      ARR_90_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATE_ARR_adjust, 5) else as.numeric(NA),
      ATT_mean_ps_adj = if (!is.null(est_ps_adj)) est_ps_adj$ATT_mean_adjust else as.numeric(NA),
      ATT_median_ps_adj = if (!is.null(est_ps_adj)) est_ps_adj$ATT_median_adjust else as.numeric(NA),
      ATT_10_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATT_ARR_adjust, 1) else as.numeric(NA),
      ATT_25_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATT_ARR_adjust, 2) else as.numeric(NA),
      ATT_50_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATT_ARR_adjust, 3) else as.numeric(NA),
      ATT_75_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATT_ARR_adjust, 4) else as.numeric(NA),
      ATT_90_ps_adj = if (!is.null(est_ps_adj)) safe_extract(est_ps_adj$ATT_ARR_adjust, 5) else as.numeric(NA),
      
      # True effect values from simulation
      true_ATE_KM = true_ATE_KM,
      true_ATE_median = true_ATE_median,
      true_ATT_KM = true_ATT_KM,
      true_ATT_median = true_ATT_median,
      ARR_true_ATE_10 = ARR_true_ATE["ARR_true_ATE_10"],
      ARR_true_ATE_25 = ARR_true_ATE["ARR_true_ATE_25"],
      ARR_true_ATE_50 = ARR_true_ATE["ARR_true_ATE_50"],
      ARR_true_ATE_75 = ARR_true_ATE["ARR_true_ATE_75"],
      ARR_true_ATE_90 = ARR_true_ATE["ARR_true_ATE_90"],
      
      ARR_true_ATT_10 = ARR_true_ATT["ARR_true_ATT_10"],
      ARR_true_ATT_25 = ARR_true_ATT["ARR_true_ATT_25"],
      ARR_true_ATT_50 = ARR_true_ATT["ARR_true_ATT_50"],
      ARR_true_ATT_75 = ARR_true_ATT["ARR_true_ATT_75"],
      ARR_true_ATT_90 = ARR_true_ATT["ARR_true_ATT_90"],
      
      
      # Ground truth log HR
      logHR = log(hr)
    )
  }, error = function(e) {
    warning(paste("Failed to create result df for HR =", hr, ", censoring =", cens, ", tp =", tp, ", i =", i, "\n", e$message))
    return(NULL)
  })
  
  return(result)
}


# Run simulations and collect results
results_list <- list()
counter <- 1

for (hr in HR) {
  for (cens in censoring_p) {
    for (tp in target_treat_prop) {
      for (i in 1:M) {
        sim_result <- run_all_simulations(hr = hr, cens = cens, tp = tp, i = i)
        
        cat("DEBUG: sim_result is of type", class(sim_result), "for sim", i, "\n")
        
        if (!is.null(sim_result) && is.data.frame(sim_result)) {
          
          
          results_list[[counter]] <- sim_result
          counter <- counter + 1
        } else {
          message(sprintf("❌ Simulation skipped for HR=%.2f, censor=%.2f, tp=%.2f, iteration=%d", hr, cens, tp, i))
        }
        
      }
    }
  }
}



all_results_df <- dplyr::bind_rows(results_list)

