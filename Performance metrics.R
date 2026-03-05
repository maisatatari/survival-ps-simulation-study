
####################### step 4 performance metrics ########


performance_metrics <- all_results_df %>%
  
  group_by(hr, cens, tp) %>%  # Group by simulation parameters
  summarize(
    
    # --- Estimated treatment effect sizes (mean across simulations) ---
    
    est_ATT_mean_match   = mean(ATT_mean_match, na.rm = TRUE),
    est_ATT_median_match = mean(ATT_median_match, na.rm = TRUE),
    
    est_ATE_mean_strat   = mean(ATE_mean_strat, na.rm = TRUE),
    est_ATE_median_strat = mean(ATE_median_strat, na.rm = TRUE),
    est_ATT_mean_strat   = mean(ATT_mean_strat, na.rm = TRUE),
    est_ATT_median_strat = mean(ATT_median_strat, na.rm = TRUE),
    
    est_ATE_mean_unstab   = mean(ATE_mean_iptw_unstab, na.rm = TRUE),
    est_ATE_median_unstab = mean(ATE_median_iptw_unstab, na.rm = TRUE),
    est_ATT_mean_iptw_unstab   = mean(ATT_mean_iptw_unstab, na.rm = TRUE),
    est_ATT_median_iptw_unstab = mean(ATT_median_iptw_unstab, na.rm = TRUE),   
    est_ATE_mean_iptw_stab   = mean(ATE_mean_iptw_stab, na.rm = TRUE),
    est_ATE_median_iptw_stab = mean(ATE_median_iptw_stab, na.rm = TRUE),
    est_ATT_mean_iptw_stab = mean(ATT_mean_iptw_stab, na.rm = TRUE),
    est_ATT_median_iptw_stab = mean(ATT_median_iptw_stab, na.rm = TRUE),  
    
    est_ATE_mean_ps_adj = mean(ATE_mean_ps_adj, na.rm = TRUE),
    est_ATE_median_ps_adj = mean(ATE_median_ps_adj, na.rm = TRUE),
    est_ATT_mean_ps_adj = mean(ATT_mean_ps_adj, na.rm = TRUE),
    est_ATT_median_ps_adj = mean(ATT_median_ps_adj, na.rm = TRUE),
    
    # True effect values from simulation
    true_ATE_KM = mean(true_ATE_KM,na.rm = TRUE),
    true_ATE_median = mean(true_ATE_median,na.rm = TRUE),
    true_ATT_KM = mean(true_ATT_KM,na.rm = TRUE),
    true_ATT_median = mean(true_ATT_median,na.rm = TRUE),
    
    # Bias = mean(estimate - truth)
    bias_ATT_mean_match = mean(ATT_mean_match - true_ATT_KM, na.rm = TRUE),
    bias_ATT_median_match = mean(ATT_median_match - true_ATT_median, na.rm = TRUE),
    bias_ARR_10_match = mean(ARR_10_match - ARR_true_ATT_10, na.rm = TRUE),
    bias_ARR_25_match = mean(ARR_25_match - ARR_true_ATT_25, na.rm = TRUE),
    bias_ARR_50_match = mean(ARR_50_match - ARR_true_ATT_50, na.rm = TRUE),
    bias_ARR_75_match = mean(ARR_75_match - ARR_true_ATT_75, na.rm = TRUE),
    bias_ARR_90_match = mean(ARR_90_match - ARR_true_ATT_90, na.rm = TRUE),
    
    # ATE Stratification Bias
    bias_ATE_mean_strat    = mean(ATE_mean_strat - true_ATE_KM, na.rm = TRUE),
    bias_ATE_median_strat  = mean(ATE_median_strat - true_ATE_median, na.rm = TRUE),
    bias_ATE_ARR_10_strat  = mean(ARR_10_strat - ARR_true_ATE_10, na.rm = TRUE),
    bias_ATE_ARR_25_strat  = mean(ARR_25_strat - ARR_true_ATE_25, na.rm = TRUE),
    bias_ATE_ARR_50_strat  = mean(ARR_50_strat - ARR_true_ATE_50, na.rm = TRUE),
    bias_ATE_ARR_75_strat  = mean(ARR_75_strat - ARR_true_ATE_75, na.rm = TRUE),
    bias_ATE_ARR_90_strat  = mean(ARR_90_strat - ARR_true_ATE_90, na.rm = TRUE),
    
    # ATT Stratification Bias
    bias_ATT_mean_strat    = mean(ATT_mean_strat - true_ATT_KM, na.rm = TRUE),
    bias_ATT_median_strat  = mean(ATT_median_strat - true_ATT_median, na.rm = TRUE),  
    bias_ATT_ARR_10_strat  = mean(ATT_10_strat - ARR_true_ATT_10, na.rm = TRUE),
    bias_ATT_ARR_25_strat  = mean(ATT_25_strat - ARR_true_ATT_25, na.rm = TRUE),
    bias_ATT_ARR_50_strat  = mean(ATT_50_strat - ARR_true_ATT_50, na.rm = TRUE),
    bias_ATT_ARR_75_strat  = mean(ATT_75_strat - ARR_true_ATT_75, na.rm = TRUE),
    bias_ATT_ARR_90_strat  = mean(ATT_90_strat - ARR_true_ATT_90, na.rm = TRUE),
    
    
    # IPTW Unstabilized ATE Bias
    bias_ATE_mean_unstab   = mean(ATE_mean_iptw_unstab - true_ATE_KM, na.rm = TRUE),
    bias_ATE_median_unstab = mean(ATE_median_iptw_unstab - true_ATE_median, na.rm = TRUE),
    bias_ATE_ARR_10_unstab = mean(ARR_10_iptw_unstab - ARR_true_ATE_10, na.rm = TRUE),
    bias_ATE_ARR_25_unstab = mean(ARR_25_iptw_unstab - ARR_true_ATE_25, na.rm = TRUE),
    bias_ATE_ARR_50_unstab = mean(ARR_50_iptw_unstab - ARR_true_ATE_50, na.rm = TRUE),
    bias_ATE_ARR_75_unstab = mean(ARR_75_iptw_unstab - ARR_true_ATE_75, na.rm = TRUE),
    bias_ATE_ARR_90_unstab = mean(ARR_90_iptw_unstab - ARR_true_ATE_90, na.rm = TRUE),
    
    # IPTW Unstabilized ATT Bias
    bias_ATT_mean_iptw_unstab   = mean(ATT_mean_iptw_unstab - true_ATT_KM, na.rm = TRUE),
    bias_ATT_median_iptw_unstab = mean(ATT_median_iptw_unstab - true_ATT_median, na.rm = TRUE),   
    bias_ATT_10_iptw_unstab     = mean(ATT_10_iptw_unstab - ARR_true_ATT_10, na.rm = TRUE),
    bias_ATT_25_iptw_unstab     = mean(ATT_25_iptw_unstab - ARR_true_ATT_25, na.rm = TRUE),
    bias_ATT_50_iptw_unstab     = mean(ATT_50_iptw_unstab - ARR_true_ATT_50, na.rm = TRUE),
    bias_ATT_75_iptw_unstab     = mean(ATT_75_iptw_unstab - ARR_true_ATT_75, na.rm = TRUE),
    bias_ATT_90_iptw_unstab     = mean(ATT_90_iptw_unstab - ARR_true_ATT_90, na.rm = TRUE),
    
    
    # IPTW Stabilized ATE Bias
    bias_ATE_mean_iptw_stab   = mean(ATE_mean_iptw_stab - true_ATE_KM, na.rm = TRUE),
    bias_ATE_median_iptw_stab = mean(ATE_median_iptw_stab - true_ATE_median, na.rm = TRUE),
    bias_ATE_ARR_10_iptw_stab = mean(ARR_10_iptw_stab - ARR_true_ATE_10, na.rm = TRUE),
    bias_ATE_ARR_25_iptw_stab = mean(ARR_25_iptw_stab - ARR_true_ATE_25, na.rm = TRUE),
    bias_ATE_ARR_50_iptw_stab = mean(ARR_50_iptw_stab - ARR_true_ATE_50, na.rm = TRUE),
    bias_ATE_ARR_75_iptw_stab = mean(ARR_75_iptw_stab - ARR_true_ATE_75, na.rm = TRUE),
    bias_ATE_ARR_90_iptw_stab = mean(ARR_90_iptw_stab - ARR_true_ATE_90, na.rm = TRUE),
    
    # IPTW Stabilized ATT Bias
    bias_ATT_mean_iptw_stab   = mean(ATT_mean_iptw_stab - true_ATT_KM, na.rm = TRUE),
    bias_ATT_median_iptw_stab = mean(ATT_median_iptw_stab - true_ATT_median, na.rm = TRUE),  
    bias_ATT_10_iptw_stab     = mean(ATT_10_iptw_stab - ARR_true_ATT_10, na.rm = TRUE),
    bias_ATT_25_iptw_stab     = mean(ATT_25_iptw_stab - ARR_true_ATT_25, na.rm = TRUE),
    bias_ATT_50_iptw_stab     = mean(ATT_50_iptw_stab - ARR_true_ATT_50, na.rm = TRUE),
    bias_ATT_75_iptw_stab     = mean(ATT_75_iptw_stab - ARR_true_ATT_75, na.rm = TRUE),
    bias_ATT_90_iptw_stab     = mean(ATT_90_iptw_stab - ARR_true_ATT_90, na.rm = TRUE),
    
    # PS Adjustment ATE Bias
    bias_ATE_mean_ps_adj   = mean(ATE_mean_ps_adj - true_ATE_KM, na.rm = TRUE),
    bias_ATE_median_ps_adj = mean(ATE_median_ps_adj - true_ATE_median, na.rm = TRUE),
    bias_ATE_ARR_10_ps_adj = mean(ARR_10_ps_adj - ARR_true_ATE_10, na.rm = TRUE),
    bias_ATE_ARR_25_ps_adj = mean(ARR_25_ps_adj - ARR_true_ATE_25, na.rm = TRUE),
    bias_ATE_ARR_50_ps_adj = mean(ARR_50_ps_adj - ARR_true_ATE_50, na.rm = TRUE),
    bias_ATE_ARR_75_ps_adj = mean(ARR_75_ps_adj - ARR_true_ATE_75, na.rm = TRUE),
    bias_ATE_ARR_90_ps_adj = mean(ARR_90_ps_adj - ARR_true_ATE_90, na.rm = TRUE),
    
    # PS Adjustment ATT Bias
    bias_ATT_mean_ps_adj = mean(ATT_mean_ps_adj - true_ATT_KM, na.rm = TRUE),
    bias_ATT_median_ps_adj = mean(ATT_median_ps_adj - true_ATT_median, na.rm = TRUE),
    bias_ATT_ARR_10_ps_adj = mean(ATT_10_ps_adj - ARR_true_ATT_10, na.rm = TRUE),
    bias_ATT_ARR_25_ps_adj = mean(ATT_25_ps_adj - ARR_true_ATT_25, na.rm = TRUE),
    bias_ATT_ARR_50_ps_adj = mean(ATT_50_ps_adj - ARR_true_ATT_50, na.rm = TRUE),
    bias_ATT_ARR_75_ps_adj = mean(ATT_75_ps_adj - ARR_true_ATT_75, na.rm = TRUE),
    bias_ATT_ARR_90_ps_adj = mean(ATT_90_ps_adj - ARR_true_ATT_90, na.rm = TRUE),
    
    
    
    
    # MSE matching
    mse_ATT_mean_match = mean((ATT_mean_match - true_ATT_KM)^2, na.rm = TRUE),
    mse_ATT_median_match = mean((ATT_median_match - true_ATT_median)^2, na.rm = TRUE),
    mse_ARR_10_match = mean((ARR_10_match - ARR_true_ATT_10)^2, na.rm = TRUE),
    mse_ARR_25_match = mean((ARR_25_match - ARR_true_ATT_25)^2, na.rm = TRUE),
    mse_ARR_50_match = mean((ARR_50_match - ARR_true_ATT_50)^2, na.rm = TRUE),
    mse_ARR_75_match = mean((ARR_75_match - ARR_true_ATT_75)^2, na.rm = TRUE),
    mse_ARR_90_match = mean((ARR_90_match - ARR_true_ATT_90)^2, na.rm = TRUE),
    
    
    # MSE stratification ATE
    mse_ATE_mean_strat   = mean((ATE_mean_strat - true_ATE_KM)^2, na.rm = TRUE),
    mse_ATE_median_strat = mean((ATE_median_strat - true_ATE_median)^2, na.rm = TRUE),
    mse_ATE_ARR_10_strat = mean((ARR_10_strat - ARR_true_ATE_10)^2, na.rm = TRUE),
    mse_ATE_ARR_25_strat = mean((ARR_25_strat - ARR_true_ATE_25)^2, na.rm = TRUE),
    mse_ATE_ARR_50_strat = mean((ARR_50_strat - ARR_true_ATE_50)^2, na.rm = TRUE),
    mse_ATE_ARR_75_strat = mean((ARR_75_strat - ARR_true_ATE_75)^2, na.rm = TRUE),
    mse_ATE_ARR_90_strat = mean((ARR_90_strat - ARR_true_ATE_90)^2, na.rm = TRUE),
    
    
    # MSE stratification ATT
    mse_ATT_mean_strat   = mean((ATT_mean_strat - true_ATT_KM)^2, na.rm = TRUE),
    mse_ATT_median_strat = mean((ATT_median_strat - true_ATT_median)^2, na.rm = TRUE),
    mse_ATT_ARR_10_strat = mean((ATT_10_strat - ARR_true_ATT_10)^2, na.rm = TRUE),
    mse_ATT_ARR_25_strat = mean((ATT_25_strat - ARR_true_ATT_25)^2, na.rm = TRUE),
    mse_ATT_ARR_50_strat = mean((ATT_50_strat - ARR_true_ATT_50)^2, na.rm = TRUE),
    mse_ATT_ARR_75_strat = mean((ATT_75_strat - ARR_true_ATT_75)^2, na.rm = TRUE),
    mse_ATT_ARR_90_strat = mean((ATT_90_strat - ARR_true_ATT_90)^2, na.rm = TRUE),
    
    
    # MSE IPTW unstabilized ATE
    mse_ATE_mean_iptw_unstab   = mean((ATE_mean_iptw_unstab - true_ATE_KM)^2, na.rm = TRUE),
    mse_ATE_median_iptw_unstab = mean((ATE_median_iptw_unstab - true_ATE_median)^2, na.rm = TRUE),
    mse_ATE_ARR_10_iptw_unstab = mean((ARR_10_iptw_unstab - ARR_true_ATE_10)^2, na.rm = TRUE),
    mse_ATE_ARR_25_iptw_unstab = mean((ARR_25_iptw_unstab - ARR_true_ATE_25)^2, na.rm = TRUE),
    mse_ATE_ARR_50_iptw_unstab = mean((ARR_50_iptw_unstab - ARR_true_ATE_50)^2, na.rm = TRUE),
    mse_ATE_ARR_75_iptw_unstab = mean((ARR_75_iptw_unstab - ARR_true_ATE_75)^2, na.rm = TRUE),
    mse_ATE_ARR_90_iptw_unstab = mean((ARR_90_iptw_unstab - ARR_true_ATE_90)^2, na.rm = TRUE),
    
    # MSE IPTW unstabilized ATT
    mse_ATT_mean_iptw_unstab   = mean((ATT_mean_iptw_unstab - true_ATT_KM)^2, na.rm = TRUE),
    mse_ATT_median_iptw_unstab = mean((ATT_median_iptw_unstab - true_ATT_median)^2, na.rm = TRUE),
    mse_ATT_10_iptw_unstab     = mean((ATT_10_iptw_unstab - ARR_true_ATT_10)^2, na.rm = TRUE),
    mse_ATT_25_iptw_unstab     = mean((ATT_25_iptw_unstab - ARR_true_ATT_25)^2, na.rm = TRUE),
    mse_ATT_50_iptw_unstab     = mean((ATT_50_iptw_unstab - ARR_true_ATT_50)^2, na.rm = TRUE),
    mse_ATT_75_iptw_unstab     = mean((ATT_75_iptw_unstab - ARR_true_ATT_75)^2, na.rm = TRUE),
    mse_ATT_90_iptw_unstab     = mean((ATT_90_iptw_unstab - ARR_true_ATT_90)^2, na.rm = TRUE),
    
    # MSE IPTW stabilized ATE
    mse_ATE_mean_iptw_stab   = mean((ATE_mean_iptw_stab - true_ATE_KM)^2, na.rm = TRUE),
    mse_ATE_median_iptw_stab = mean((ATE_median_iptw_stab - true_ATE_median)^2, na.rm = TRUE),
    mse_ATE_ARR_10_iptw_stab = mean((ARR_10_iptw_stab - ARR_true_ATE_10)^2, na.rm = TRUE),
    mse_ATE_ARR_25_iptw_stab = mean((ARR_25_iptw_stab - ARR_true_ATE_25)^2, na.rm = TRUE),
    mse_ATE_ARR_50_iptw_stab = mean((ARR_50_iptw_stab - ARR_true_ATE_50)^2, na.rm = TRUE),
    mse_ATE_ARR_75_iptw_stab = mean((ARR_75_iptw_stab - ARR_true_ATE_75)^2, na.rm = TRUE),
    mse_ATE_ARR_90_iptw_stab = mean((ARR_90_iptw_stab - ARR_true_ATE_90)^2, na.rm = TRUE),
    
    # MSE IPTW stabilized ATT
    mse_ATT_mean_iptw_stab   = mean((ATT_mean_iptw_stab - true_ATT_KM)^2, na.rm = TRUE),
    mse_ATT_median_iptw_stab = mean((ATT_median_iptw_stab - true_ATT_median)^2, na.rm = TRUE),
    mse_ATT_10_iptw_stab     = mean((ATT_10_iptw_stab - ARR_true_ATT_10)^2, na.rm = TRUE),
    mse_ATT_25_iptw_stab     = mean((ATT_25_iptw_stab - ARR_true_ATT_25)^2, na.rm = TRUE),
    mse_ATT_50_iptw_stab     = mean((ATT_50_iptw_stab - ARR_true_ATT_50)^2, na.rm = TRUE),
    mse_ATT_75_iptw_stab     = mean((ATT_75_iptw_stab - ARR_true_ATT_75)^2, na.rm = TRUE),
    mse_ATT_90_iptw_stab     = mean((ATT_90_iptw_stab - ARR_true_ATT_90)^2, na.rm = TRUE),
    
    
    # PS Adjustment ATE
    mse_ATE_mean_ps_adj   = mean((ATE_mean_ps_adj - true_ATE_KM)^2, na.rm = TRUE),
    mse_ATE_median_ps_adj = mean((ATE_median_ps_adj - true_ATE_median)^2, na.rm = TRUE),
    mse_ATE_ARR_10_ps_adj = mean((ARR_10_ps_adj - ARR_true_ATE_10)^2, na.rm = TRUE),
    mse_ATE_ARR_25_ps_adj = mean((ARR_25_ps_adj - ARR_true_ATE_25)^2, na.rm = TRUE),
    mse_ATE_ARR_50_ps_adj = mean((ARR_50_ps_adj - ARR_true_ATE_50)^2, na.rm = TRUE),
    mse_ATE_ARR_75_ps_adj = mean((ARR_75_ps_adj - ARR_true_ATE_75)^2, na.rm = TRUE),
    mse_ATE_ARR_90_ps_adj = mean((ARR_90_ps_adj - ARR_true_ATE_90)^2, na.rm = TRUE),
    
    # PS Adjustment ATT
    mse_ATT_mean_ps_adj   = mean((ATT_mean_ps_adj - true_ATT_KM)^2, na.rm = TRUE),
    mse_ATT_median_ps_adj = mean((ATT_median_ps_adj - true_ATT_median)^2, na.rm = TRUE),
    mse_ATT_ARR_10_ps_adj = mean((ATT_10_ps_adj - ARR_true_ATT_10)^2, na.rm = TRUE),
    mse_ATT_ARR_25_ps_adj = mean((ATT_25_ps_adj - ARR_true_ATT_25)^2, na.rm = TRUE),
    mse_ATT_ARR_50_ps_adj = mean((ATT_50_ps_adj - ARR_true_ATT_50)^2, na.rm = TRUE),
    mse_ATT_ARR_75_ps_adj = mean((ATT_75_ps_adj - ARR_true_ATT_75)^2, na.rm = TRUE),
    mse_ATT_ARR_90_ps_adj = mean((ATT_90_ps_adj - ARR_true_ATT_90)^2, na.rm = TRUE),
    
    
    # --- Standard errors ---
    se_ATT_mean_match   = sd(ATT_mean_match, na.rm = TRUE) / sqrt(M),
    se_ATT_median_match = sd(ATT_median_match, na.rm = TRUE) / sqrt(M),
    
    se_ATE_mean_strat   = sd(ATE_mean_strat, na.rm = TRUE) / sqrt(M),
    se_ATE_median_strat = sd(ATE_median_strat, na.rm = TRUE) / sqrt(M),
    se_ATT_mean_strat   = sd(ATT_mean_strat, na.rm = TRUE) / sqrt(M),
    se_ATT_median_strat = sd(ATT_median_strat, na.rm = TRUE) / sqrt(M),
    
    se_ATE_mean_iptw_unstab   = sd(ATE_mean_iptw_unstab, na.rm = TRUE) / sqrt(M),
    se_ATE_median_iptw_unstab = sd(ATE_median_iptw_unstab, na.rm = TRUE) / sqrt(M),
    se_ATT_mean_iptw_unstab   = sd(ATT_mean_iptw_unstab, na.rm = TRUE) / sqrt(M),
    se_ATT_median_iptw_unstab = sd(ATT_median_iptw_unstab, na.rm = TRUE) / sqrt(M),
    
    se_ATE_mean_iptw_stab   = sd(ATE_mean_iptw_stab, na.rm = TRUE) / sqrt(M),
    se_ATE_median_iptw_stab = sd(ATE_median_iptw_stab, na.rm = TRUE) / sqrt(M),
    se_ATT_mean_iptw_stab   = sd(ATT_mean_iptw_stab, na.rm = TRUE) / sqrt(M),
    se_ATT_median_iptw_stab = sd(ATT_median_iptw_stab, na.rm = TRUE) / sqrt(M),
    
    se_ATE_mean_ps_adj   = sd(ATE_mean_ps_adj, na.rm = TRUE) / sqrt(M),
    se_ATE_median_ps_adj = sd(ATE_median_ps_adj, na.rm = TRUE) / sqrt(M),
    se_ATT_mean_ps_adj   = sd(ATT_mean_ps_adj, na.rm = TRUE) / sqrt(M),
    se_ATT_median_ps_adj = sd(ATT_median_ps_adj, na.rm = TRUE) / sqrt(M),
    
    .groups = "drop"
    
  )

# Step 2: Compute CIs and coverage using mutate
performance_metrics <- performance_metrics %>%
  mutate(
    # --- 95% CI ---
    ci_low_ATT_mean_match   = est_ATT_mean_match - 1.96 * se_ATT_mean_match,
    ci_high_ATT_mean_match  = est_ATT_mean_match + 1.96 * se_ATT_mean_match,
    ci_low_ATT_median_match = est_ATT_median_match - 1.96 * se_ATT_median_match,
    ci_high_ATT_median_match= est_ATT_median_match + 1.96 * se_ATT_median_match,
    
    ci_low_ATE_mean_strat   = est_ATE_mean_strat - 1.96 * se_ATE_mean_strat,
    ci_high_ATE_mean_strat  = est_ATE_mean_strat + 1.96 * se_ATE_mean_strat,
    ci_low_ATE_median_strat = est_ATE_median_strat - 1.96 * se_ATE_median_strat,
    ci_high_ATE_median_strat= est_ATE_median_strat + 1.96 * se_ATE_median_strat,
    
    ci_low_ATT_mean_strat   = est_ATT_mean_strat - 1.96 * se_ATT_mean_strat,
    ci_high_ATT_mean_strat  = est_ATT_mean_strat + 1.96 * se_ATT_mean_strat,
    ci_low_ATT_median_strat = est_ATT_median_strat - 1.96 * se_ATT_median_strat,
    ci_high_ATT_median_strat= est_ATT_median_strat + 1.96 * se_ATT_median_strat,
    
    ci_low_ATE_mean_unstab   = est_ATE_mean_unstab - 1.96 * se_ATE_mean_iptw_unstab,
    ci_high_ATE_mean_unstab  = est_ATE_mean_unstab + 1.96 * se_ATE_mean_iptw_unstab,
    ci_low_ATE_median_unstab = est_ATE_median_unstab - 1.96 * se_ATE_median_iptw_unstab,
    ci_high_ATE_median_unstab= est_ATE_median_unstab + 1.96 * se_ATE_median_iptw_unstab,
    ci_low_ATT_mean_unstab   = est_ATT_mean_iptw_unstab - 1.96 * se_ATT_mean_iptw_unstab,
    ci_high_ATT_mean_unstab  = est_ATT_mean_iptw_unstab + 1.96 * se_ATT_mean_iptw_unstab,
    ci_low_ATT_median_unstab = est_ATT_median_iptw_unstab - 1.96 * se_ATT_median_iptw_unstab,
    ci_high_ATT_median_unstab= est_ATT_median_iptw_unstab + 1.96 * se_ATT_median_iptw_unstab,
    
    ci_low_ATE_mean_iptw_stab   = est_ATE_mean_iptw_stab - 1.96 * se_ATE_mean_iptw_stab,
    ci_high_ATE_mean_iptw_stab  = est_ATE_mean_iptw_stab + 1.96 * se_ATE_mean_iptw_stab,
    ci_low_ATE_median_iptw_stab = est_ATE_median_iptw_stab - 1.96 * se_ATE_median_iptw_stab,
    ci_high_ATE_median_iptw_stab= est_ATE_median_iptw_stab + 1.96 * se_ATE_median_iptw_stab,
    ci_low_ATT_mean_iptw_stab   = est_ATT_mean_iptw_stab - 1.96 * se_ATT_mean_iptw_stab,
    ci_high_ATT_mean_iptw_stab  = est_ATT_mean_iptw_stab + 1.96 * se_ATT_mean_iptw_stab,
    ci_low_ATT_median_iptw_stab = est_ATT_median_iptw_stab - 1.96 * se_ATT_median_iptw_stab,
    ci_high_ATT_median_iptw_stab= est_ATT_median_iptw_stab + 1.96 * se_ATT_median_iptw_stab,
    
    ci_low_ATE_mean_ps_adj   = est_ATE_mean_ps_adj - 1.96 * se_ATE_mean_ps_adj,
    ci_high_ATE_mean_ps_adj  = est_ATE_mean_ps_adj + 1.96 * se_ATE_mean_ps_adj,
    ci_low_ATE_median_ps_adj = est_ATE_median_ps_adj - 1.96 * se_ATE_median_ps_adj,
    ci_high_ATE_median_ps_adj= est_ATE_median_ps_adj + 1.96 * se_ATE_median_ps_adj,
    ci_low_ATT_mean_ps_adj   = est_ATT_mean_ps_adj - 1.96 * se_ATT_mean_ps_adj,
    ci_high_ATT_mean_ps_adj  = est_ATT_mean_ps_adj + 1.96 * se_ATT_mean_ps_adj,
    ci_low_ATT_median_ps_adj = est_ATT_median_ps_adj - 1.96 * se_ATT_median_ps_adj,
    ci_high_ATT_median_ps_adj= est_ATT_median_ps_adj + 1.96 * se_ATT_median_ps_adj,
    
    # --- Coverage ---
    coverage_ATT_mean_match   = mean(ci_low_ATT_mean_match <= true_ATT_KM & ci_high_ATT_mean_match >= true_ATT_KM),
    coverage_ATT_median_match = mean(ci_low_ATT_median_match <= true_ATT_median & ci_high_ATT_median_match >= true_ATT_median),
    
    coverage_ATE_mean_strat   = mean(ci_low_ATE_mean_strat <= true_ATE_KM & ci_high_ATE_mean_strat >= true_ATE_KM),
    coverage_ATE_median_strat = mean(ci_low_ATE_median_strat <= true_ATE_median & ci_high_ATE_median_strat >= true_ATE_median),
    coverage_ATT_mean_strat   = mean(ci_low_ATT_mean_strat <= true_ATT_KM & ci_high_ATT_mean_strat >= true_ATT_KM),
    coverage_ATT_median_strat = mean(ci_low_ATT_median_strat <= true_ATT_median & ci_high_ATT_median_strat >= true_ATT_median),
    
    coverage_ATE_mean_unstab   = mean(ci_low_ATE_mean_unstab <= true_ATE_KM & ci_high_ATE_mean_unstab >= true_ATE_KM),
    coverage_ATE_median_unstab = mean(ci_low_ATE_median_unstab <= true_ATE_median & ci_high_ATE_median_unstab >= true_ATE_median),
    coverage_ATT_mean_unstab   = mean(ci_low_ATT_mean_unstab <= true_ATT_KM & ci_high_ATT_mean_unstab >= true_ATT_KM),
    coverage_ATT_median_unstab = mean(ci_low_ATT_median_unstab <= true_ATT_median & ci_high_ATT_median_unstab >= true_ATT_median),
    
    coverage_ATE_mean_iptw_stab   = mean(ci_low_ATE_mean_iptw_stab <= true_ATE_KM & ci_high_ATE_mean_iptw_stab >= true_ATE_KM),
    coverage_ATE_median_iptw_stab = mean(ci_low_ATE_median_iptw_stab <= true_ATE_median & ci_high_ATE_median_iptw_stab >= true_ATE_median),
    coverage_ATT_mean_iptw_stab   = mean(ci_low_ATT_mean_iptw_stab <= true_ATT_KM & ci_high_ATT_mean_iptw_stab >= true_ATT_KM),
    coverage_ATT_median_iptw_stab = mean(ci_low_ATT_median_iptw_stab <= true_ATT_median & ci_high_ATT_median_iptw_stab >= true_ATT_median),
    
    coverage_ATE_mean_ps_adj   = mean(ci_low_ATE_mean_ps_adj <= true_ATE_KM & ci_high_ATE_mean_ps_adj >= true_ATE_KM),
    coverage_ATE_median_ps_adj = mean(ci_low_ATE_median_ps_adj <= true_ATE_median & ci_high_ATE_median_ps_adj >= true_ATE_median),
    coverage_ATT_mean_ps_adj   = mean(ci_low_ATT_mean_ps_adj <= true_ATT_KM & ci_high_ATT_mean_ps_adj >= true_ATT_KM),
    coverage_ATT_median_ps_adj = mean(ci_low_ATT_median_ps_adj <= true_ATT_median & ci_high_ATT_median_ps_adj >= true_ATT_median)
  )

