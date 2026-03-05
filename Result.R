##################################################### FANAL RESULT    

# -------------------- Final Results List -------------------- #
final_results <- list(
  True = list(
    ATE_mean = true_effects$true_ATE_KM,
    ATE_median = true_effects$true_ATE_median,
    ATT_mean = true_effects$true_ATT_KM,
    ATT_median = true_effects$true_ATT_median,
    ARR_ATE = true_effects$ARR_true_ATE,
    ARR_ATT = true_effects$ARR_true_ATT
  ),
  Matching = ATT_results_match,
  Stratification = list(
    ATE_mean_strat = results_strat$ATE$ATE_mean_strat,
    ATE_median_strat = results_strat$ATE$ATE_median_strat,
    ARR_ATE_strat = results_strat$ATE$ARR_strat_ATE,
    ATT_mean_strat = results_strat$ATT$ATT_mean_strat,
    ATT_median_strat = results_strat$ATT$ATT_median_strat,
    ARR_ATT_strat = results_strat$ATT$ARR_strat_ATT
  ),
  IPTW = list(
    ATE_mean_stab = ATE_iptw_results$ATE_mean_stab,
    ATE_median_stab = ATE_iptw_results$ATE_median_stab,
    ATE_ARR_stab = ATE_iptw_results$ATE_ARR_stab,
    ATE_mean_unstab = ATE_iptw_results$ATE_mean_unstab,
    ATE_median_unstab = ATE_iptw_results$ATE_median_unstab,
    ATE_ARR_unstab = ATE_iptw_results$ATE_ARR_unstab,
    ATT_mean_stab = ATT_iptw_results$ATT_mean_stab,
    ATT_median_stab = ATT_iptw_results$ATT_median_stab,
    ATT_ARR_stab = ATT_iptw_results$ATT_ARR_stab,
    ATT_mean_unstab = ATT_iptw_results$ATT_mean_unstab,
    ATT_median_unstab = ATT_iptw_results$ATT_median_unstab,
    ATT_ARR_unstab = ATT_iptw_results$ATT_ARR_unstab
  ),
  PS_adjust = list(
    ATE_mean_adjust = results_ATE_adjust$ATE_mean_adjust,
    ATE_median_adjust = results_ATE_adjust$ATE_median_adjust,
    ATE_ARR_adjust = results_ATE_adjust$ATE_ARR_adjust,
    ATT_mean_adjust = results_ATT_adjust$ATT_mean_adjust,
    ATT_median_adjust = results_ATT_adjust$ATT_median_adjust,
    ATT_ARR_adjust = results_ATT_adjust$ATT_ARR_adjust
  )
)

# -------------------- Return -------------------- #
return(list(
  final_results = final_results,
  True = final_results$True,
  data = data_out,
  mean_surv_treated_observed = mean_surv_treated_observed,
  mean_surv_control_observed = mean_surv_control_observed,
  ATE_mean_observed = ATE_mean_observed,
  ATT_mean_observed = ATT_mean_observed,
  ATT_results_match = ATT_results_match,
  results_strat = results_strat,
  ATE_iptw_results = ATE_iptw_results,
  ATT_iptw_results = ATT_iptw_results,
  results_ATE_adjust = results_ATE_adjust,
  results_ATT_adjust = results_ATT_adjust
))

}, error = function(e) {
  message("Error occurred: ", e$message)
  return(NULL)
})
}

sim_results <- run_simulation(n = 10000, lambda = 0.00002, eta = 2, censoring_p = 0.6, HR = 1.10, target_treat_prop = 0.10)
