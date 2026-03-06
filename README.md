# Propensity Score Methods for Estimating Absolute Treatment Effects in Survival Analysis
[DOI]: (https://doi.org/10.5281/zenodo.18886413)
This repository contains R code used for the simulation study evaluating the performance of propensity score methods for estimating absolute treatment effects (ATE and ATT) in mean and median survival time.

## Contents
- simulation_data_generation.R : generates simulated survival data
- ps_matching.R : analysis using propensity score matching
- ps_stratification.R : analysis using propensity score stratification
- stabilized_IPTW.R : analysis using stabilized inverse probability of treatment weighting
- ps_adjustment.R : regression adjustment using propensity scores

## Software
The simulations were conducted in R (version 4.4.2).

Required packages:
- survival
- MatchIt
- WeightIt
- survey

## Reproducing the Results
1. Run `simulation_data_generation.R`
2. Run each analysis script
3. The scripts generate bias and MSE results used in Figures 1- 3 of the manuscript.

## Author
Mahin Tatari
