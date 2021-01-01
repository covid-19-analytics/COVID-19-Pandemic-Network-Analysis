# COVID-19 Pandemic Network Analysis

On Topological Properties of COVID-19: Predicting and Controling Pandemic Risk with Network Statistics

# Backgrounds
This study proposes a network analysis to assess global pandemic risk by linking 164 countries in pandemic networks, where links between countries were specified by the level of ‘co-movement’ of newly confirmed COVID-19 cases. More countries showing increase in the COVID-19 cases simultaneously will signal the pandemic prevalent over the world. The network density, clustering coefficients, and assortativity in the pandemic networks provide early warning signals of the pandemic in late February 2020. We propose a preparedness pandemic risk score for prediction and a severity risk score for pandemic control.

--------------------------------------------------------------

# R Program

All data records were generated using code developed in R version 3.6.3.

Make sure you run the codes in the following order. Otherwise, unexpected error/ issues might occur unless you know what you are doing.
All of the following code required default/ target values to run properly.

1. [_01a_estimate_world_recovery_rate.R] Aim: Estimate world revoery rate by different imputation cutoff 

2. [_01b_imputation_recovery_cases_vF.R] Aim: Imputation on recovery cases by estimated daily recovery rate by filtering the countries that are weird (below cutoff)

3. [_02a_reproduce_final_results_by_weighted_vF.R] Aim: Produce network statistics, risk scores, risk contribution by weighted matrix 

4. [_02b_reproduce_final_results_by_binarized_vF.R] Aim: Produce network statistics, risk scores, risk contribution by binarized matrix (r= 0.4, 0.5, 0.6), and different imputation cutoff (0.5, 0.6, 0.7, 0.8)

5. [_02c_simulate_network_results.R] Aim: Simulate the network statistics by network density

# Demo run

Please refer to our sample run "_sample_run.R" for the details: 

```R


# *************************************************************************
# [_sample_run.R]
# 
#   Aim: Provide a sample run procedure 
# *************************************************************************


# =================================================================
# Run -------------------------------------------------------------

# =================================================================
# Step 1a: Estimate the world recovery rate ----------

proj_name <- ""
target_date_str <- "Dec29"

# We will just perform imputation on the data points that started from May 2020 -----------
imputation_date_from <- ymd("2020-05-01") 

v_cutoff <- seq(0.5, 0.8, by= 0.1) # imputation cutoff(s)

source(here::here("", "R", "_01a_estimate_world_recovery_rate.R"))
rm(list= ls())
# ---------------------------------

Sys.sleep(3)


# =================================================================
# Step 1b: Imputation on recovery numbers ----------

proj_name <- ""
target_date_str <- "Dec29"

# We will just perform imputation on the data points that started from May 2020 -----------
imputation_date_from <- ymd("2020-05-01") 

v_cutoff <- seq(0.5, 0.8, by= 0.1) # imputation cutoff(s)

# Step 1b: Imputation on recovery numbers ----------
source(here::here("", "R", "_01b_imputation_recovery_cases_vF.R"))
rm(list= ls())
# ---------------------------------

Sys.sleep(3)




# =================================================================
# Step 2a: Produce results by weighted matrix ----------

proj_name <- ""

cutoff_I <- 0.7 # imputation cutoff

date_str <- "Dec29"
target_as_of_date <- ymd("2020-12-29")

source(here::here("", "R", "_02a_reproduce_final_results_by_weighted_vF.R"))
rm(list= ls())
# ---------------------------------

Sys.sleep(3)




# =================================================================
# Step 2b: Produce results by binarized matrix (r, rR) ----------

v_cutoff <- seq(0.5, 0.8, by= 0.1) 

for (tmp_cutoff in v_cutoff) {
  
  proj_name <- ""
  
  date_str <- "Dec29"
  target_as_of_date <- ymd("2020-12-29")
  
  cutoff_I <- tmp_cutoff
  threshold_val <- 0.5
  
  cat("cutoff_I: ", cutoff_I, " | ", "threshold_val: ", threshold_val, "\n\n")
  
  source(here::here("", "R", "_02b_reproduce_final_results_by_binarized_vF.R"))
  rm(list= ls())
}

# ---------------------------------

Sys.sleep(3)

v_threshold <- seq(0.4, 0.6, by= 0.1) 

for (tmp_threshold in v_threshold) {
  proj_name <- ""
  
  date_str <- "Dec29"
  target_as_of_date <- ymd("2020-12-29")
  
  cutoff_I <- 0.7
  threshold_val <- tmp_threshold
  
  cat(rep("-", 10), rep("-", 10), "\n")
  cat("cutoff_I: ", cutoff_I, " | ", "threshold_val: ", threshold_val, "\n\n")
  
  source(here::here("", "R", "_02b_reproduce_final_results_by_binarized_vF.R"))
  rm(list= ls())
}


# ---------------------------------

Sys.sleep(3)





# =================================================================
# Step 2c: Simulate the network statistics

proj_name <- ""

seed <- 1010
set.seed(seed)

NUM_CORE <- 2 

date_str <- "Dec29"
date_end <- target_as_of_date <- ymd("2020-12-29")

threshold_val <- 0.5 # correlation threshold
cutoff_I <- 0.7 # imputation cutoff

source(here::here("", "R", "_02c_simulate_network_results.R"))
rm(list= ls())
# ---------------------------------

Sys.sleep(3)




# =================================================================



```
