
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

