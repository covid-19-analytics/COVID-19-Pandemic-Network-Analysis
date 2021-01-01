
# *************************************************************************
# [_01a_estimate_world_recovery_rate.R]
# 
#   Aim: Estimate world revoery rate by different imputation cutoff 
# *************************************************************************

# source(here::here("", "R", "_01a_estimate_world_recovery_rate.R"))

# =================================================================
# Load Library ----------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE) # Suppress summarize info

library(readr)

library(lubridate)

library(ggplot2)

# # =================================================================
# # Basic setup -----------------------------------------------------
# 
# seed <- 1010
# set.seed(seed)
# 
# proj_name <- ""
# target_date_str <- "Dec29"
# 
# # We will just perform imputation on the data points that started from May 2020 -----------
# imputation_date_from <- ymd("2020-05-01") 
# 
# v_cutoff <- seq(0.5, 0.8, by= 0.1)

# =================================================================
# Load Data -------------------------------------------------------

# Daily reported confirmed cases -----------
df_Xit <- read_csv(
  here::here(proj_name, "raw_data", 
             sprintf("upto_%s_FINAL_confirmed_World.csv", target_date_str))) %>% 
  tidyr::pivot_longer(-date, names_to= "region_EN", values_to= "Xit") %>%
  dplyr::mutate(Xit= ifelse(is.na(Xit), 0, Xit)) %>%
  dplyr::mutate(date= dmy(date)) %>%
  dplyr::arrange(date)

# Daily reported recovered cases -----------
df_Rit <- read_csv(
  here::here(proj_name, "raw_data", 
             sprintf("upto_%s_FINAL_recovered_World.csv", target_date_str))) %>% 
  tidyr::pivot_longer(-date, names_to= "region_EN", values_to= "Rit") %>%
  dplyr::mutate(Rit= ifelse(is.na(Rit), 0, Rit)) %>%
  dplyr::mutate(date= dmy(date)) %>%
  dplyr::arrange(date)


# =================================================================
# Data aggregation (cumulated total) ------------------------------

df_case <- left_join(df_Xit, df_Rit, by= c("date", "region_EN"))

df_cs_case <- df_case %>%
  dplyr::group_by(region_EN) %>%
  dplyr::mutate(
    cs_Xit= cumsum(Xit),
    cs_Rit= cumsum(Rit)
  ) %>%
  dplyr::mutate(
    Rt= cs_Rit / cs_Xit
  )

# df_cs_case$Rt %>% summary()
# {df_cs_case$Rt > 1 } %>% table()
# { is.na(df_cs_case$Rt) } %>% table()

# -----------------------------------------

# Apply cut-off threshold -----------
lapply(v_cutoff, function(tmp_cutoff) {
  
  df_cs_case <- df_cs_case %>%
    dplyr::mutate(
      flag_below_cutoff= Rt < tmp_cutoff)
  
  tmp_df_cs_case <- df_cs_case %>%
    dplyr::filter(flag_below_cutoff!=TRUE) %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(num_node= n()) %>%
    dplyr::summarise(
      grand_Xit= sum(cs_Xit),
      grand_Rit= sum(cs_Rit),
      num_node= n()) %>%
    dplyr::mutate(grand_Rt= grand_Rit / grand_Xit)
  
  tmp_df_cs_case$cutoff_I <- tmp_cutoff
  
  return(tmp_df_cs_case)
  
}) -> l_final

df_final <- do.call("rbind", l_final)

# --------------------------------------------

# [Output] Store the estimated recovered numbers for later usage ----------
df_final %>%
  write_csv(., here::here(
    proj_name, "output", 
    sprintf("df_estimated_Rt_%s_by_cutoffs.csv", target_date_str)))

# =================================================================
  
