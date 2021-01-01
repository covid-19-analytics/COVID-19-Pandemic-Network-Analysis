
# *************************************************************************
# [_01b_imputation_recovery_cases_vF.R]
# 
#   Aim: Imputation on recovery cases by estimated daily recovery rate
#     by filtering the countries that are weird (below cutoff)
# *************************************************************************

# source(here::here("", "R", "_01b_imputation_recovery_cases_vF.R"))

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

df_final <- read_csv(here::here(
  proj_name, "output", 
  sprintf("df_estimated_Rt_%s_by_cutoffs.csv", target_date_str)))

# =================================================================

actual_cs_Rit <- df_cs_case %>%
  dplyr::select(date, region_EN, cs_Xit, cs_Rit) %>%
  dplyr::filter(date < imputation_date_from)

for (tmp_cutoff in v_cutoff) {
  
  estimate_cs_Rit <- df_final %>%
    dplyr::select(date, cutoff_I, grand_Rt) %>%
    dplyr::rename(Rt_hat= grand_Rt) %>% 
    dplyr::filter(cutoff_I==tmp_cutoff) %>%
    
    left_join(dplyr::filter(df_cs_case, date>=imputation_date_from), ., by= c("date"))
  
  estimate_cs_Rit <- estimate_cs_Rit %>%
    dplyr::mutate(need_impute= Rt < cutoff_I) %>%
    dplyr::mutate(cs_Rit_hat= ifelse(
      need_impute==TRUE, 
      ceiling(cs_Xit * Rt_hat), cs_Rit))
  
  df_cs_estimate <- estimate_cs_Rit %>% 
    dplyr::select(date, region_EN, cutoff_I, Rt, Rt_hat, cs_Xit, need_impute, cs_Rit, cs_Rit_hat) %>%
    rbind(dplyr::mutate(actual_cs_Rit, cs_Rit_hat= cs_Rit), .)
  df_cs_estimate$cutoff_I <- tmp_cutoff
  
  # --------------------------------------------
  
  df_estimate <- df_cs_estimate %>%
    dplyr::arrange(date, region_EN) %>%
    dplyr::group_by(region_EN) %>%
    dplyr::mutate(Rit_hat= cs_Rit_hat-lag(cs_Rit_hat)) %>%
    
    # Fix the potential negative Rit_hat ----------
    dplyr::mutate(Rit_hat= ifelse(Rit_hat<0, 0, Rit_hat))
  
  tmp_df <- df_estimate %>%
    dplyr::select(date, region_EN, Rit_hat) %>%
    tidyr::pivot_wider(names_from= region_EN, values_from= Rit_hat)
  
  
  # [Output] Store different sets of estimated recovery numbers by cutoff -----------
  write_csv(tmp_df,
            path= here::here(
              proj_name, "raw_data", 
              sprintf("upto_%s_estimated_Rit_cutoff%s.csv", 
                      target_date_str, tmp_cutoff)))
  
}

# =================================================================

