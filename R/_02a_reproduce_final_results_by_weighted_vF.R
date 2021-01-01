
# *************************************************************************
# [_02a_reproduce_final_results_by_weighted_vF.R]
# 
#   Aim: Produce network statistics, risk scores, risk contribution by weighted matrix 
# *************************************************************************

# source(here::here("", "R", "_02a_reproduce_final_results_by_weighted_vF.R"))

# =================================================================
# Load Library ----------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE) # Suppress summarize info

library(ggplot2)

library(tidyr)

library(readr)
library(lubridate)

library(rnaturalearth)
library(sp)

library(spdplyr)

library(rgdal)
library(maptools)
# library(rgeos)

library(RColorBrewer)

library(ggrepel)
library(ggsflabel)

# library(tidygraph)
library(ggraph)

library(igraph)

library(ggforce)
library(viridis)

library(ggpubr)

library(ggthemes)
library(cowplot)

library(Rlab)

library(here)

# =================================================================
# ***** Basic Setup -----------------------------------------------

# proj_name <- ""
# 
# # Set Reproducible seed ----------
# seed <- 1010
# set.seed(seed)

# actual_date_begin (DO NOT CHANGE this) ----------
actual_date_begin <- ymd("2020-01-22")

# cutoff_I <- 0.7
tmp_weight <- "wp" # ****************** DO NOT CHANGE THIS

# date_str <- "Dec29"
# target_as_of_date <- ymd("2020-12-29")

# special case: weighted matrix
threshold_val <- 0.0 # ****************** DO NOT CHANGE THIS

# =========================================================================

# ---------- [ ]: historical data and region meta data --------------------
target_data_files <- c(
  sprintf("upto_%s_FINAL_confirmed_World.csv", date_str), # historical data
  sprintf("region_%s.csv", "World")) # region meta data

# ---------- [ ]: Network edge threshold ----------
egde_threshold <- data.frame(
  # threshold_val= c(0.5),
  threshold_val= c(threshold_val),
  # threshold_type= c(rep("r", 1)),
  threshold_type= c(rep(tmp_weight, 1)),
  stringsAsFactors= FALSE)

# ---------- [ ]: sampling_range (begin, end) -----------------------------
flag_include_t0 <- FALSE
# flag_include_t0 <- TRUE

date_begin <- ymd("2020-01-22")
date_end <- target_as_of_date

sampling_range <- seq(date_begin, date_end, by= '1 day')
sampling_range <- list(
  "min"= min(sampling_range),
  "max"= max(sampling_range)
)

# this will be ignored, if using rolling_window, but KEEP this to avoid code conflicts ****
first_date_period_one <- ymd("2020-01-30")

# ---------- [ ]: sampling_type -------------------------------------------
sampling_type <- "rolling_window"

# ---------- [ ]: rolling_window_size (optional, or use default) ----------
rolling_window_size <- 2

# ---------- [ ]: fixed_period_size (optional, or use default) ------------
fixed_period_size <- 7 # eg. 7 days

# ---------- [ ]: selected_regions (optional) -----------------------------
#   automatic skip the region that did not have 2018 population data;
#   will not affect the map displays except the fill/ color.

# *** Remarks: The current set is in 1:176 range, 
#   which excluded some latest regions (which might small), since our analysis began earlier ***

selected_regions <- 1:176 # eg. for World, the final length will be 164

node_type <- "dynamic"

# *** Remarks: flag_compute_order ***
flag_compute_order <- FALSE # first compute Yit, then filter by date

# ---------- []: weight_type (optional) ----------------------------------
weight_type <- "ALL"

# ---------- []: basic_setup -------------------------------------------
basic_setup <- list(
  "actual_date_begin"= actual_date_begin,
  
  "proj_name"= proj_name, 
  "target_data"= target_data_files, 
  
  "egde_threshold"= egde_threshold,
  
  "flag_include_t0"= flag_include_t0, 
  "date_begin"= date_begin, 
  "date_end"= date_end, 
  "sampling_range"= sampling_range,
  "first_date_period_one"= first_date_period_one,
  
  "flag_compute_order"= flag_compute_order,
  
  "sampling_type"= sampling_type, 
  "rolling_window_size"= rolling_window_size, 
  "fixed_period_size"= fixed_period_size,
  
  "selected_regions"= selected_regions,
  "node_type"= node_type,
  "weight_type"= weight_type
)


# ====================================================================
# ***** Custom functions -----

'%ni%' <- Negate('%in%')

# =========================================================
# cust_func: load_historical_data() ----------
load_historical_data <- function(tmp_setup) {
  
  proj_name <- tmp_setup$proj_name
  
  # *** Remarks: folder "raw_data" is the main storage location
  #   for those dataset we used ***
  
  # Load region_meta data ----------
  region_meta <- readr::read_csv(here::here(proj_name, "raw_data", tmp_setup$target_data[2])) %>%
    dplyr::mutate(regionID= as.integer(regionID)) %>%
    rename_all(recode, pop_m= "pop") # rename into pop if pop_m existed 
  
  # *** Remarks: excluding those regions that without 2018 population ***
  region_meta <- region_meta %>%
    dplyr::filter(!is.na(pop))
  
  if (length(tmp_setup$selected_regions) > 1) {
    region_meta <- region_meta %>%
      dplyr::filter(regionID %in% tmp_setup$selected_regions) 
  } else if (tmp_setup$selected_regions=="ALL") {} else {
    region_meta <- region_meta %>%
      dplyr::filter(regionID %in% tmp_setup$selected_regions) 
  }
  
  # -------------------------------------------------------- 
  
  # Load historical data ---------- 
  df_newCase <- readr::read_csv(here::here(proj_name, "raw_data", tmp_setup$target_data[1]), na= c("", "NA"))
  
  # -------------------------------
  
  df_newCase <- df_newCase %>%
    dplyr::mutate(
      date= dmy(date),
      t= seq(from= 0, NROW(df_newCase)-1))
  
  # -------------------------------
  
  tmp_num <- as.integer(
    tmp_setup$first_date_period_one - tmp_setup$actual_date_begin) + 1
  
  df_date <- df_newCase %>%
    dplyr::select(t, date) %>%
    dplyr::mutate(
      period_no= findInterval(
        t, c(seq(tmp_num, max(t), by= tmp_setup$fixed_period_size))))
  
  # -------------------------------
  
  if (tmp_setup$flag_include_t0 == TRUE) {
    df_date <- df_date %>% 
      dplyr::filter(
        is.na(date) |
          (date >= tmp_setup$sampling_range$min & date <= tmp_setup$sampling_range$max)
      )
    
    if (min(df_date$date, na.rm= TRUE)!=tmp_setup$actual_date_begin) {
      df_date <- df_date %>% 
        dplyr::filter(!is.na(date))
    }
  } else {
    df_date <- df_date %>% 
      dplyr::filter(!is.na(date)) %>%
      dplyr::filter(date >= tmp_setup$sampling_range$min & date <= tmp_setup$sampling_range$max)
  }
  
  df_newCase <- df_newCase %>%
    dplyr::filter(date %in% df_date$date)
  
  # -------------------------------
  
  df_newCase <- df_newCase %>%
    
    tidyr::pivot_longer(
      -c(t, date), names_to= "region_EN", values_to= "Xit") %>%
    
    # Treat NA as zero ----------
  dplyr::mutate(Xit= ifelse(is.na(Xit), 0, Xit))
  
  # -------------------------------
  
  df_newCase <- dplyr::left_join(
    df_newCase, region_meta, 
    by= c("region_EN"="region_EN"))
  
  return(list(
    "data"= df_newCase,
    "meta"= region_meta,
    "date"= df_date
  ))
}

# =========================================================
# cust_func: calc_Corr() ----------
calc_Corr <- function(tmp_setup, df_date, df_newCase, region_meta) {
  
  if (tmp_setup$sampling_type=="rolling") {
    tmp_rolling_width <- tmp_setup$rolling_window_size
    
    v_period <- unique(df_date$period_no)
    tmp_base_set <- v_period[seq(1, 1+tmp_rolling_width-1)]
    period_set <- lapply(
      seq(0, (length(v_period)-tmp_rolling_width + 1) - 1), 
      function(rolling_idx) {
        tmp_set <- tmp_base_set + 1* rolling_idx
        return(tmp_set)
      })
  } else {
    period_set <- seq(min(df_date$period_no), max(df_date$period_no), by= 1)
  }
  
  # cat("period_set: \n")
  # print(period_set)
  
  # Calculate Correlation Corr(Yit, Yjt)
  lapply(period_set, function(tmp_idx) {
    
    # cat(rep("-", 10), tmp_idx, rep("-", 10), "\n")
    
    if (all(tmp_idx %in% unique(df_date$period_no))) {} else {
      cat("Skipped ...\n")
      next()
    }
    
    # Data Transformation ====================
    
    # *** Remarks: first compute the Yit, then filter by period_set ***
    
    tmp_df <- df_newCase %>%
      dplyr::group_by(regionID) %>%
      dplyr::mutate(Xit_lag= dplyr::lag(Xit, n= 1L)) %>%
      dplyr::mutate(Yit= sqrt(Xit) - sqrt(Xit_lag)) %>%
      dplyr::filter(regionID %in% region_meta$regionID)
    
    # ********************
    if (sampling_type=="rolling_window") {
      selected_t <- dplyr::filter(df_date, period_no %in% tmp_idx)$t
    } else {}
    # ********************
    
    tmp_df <- tmp_df %>%
      dplyr::filter(t %in% selected_t) %>%
      dplyr::group_by(regionID)
    
    
    
    orderedRegion <- tmp_df %>%
      dplyr::summarise(total= sum(Xit, na.rm= TRUE)) %>%
      dplyr::arrange(desc(total))
    
    # Calculation - Correlation ------------------
    # *** Remarks: will not include t=0 into the calculation of correlation ***
    df_Yit <- dplyr::filter(tmp_df, t!=0) %>%
      dplyr::select(t, regionID, Yit) %>%
      dplyr::filter(!is.na(Yit))
    
    # *** Remarks: Special treatment, which excluding those just have zero ***
    exceptID <- df_Yit %>%
      dplyr::group_by(regionID) %>%
      dplyr::summarise(tmp_sd= sd(Yit, na.rm= TRUE)) %>%
      dplyr::filter(tmp_sd==0) %>%
      .$regionID
    
    df_Yit <- df_Yit %>%
      dplyr::filter(regionID %ni% exceptID)
    
    orderedRegion <- orderedRegion %>%
      dplyr::filter(regionID %in% unique(df_Yit$regionID)) %>% 
      .$regionID %>% as.character()
    
    v_t <- df_Yit$t %>% unique()
    # ******************
    
    df_Yit <- df_Yit %>%
      tidyr::pivot_wider(names_from= regionID, values_from= Yit) 
    
    
    df_Yit <- tibble::column_to_rownames(df_Yit, var= "t")
    m_Yit <- as.matrix(df_Yit)
    
    # --------------------------------------------
    
    # Rearrange the regions by descending order ----------
    m_Yit <- m_Yit[, orderedRegion]
    
    # Calculation(s) ----------
    m_cor <- cor(m_Yit)
    
    # *** Remarks: use all selected t or the date t that Yit is not NA ***
    
    return(list(
      "selected_t"= selected_t,
      "n"= length(v_t),
      "corr"= m_cor
    ))
  }) -> l_result # Single period - Corr(Yit, Yjt)
  
  return(l_result)
}

# =========================================================
# cust_func: calc_riskScore() ----------
calc_riskScore <- function(
  tmp_setup, l_result, region_meta, weight_type= "equal",
  tmp_threshold, threshold_option= c("r", "alpha"), need_standardize= FALSE,
  selected_regions= "ALL") {
  
  cat(rep("-", 10), "Calculate risk score ", weight_type, " | ", need_standardize, " | ",
      threshold_option, "= ", tmp_threshold, 
      rep("-", 10), "\n")
  
  # weight_type <- "non_infected_population"
  
  lapply(seq(length(l_result)), function(i) {
    
    # cat(rep("-", 10), i, rep("-", 10), "\n")
    # ------------------------------------
    
    selected_t <- l_result[[i]]$selected_t
    
    tmp_corr <- l_result[[i]]$corr
    
    # **************************************
    # Use for capturing the ordered region names ----------
    tmp_cols <- colnames(tmp_corr)
    # -----------------------------------------------------
    
    # *** Remarks: this is for fixed node size ***
    all_names <- as.character(region_meta$regionID)
    N <- length(all_names) # number of nodes
    # **************************************
    
    # cat("selected_t: ", selected_t, "\n")
    # cat("N: ", N, "\n")
    
    # ******************************************
    # A <- tmp_corr
    A <- matrix(rep(0, N*N), nrow= N)
    # dimnames(A) <- list(tmp_cols, tmp_cols)
    dimnames(A) <- list(all_names, all_names)
    
    if (threshold_option %in% c("r", "w", "wp")) {
      A[tmp_cols, tmp_cols] <- tmp_corr
    } else {}
    
    full_m <- A
    # ******************************************
    
    if (threshold_option=="r") {
      # *** Remarks: modified to greater than ONLY ***
      A[which(full_m > tmp_threshold)] <- 1 
      A[which(full_m <= tmp_threshold)] <- 0
      diag(A) <- 0 
      
    } else if (threshold_option=="wp") {
      
      # ************************************ 
      # Special: using positive correlation only; other treat as zero
      
      A[which(full_m < 0)] <- 0
      diag(A) <- 0 
      
    } else {
      diag(A) <- 0 
      
      # stop("Invalid threshold_option!")
    }
    
    A_dash <- matrix(rep(1, N*N), nrow= N)
    diag(A_dash) <- 0
    
    # Calculate weight(s) ----------
    
    # Rolling period subtotal ----------
    tmp_subtotal <- df_newCase %>%
      dplyr::group_by(regionID, region_EN) %>%
      dplyr::filter(t %in% selected_t) %>%
      dplyr::filter(regionID %in% region_meta$regionID) %>%
      dplyr::summarise(subtotal= sum(Xit, na.rm= TRUE)) %>%
      dplyr::arrange(regionID)
    
    tmp_non_infected_population <- df_allCase %>%
      dplyr::group_by(regionID, region_EN) %>%
      dplyr::filter(t %in% selected_t) %>%
      dplyr::filter(regionID %in% region_meta$regionID) %>%
      dplyr::select(c(t, date, regionID, region_EN, pop, cs_Xit)) %>%
      dplyr::mutate(non_infected_population= pop-cs_Xit) %>%
      dplyr::arrange(regionID, date) %>%
      dplyr::filter(date==max(date)) %>%
      dplyr::arrange(regionID)
    
    tmp_active_population <- df_allCase %>%
      dplyr::group_by(regionID, region_EN) %>%
      dplyr::filter(t %in% selected_t) %>%
      dplyr::filter(regionID %in% region_meta$regionID) %>%
      dplyr::select(c(t, date, regionID, region_EN, Ait)) %>%
      dplyr::arrange(regionID, date) %>%
      dplyr::filter(date==max(date)) %>%
      dplyr::arrange(regionID)
    
    # ------------------------------------
    
    v_equal <- 1/N
    
    v_subtotal <- tmp_subtotal$subtotal
    v_active_population <- tmp_active_population$Ait # Ait
    
    v_population <- region_meta$pop
    v_non_infected_population <- tmp_non_infected_population$non_infected_population
    
    # ------------------------------------
    
    if (weight_type=="mixed") {
      m_W <- m_V <- matrix(v_non_infected_population, nrow= N, byrow= FALSE)
      
    } else if (weight_type=="mixed_active") {
      m_W <- matrix(v_non_infected_population, nrow= N, byrow= FALSE)
      m_V <- matrix(v_active_population, nrow= N, byrow= FALSE)
    }
    
    tmp_S <- t(m_W) %*% A %*% m_V
    
    # -------------------------------------------------
    # -------------------------------------------------
    
    if (weight_type=="mixed_active") {
      
      # *** Calculation - Contribution  *** ----------
      
      if (selected_regions=="ALL") {
        
        v_active_population <- tmp_active_population$Ait # Ait
        v_non_infected_population <- tmp_non_infected_population$non_infected_population
        
        v_regionID <- region_meta$regionID %>% as.character()
        
        tmp_A <- A
        
      } else {
        
        v_active_population <- tmp_active_population %>%
          dplyr::filter(region_EN %in% selected_regions) %>% .$Ait
        
        v_non_infected_population <- tmp_non_infected_population %>%
          dplyr::filter(region_EN %in% selected_regions) %>% .$non_infected_population
        
        tmp_region <- region_meta %>%
          dplyr::filter(region_EN %in% selected_regions)
        
        v_regionID <- tmp_region$regionID %>% as.character()
        
        tmp_A <- A[v_regionID, v_regionID]
        
      }
      
      k <- v_regionID %>% length()
      
      tmp_m_W <- matrix(v_non_infected_population, nrow= k, byrow= FALSE)
      tmp_m_V <- matrix(v_active_population, nrow= k, byrow= FALSE)
      
      sapply(seq(length(v_active_population)), function(l) {
        val <- 0
        
        for (j in seq(length(v_non_infected_population))) {
          val <- val + ( v_non_infected_population[j] * tmp_A[j,l] )
        }
        
        val <- v_active_population[l] * val
      }) -> v_contribution
      
      # *** Remarks: for contribution calcualtion (by k countries) ***
      
      k_S <- t(tmp_m_W) %*% tmp_A %*% tmp_m_V
      
      if (need_standardize==TRUE) {
        v_contribution <- v_contribution / c(k_S)
      } else {}
      
      
      df_rs_contribution <- data.frame(
        regionID= as.integer(v_regionID), 
        rs= v_contribution, 
        stringsAsFactors= FALSE)
      
      df_rs_contribution$period_no <- i-1
      df_rs_contribution$N <- k
      df_rs_contribution$threshold_val <- tmp_threshold
      df_rs_contribution$threshold_type <- threshold_option
      df_rs_contribution$need_standardize <- need_standardize
      
    } else if (weight_type=="mixed") {
      # *** Calculation - Contribution  *** ----------
      
      if (selected_regions=="ALL") {
        
        # v_subt <- tmp_subtotal$subtotal
        # v_pop <- region_meta$pop
        
        v_non_infected_population <- tmp_non_infected_population %>% .$non_infected_population
        
        v_pop <- v_subt <- v_non_infected_population
        
        v_regionID <- region_meta$regionID %>% as.character()
        
        tmp_A <- A
        
      } else {
        
        # v_subt <- tmp_subtotal %>%
        #   dplyr::filter(region_EN %in% selected_regions) %>% .$subtotal
        # v_pop <- tmp_region %>% .$pop
        
        v_non_infected_population <- tmp_non_infected_population %>%
          dplyr::filter(region_EN %in% selected_regions) %>% .$non_infected_population
        
        tmp_region <- region_meta %>%
          dplyr::filter(region_EN %in% selected_regions)
        
        v_pop <- v_subt <- v_non_infected_population
        
        v_regionID <- tmp_region$regionID %>% as.character()
        
        tmp_A <- A[v_regionID, v_regionID]
        
      }
      
      k <- v_regionID %>% length()
      
      tmp_m_W <- matrix(v_pop, nrow= k, byrow= FALSE)
      tmp_m_V <- matrix(v_subt, nrow= k, byrow= FALSE)
      
      sapply(seq(length(v_subt)), function(l) {
        val <- 0
        
        for (j in seq(length(v_pop))) {
          val <- val + ( v_pop[j] * tmp_A[j,l] )
        }
        
        val <- v_subt[l] * val
      }) -> v_contribution
      
      # *** Remarks: for contribution calcualtion (by k countries) ***
      
      k_S <- t(tmp_m_W) %*% tmp_A %*% tmp_m_V
      
      if (need_standardize==TRUE) {
        v_contribution <- v_contribution / c(k_S)
      } else {}
      
      
      df_rs_contribution <- data.frame(
        regionID= as.integer(v_regionID), 
        rs= v_contribution, 
        stringsAsFactors= FALSE)
      
      df_rs_contribution$period_no <- i-1
      df_rs_contribution$N <- k
      df_rs_contribution$threshold_val <- tmp_threshold
      df_rs_contribution$threshold_type <- threshold_option
      df_rs_contribution$need_standardize <- need_standardize
      
    } else {
      
      df_rs_contribution <- NULL
      
    }
    
    # ***************************************************
    if (weight_type %ni% c("mixed", "mixed_active")) {
      
      # Standardization by A_dash (with popultation as weight vector) ----------
      if (need_standardize==TRUE) {
        tmp_W <- m_W
        
        tmp_S <- tmp_S / (t(tmp_W) %*% A_dash %*% tmp_W)
      }
      
    } else {
      
      if (need_standardize==TRUE) {
        tmp_W <- matrix(v_population, nrow= N, byrow= FALSE)
        
        tmp_S <- tmp_S / (t(tmp_W) %*% A_dash %*% tmp_W)
      }
      
    }
    # ***************************************************
    
    
    # -------------------------------------------------
    
    df_risk_score <- data.frame(period_no= i-1, N= N, s= tmp_S, stringsAsFactors= FALSE)
    contribution <- list("m_W"= m_W, "m_V"= m_V, "A"= A, "A_dash"= A_dash, 
                         contrb= df_rs_contribution)
    
    return(list(
      "risk_score"= df_risk_score,
      "contribution"= contribution
    ))
  }) -> l_risk_score
  
  l_tmp <- l_risk_score %>%
    lapply(., function(l) {
      return(l$risk_score)
    })
  
  all_risk_score <- do.call("rbind", l_tmp)
  all_risk_score$threshold_val <- tmp_threshold
  all_risk_score$threshold_type <- threshold_option
  all_risk_score$need_standardize <- need_standardize
  
  l_tmp2 <- l_risk_score %>%
    lapply(., function(l) {
      return(l$contribution)
    })
  
  return(list(
    "df"= all_risk_score,
    "weight_type"= weight_type,
    "contribution"= l_tmp2
  ))
  
}


# =========================================================
# cust_func: create_graph() ----------
create_graph <- function(tmp_setup, l_result, region_meta,
                         tmp_threshold, threshold_option) {
  
  # cat(rep("-", 10), "Calculate Ci", rep("-", 10), "\n")
  lapply(seq(length(l_result)), function(i) {
    
    # cat(rep("-", 10), i, rep("-", 10), "\n")
    
    tmp_corr <- l_result[[i]]$corr
    
    # -------------------------------------------------
    
    tmp_res <- tmp_corr %>% as.data.frame()
    
    tmp_res$Var1 <- rownames(tmp_res)
    tmp_res <- tmp_res %>% tidyr::gather(-Var1, key= "Var2", value= "corr")
    
    tmp_res$Var1 <- sprintf("R%s", tmp_res$Var1)
    tmp_res$Var2 <- sprintf("R%s", tmp_res$Var2)
    
    res.melted_corr <- tmp_res %>% 
      dplyr::filter(Var1 != Var2)
    
    res.melted_corr <- res.melted_corr %>%
      rename(from= Var1, to= Var2)
    
    # -------------------------------------------------
    
    res.melted <- res.melted_corr
    
    # ***************************************************************************
    if (tmp_setup$node_type=="dynamic") {
      rstat_nodes <- data.frame(name= unique(c(res.melted$from, res.melted$to)))  
    } else {
      rstat_nodes <- data.frame(name= sprintf("R%s", region_meta$regionID))
    }
    # ***************************************************************************
    
    if (threshold_option=="r") {
      tmp_edges <- res.melted # for kept the correlation info
    } else {
      tmp_edges <- res.melted # for kept the correlation info
    }
    
    tmp_edges$value <- tmp_edges$corr
    
    tmp_graph <- tidygraph::tbl_graph(nodes= rstat_nodes, edges= tmp_edges, directed= FALSE)
    tmp_graph <- simplify(tmp_graph, remove.multiple= TRUE, remove.loops= TRUE)
    
    return(list(
      "period_no"= i-1,
      "graph"= tmp_graph
    ))
  }) -> l_network
  
  # -----------------------------------------------------
  
  return(list(
    "network"= l_network,
    "threshold_val"= tmp_threshold,
    "threshold_type"= threshold_option,
    "l_all_corr"= l_result
  ))
}


# =========================================================
# cust_func: create_rolling_sets() ----------
create_rolling_sets <- function(df_date, tmp_setup, begin_t= 0) {
  
  tmp_t <- min(df_date$t)
  
  if (begin_t > tmp_t) {
    tmp_t <- begin_t
  }
  
  w_size <- tmp_setup$fixed_period_size * tmp_setup$rolling_window_size
  tmp_ref_begin <- tmp_t + w_size - 1
  
  shift_by_size <- 1
  range_ref_t <- seq(tmp_ref_begin, max(df_date$t), by= shift_by_size)
  
  lapply(seq(length(range_ref_t)), function(idx) {
    
    ref_t <- range_ref_t[idx]
    
    tmp <- df_date %>%
      dplyr::filter(t==ref_t)
    
    begin_date <- tmp$date - w_size + 1
    end_date <- tmp$date
    tmp_interval <- seq(begin_date, end_date, by= 1)
    
    tmp_df <- data.frame(date= tmp_interval, stringsAsFactors= FALSE)
    tmp_df$period_no <- idx - 1
    
    return(tmp_df)
  }) -> l_period_set
  
  df_period_set <- do.call("rbind", l_period_set)
  
  return(df_period_set)
}


# =========================================================
# cust_func: create_color_palette() ----------
create_color_palette <- function(l_rgb, tmp_max= 255) {
  v_color_hex <- sapply(l_rgb, function(tmp_rgb) {
    rgb(tmp_rgb[1], tmp_rgb[2], tmp_rgb[3], max= tmp_max)
  })
  
  return(v_color_hex)
}

# ====================================================================
# ***** Actual program -----


# ***** Input: Confirmed cases from WHO -----
obj_data <- load_historical_data(basic_setup)

if (basic_setup$sampling_type=="rolling_window") {
  
  # Create rolling sets ----------
  df_date <- left_join(
    create_rolling_sets(obj_data$date, basic_setup, begin_t= 0),
    dplyr::select(obj_data$date, c(t, date)), by= c("date"="date"))
  
} else {
  df_date <- obj_data$date
}

region_meta <- obj_data$meta
df_newCase <- obj_data$data


## --------------------------------------------------

# ***** Input: Recovered cases from JHU -----
df_recoveredCase <- read_csv(
  here::here("raw_data", sprintf("upto_%s_estimated_Rit_cutoff%s.csv", date_str, cutoff_I))) %>%
  tidyr::pivot_longer(-date, names_to= "region_EN", values_to= "Rit") %>%
  dplyr::group_by(region_EN)
# We might directly use their stated date *****

if (class(df_recoveredCase$date)!="Date") {
  df_recoveredCase <- df_recoveredCase %>%
    dplyr::mutate(date= dmy(date))
}

# ***** Input: Deaths from WHO -----
df_deathCase <- read_csv(
  here::here(proj_name, "raw_data", sprintf("upto_%s_FINAL_deaths_World.csv", date_str))) %>%
  tidyr::pivot_longer(-date, names_to= "region_EN", values_to= "Dit") %>%
  dplyr::group_by(region_EN)

if (class(df_deathCase$date)!="Date") {
  df_deathCase <- df_deathCase %>%
    dplyr::mutate(date= dmy(date))
}

# --------------------------------------------------
# **************************************************

df_allCase <- left_join(
  df_newCase, df_deathCase, by= c("date", "region_EN"))

df_allCase <- left_join(
  df_allCase, df_recoveredCase, by= c("date", "region_EN"))

# ----------------------------

df_allCase <- df_allCase %>%
  dplyr::filter(!is.na(region_EN))

df_allCase <- df_allCase %>%
  dplyr::group_by(region_EN) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    Dit= ifelse(is.na(Dit), 0, Dit),
    Rit= ifelse(is.na(Rit), 0, Rit)
  ) %>%
  dplyr::mutate(
    cs_Xit= cumsum(Xit),
    cs_Rit= cumsum(Rit), 
    cs_Dit= cumsum(Dit)
  ) %>%
  dplyr::mutate(Ait= cs_Xit-cs_Rit-cs_Dit)

# ***************************
# Fix the negative Ait ------
df_allCase <- df_allCase %>%
  dplyr::mutate(Ait= ifelse(Ait < 0, 0, Ait))
# ***************************


# ====================================================================
# ***** Calculate Correlation matrix -----

l_result <- calc_Corr(basic_setup, df_date, df_newCase, region_meta)

# ====================================================================
# ***** Construct dynamic networks -----

l_obj_graph <- lapply(seq(NROW(basic_setup$egde_threshold)), function(idx) {
  tmp_edge_threshold <- basic_setup$egde_threshold[idx,]
  
  obj_graph <- create_graph(
    basic_setup, l_result, region_meta, 
    tmp_threshold= tmp_edge_threshold$threshold_val, 
    threshold_option= tmp_edge_threshold$threshold_type)
  
  
  return(obj_graph)
})


N <- length(region_meta$regionID)

# ====================================================================
# ***** Calculate Risk scores and contributions -----

selected_region_EN <- "ALL"

if (basic_setup$weight_type=="NONE") {
  l_risk_score <- list()
  
} else if (basic_setup$weight_type=="ALL") {
  l_risk_score <- lapply(
    c("mixed_active", "mixed_active.standardize",  # by SRS: [mixed_active]
      "mixed", "mixed.standardize"),               # by PRS: [mixed]
    function(type) {
      
      if (grepl("(.standardize)", type, ignore.case= TRUE)) {
        need_standardize <- TRUE
      } else {
        need_standardize <- FALSE
      }
      
      type <- gsub("(.standardize)", "", type)
      
      l_rs <- lapply(seq(NROW(basic_setup$egde_threshold)), function(idx) {
        tmp_edge_threshold <- basic_setup$egde_threshold[idx,]
        
        tmp_risk_score <- calc_riskScore(
          basic_setup, l_result, region_meta, type,
          tmp_threshold= tmp_edge_threshold$threshold_val, 
          threshold_option= tmp_edge_threshold$threshold_type,
          need_standardize= need_standardize,
          selected_regions= selected_region_EN)
        
        return(tmp_risk_score)
      })
      
      obj_risk_score <- list(
        "df"= do.call(
          "rbind", lapply(l_rs, function(rs) { return(rs$df) })),
        "weight_type"= type,
        "contribution"= lapply(l_rs, function(rs) { return(rs$contribution) })
      )
      
      return(obj_risk_score)
    })
} else {
  l_risk_score <- lapply(basic_setup$weight_type, function(type) {
    
    l_rs <- lapply(seq(NROW(basic_setup$egde_threshold)), function(idx) {
      tmp_edge_threshold <- basic_setup$egde_threshold[idx,]
      
      tmp_risk_score <- calc_riskScore(
        basic_setup, l_result, region_meta, type,
        tmp_threshold= tmp_edge_threshold$threshold_val, 
        threshold_option= tmp_edge_threshold$threshold_type)
      
      return(tmp_risk_score)
    })
    
    obj_risk_score <- list(
      "df"= do.call(
        "rbind", lapply(l_rs, function(rs) { return(rs$df) })),
      "weight_type"= type)
    
  })
}

# ====================================================================  
# ====================================================================
# ***** Extract the results into 3 data.frame (tabular form) -----
# Network statistics, Risk scores, Risk Contribution


# ====================================================================
# ***** Extract the required risk contribution (fit, git) -----


# --------------------------------------------------------------------
# Input: WHO Region meta (iso 3166 alpha-2, alpha-3, Region)

df_WHO_region <- read_csv(here::here(proj_name, "raw_data", "WHO_Regions.csv"))

tmp_region_meta <- df_WHO_region %>%
  dplyr::select(country_EN, WHO_Region_CODE, WHO_region_name) %>%
  left_join(
    region_meta, ., by= c("region_EN"="country_EN"))

tmp_region_meta <- tmp_region_meta %>%
  dplyr::mutate(
    WHO_region_name= ifelse(
      WHO_region_name %in% c("Western Pacific", "South-East Asia"),
      "Asia", WHO_region_name))

# --------------------------------------------

l_ts_risk_contrb_sp <- list()

if (length(l_risk_score) > 0) {
  
  for (i in seq(length(l_risk_score))) {
    obj_risk_score <- l_risk_score[[i]]
    
    if (obj_risk_score$weight_type %ni% c("mixed", "mixed_active")) {
      next()
    }
    
    if (unique(obj_risk_score$df$need_standardize)!=TRUE) { next() }
    
    tmp_contribution <- obj_risk_score$contribution
    tmp_risk_score <- obj_risk_score$df
    
    lapply(seq(length(tmp_contribution)), function(th) {
      tmp_result <- tmp_contribution[[th]]
      
      lapply(seq(length(tmp_result)), function(p_idx) {
        df_contribution <- tmp_result[[p_idx]]$contrb
        
        return(df_contribution)
      }) -> l_df
      
      contrb_df <- do.call("rbind", l_df)
      
      return(contrb_df)
    }) -> l_final_df
    
    tmp_contrb <- do.call("rbind", l_final_df)
    
    tmp_contrb <- left_join(tmp_contrb, tmp_risk_score, 
                            by= c("period_no", "N", 
                                  "threshold_val", "threshold_type", 
                                  "need_standardize"))
    
    tmp_contrb <- tmp_contrb %>%
      dplyr::mutate(final_contrb= rs)
    
    tmp_contrb <- tmp_region_meta %>% 
      dplyr::select(regionID, region_EN, WHO_Region_CODE, WHO_region_name) %>%
      left_join(tmp_contrb, ., by= c("regionID"))
    
    tmp_contrb <- df_date %>%
      dplyr::group_by(period_no) %>%
      dplyr::filter(row_number()==n()) %>%
      dplyr::select(date, period_no) %>%
      left_join(tmp_contrb, ., by= c("period_no"= "period_no"))
    
    tmp_contrb$weight_type <- obj_risk_score$weight_type
    l_ts_risk_contrb_sp <- append(l_ts_risk_contrb_sp, list(tmp_contrb))
    
  }    
}

df_ts_risk_contrb_sp <- do.call("rbind", l_ts_risk_contrb_sp)

df_ts_risk_contrb_sp$risk_type <- "risk_contribution"

# df_ts_risk_contrb_sp %>% colnames()
# df_ts_risk_contrb_sp %>% head()

tmp_df_sp <- df_ts_risk_contrb_sp %>%
  dplyr::select(date, regionID, region_EN, WHO_region_name, weight_type, final_contrb)

# head(tmp_df_sp)

tmp_df_sp_wider <- tmp_df_sp %>% 
  tidyr::pivot_wider(names_from= "weight_type", values_from= "final_contrb") %>%
  dplyr::rename(fit= "mixed", git= "mixed_active")


# ====================================================================
# ***** Extract the required risk scores (PRS, SRS) -----

l_ts_St <- list()

if (length(l_risk_score) > 0) {
  
  for (i in seq(length(l_risk_score))) {
    obj_risk_score <- l_risk_score[[i]]
    
    tmp_risk_score <- obj_risk_score$df
    
    if(obj_risk_score$weight_type %ni% c("mixed", "mixed_active")) { next() }
    if(unique(tmp_risk_score$need_standardize)!=FALSE) { next() }
    
    tmp_risk_score <- df_date %>%
      dplyr::group_by(period_no) %>%
      dplyr::filter(row_number()==n()) %>%
      dplyr::select(date, period_no) %>%
      left_join(tmp_risk_score, ., by= c("period_no"= "period_no"))
    
    tmp_risk_score$weight_type <- obj_risk_score$weight_type
    
    l_ts_St <- append(l_ts_St, list(tmp_risk_score))
    
  }    
}

df_ts_St <- do.call("rbind", l_ts_St)


df_ts_St$risk_type <- "risk_score"

# df_ts_St %>% colnames()
# df_ts_St %>% head()

# Standardize the risk score ----------
A <- matrix(1, nrow= N, ncol= N, byrow= FALSE)
diag(A) <- 0

m_P <- matrix(region_meta$pop, nrow= N, byrow= FALSE)

tmp_denominator <- as.numeric(t(m_P) %*% A %*% m_P)
tmp_denominator_s2t <- as.numeric(t(m_P) %*% A %*% m_P) * 0.001

df_ts_St <- df_ts_St %>%
  dplyr::mutate(
    standardized_s= ifelse(weight_type=="mixed", s /tmp_denominator, s /tmp_denominator_s2t))


tmp_df_ts_St <- df_ts_St %>%
  dplyr::select(date, weight_type, standardized_s)

# head(tmp_df_ts_St)

tmp_df_ts_St_wider <- tmp_df_ts_St %>% 
  tidyr::pivot_wider(names_from= "weight_type", values_from= "standardized_s") %>%
  dplyr::rename(PRS= "mixed", SRS= "mixed_active")

# =================================================

# Risk score/ contribution -----------

# PART I -----------

df_date$date %>% summary()
l_risk_score %>% sapply(., function(l) { l$weight_type })

l_ts_St <- list()

if (length(l_risk_score) > 0) {
  
  for (i in seq(length(l_risk_score))) {
    obj_risk_score <- l_risk_score[[i]]
    
    tmp_risk_score <- obj_risk_score$df
    
    if(obj_risk_score$weight_type %ni% c("mixed", "mixed_active")) { next() }
    if(unique(tmp_risk_score$need_standardize)!=FALSE) { next() }
    
    print(obj_risk_score$weight_type)
    print(unique(tmp_risk_score$need_standardize))
    
    tmp_risk_score <- df_date %>%
      dplyr::group_by(period_no) %>%
      dplyr::filter(row_number()==n()) %>%
      dplyr::select(date, period_no) %>%
      left_join(tmp_risk_score, ., by= c("period_no"= "period_no"))
    
    tmp_risk_score$weight_type <- obj_risk_score$weight_type
    
    l_ts_St <- append(l_ts_St, list(tmp_risk_score))
    
  }    
}

df_ts_St <- do.call("rbind", l_ts_St)


df_ts_St$risk_type <- "risk_score"

df_ts_St %>% colnames()
df_ts_St %>% head()

# ---------------------------------

# # *********************************
# Standardize the risk score ----------
A <- matrix(1, nrow= N, ncol= N, byrow= FALSE)
diag(A) <- 0

m_P <- matrix(region_meta$pop, nrow= N, byrow= FALSE)

tmp_denominator <- as.numeric(t(m_P) %*% A %*% m_P)
tmp_denominator_s2t <- as.numeric(t(m_P) %*% A %*% m_P) * 0.001


df_ts_St <- df_ts_St %>%
  dplyr::mutate(
    standardized_s= ifelse(weight_type=="mixed", s /tmp_denominator, s /tmp_denominator_s2t))


# ==========================
# ==========================

# PART II -----------

df_WHO_region <- read_csv(here::here(proj_name, "raw_data", "WHO_Regions.csv"))

tmp_region_meta <- df_WHO_region %>%
  dplyr::select(country_EN, WHO_Region_CODE, WHO_region_name) %>%
  left_join(
    region_meta, ., by= c("region_EN"="country_EN"))

tmp_region_meta <- tmp_region_meta %>%
  dplyr::mutate(
    WHO_region_name= ifelse(
      WHO_region_name %in% c("Western Pacific", "South-East Asia"),
      "Asia", WHO_region_name))

v_color <- create_color_palette(list(
  c(255, 18, 0), 
  c(255, 147, 0), 
  c(88, 118, 0), 
  c(0, 223, 98), 
  c(0, 95, 145), 
  c(109, 0, 255), 
  c(255, 0, 188), 
  c(133, 133, 133), 
  c(128, 46, 0), 
  c(152, 0, 67)
))

tmp_fill <- data.frame(
  color= v_color[1:5],
  WHO_region_name= c("Africa", "America", "Asia", "Eastern Mediterranean", "Europe"),
  stringsAsFactors= FALSE)

tmp_region_meta <- left_join(
  tmp_region_meta, tmp_fill, by= "WHO_region_name")


# --------------------------------------------

# =====================================================


l_ts_risk_contrb <- list()

if (length(l_risk_score) > 0) {
  
  for (i in seq(length(l_risk_score))) {
    obj_risk_score <- l_risk_score[[i]]
    
    if (obj_risk_score$weight_type %ni% c("mixed", "mixed_active")) {
      next()
    }
    
    if (unique(obj_risk_score$df$need_standardize)!=TRUE) { next() }
    
    print(obj_risk_score$weight_type)
    print(unique(tmp_risk_score$need_standardize))
    
    
    tmp_contribution <- obj_risk_score$contribution
    tmp_risk_score <- obj_risk_score$df
    
    lapply(seq(length(tmp_contribution)), function(th) {
      tmp_result <- tmp_contribution[[th]]
      
      lapply(seq(length(tmp_result)), function(p_idx) {
        df_contribution <- tmp_result[[p_idx]]$contrb
        
        return(df_contribution)
      }) -> l_df
      
      contrb_df <- do.call("rbind", l_df)
      
      return(contrb_df)
    }) -> l_final_df
    
    tmp_contrb <- do.call("rbind", l_final_df)
    
    tmp_contrb <- left_join(tmp_contrb, tmp_risk_score, 
                            by= c("period_no", "N", 
                                  "threshold_val", "threshold_type", 
                                  "need_standardize"))
    
    tmp_contrb <- tmp_contrb %>%
      dplyr::mutate(final_contrb= rs)
    
    tmp_contrb <- tmp_region_meta %>% 
      dplyr::select(regionID, region_EN, WHO_Region_CODE, WHO_region_name) %>%
      left_join(tmp_contrb, ., by= c("regionID"))
    
    tmp_contrb <- df_date %>%
      dplyr::group_by(period_no) %>%
      dplyr::filter(row_number()==n()) %>%
      dplyr::select(date, period_no) %>%
      left_join(tmp_contrb, ., by= c("period_no"= "period_no"))
    
    # ***************************************
    tmp_contrbX <- tmp_contrb %>%
      dplyr::group_by(date, WHO_region_name, threshold_val, threshold_type, need_standardize) %>%
      dplyr::summarise(final_contrbX= sum(final_contrb, na.rm= TRUE))
    # ***************************************
    
    tmp_contrbX$weight_type <- obj_risk_score$weight_type
    
    l_ts_risk_contrb <- append(l_ts_risk_contrb, list(tmp_contrbX))
    
  }    
}

df_ts_risk_contrb <- do.call("rbind", l_ts_risk_contrb)

df_ts_risk_contrb$risk_type <- "risk_contribution"

df_ts_risk_contrb %>% colnames()
df_ts_risk_contrb %>% head()

# =========================================================================

# [Output] Risk scores ----------
df_ts_St %>%
  dplyr::select(date, standardized_s, threshold_type, threshold_val, weight_type) %>%
  write_csv(., here::here(
    proj_name, "output", 
    sprintf("df_final_riskScore_%s_%s.csv", date_str, tmp_weight,
            gsub("[^0-9]+", "", threshold_val))))

# [Output] risk contribution (by WHO Region, country/ regions) ----------
df_ts_risk_contrb %>%
  write_csv(., here::here(
    proj_name, "output",
    sprintf("df_final_riskContrb_%s_%s.csv", date_str, tmp_weight,
            gsub("[^0-9]+", "", threshold_val))))

df_ts_risk_contrb_sp %>%
  write_csv(., here::here(
    proj_name, "output",
    sprintf("df_final_riskContrb_detail_%s_%s.csv", date_str, tmp_weight,
            gsub("[^0-9]+", "", threshold_val))))


# =========================================================================

