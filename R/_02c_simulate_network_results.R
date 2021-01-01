
# *************************************************************************
# [_02c_simulate_network_results.R]
# 
#   Aim: Simulate the network statistics by network density
# *************************************************************************

# source(here::here("", "R", "_02c_simulate_network_results.R"))

# For parallel processing ----------
# install.packages("foreach")
# install.packages("doSNOW")

# =========================================
# Load Library ----------------------------

library(dplyr)
library(readr)

library(lubridate)

library(ggplot2)

library(ggthemes)
library(cowplot)

library(ggpubr)

library(ggraph)
library(igraph)

library(Rlab)

# library(foreach)
library(doSNOW)

# =========================================
# Basic setup -----------------------------

# # suggestions: 2 or above; please check the number of cores of your CPU
# NUM_CORE <- 2 
# 
# proj_name <- ""
# 
# initial_seed <- seed <- 1010
# set.seed(seed)
# 
# # threshold_val <- 0.4
# threshold_val <- 0.5
# # threshold_val <- 0.6
# 
# # ------------------------
# 
# # cutoff_I <- 0.5
# # cutoff_I <- 0.6
# cutoff_I <- 0.7
# # cutoff_I <- 0.8
# 
# date_str <- "Dec29"
# date_end <- target_as_of_date <- ymd("2020-12-29")

# ------------------------

threshold_option <- "r"

# ------------------------

df_final_network_stat <- read_csv(
  here::here(proj_name, "output",  
             sprintf("df_final_network_stat_%s_%s%s_Rit_hat_%s.csv", 
                     date_str, threshold_option,
                     gsub("[^0-9]+", "", threshold_val), cutoff_I)))

# ==================================================================

# cust_func: perform_simulation() ----------
perform_simulation <- function(
  dist_type= "Bern", prob_success, num_trial, tmp_date, seed) {
  
  time_begin <- Sys.time()
  
  # Create a full simple graph (undirected) ----------
  g <- igraph::make_full_graph(N, directed= FALSE, loops= FALSE)
  df_graph <- igraph::as_data_frame(g)
  
  lapply(seq(num_trial), function(idx) {
    
    tmp_seed <- seed+idx
    
    # Random draw n outcomes from Bernoulli(p) ------------
    set.seed(tmp_seed)
    v_outcome <- Rlab::rbern(n= num_possible_edges, prob= prob_success)
    df_graph$linked <- v_outcome
    
    # Filter the edges by linked ----------
    tmp_graph <- dplyr::filter(df_graph, linked==1)
    
    # Create a new graph by using the selected edges and all vertices ----------
    tmp_g <- igraph::graph_from_data_frame(
      tmp_graph, directed= FALSE, vertices= as.character(V(g)))
    
    # *** Filter duplicated edges, just for being more safe ***
    tmp_g <- simplify(tmp_g, remove.multiple= TRUE, remove.loop= TRUE)
    
    
    # calculate network statistics ----------
    tmp_netStat <- data.frame(trial_no= idx, ref_date= tmp_date,
                              stringsAsFactors= FALSE)
    
    tmp_netStat$num_edge <- gsize(tmp_g)
    tmp_netStat$edge_density <- edge_density(tmp_g, loops= FALSE)
    tmp_netStat$CC_global <- transitivity(tmp_g, type= "global")
    tmp_netStat$assortativity <- assortativity_degree(tmp_g, directed= FALSE)
    
    
    tmp_netStat$ref_seed <- tmp_seed
    
    tmp_result <- list(
      network_stat= tmp_netStat
    )
    
    return(tmp_result)
    
  }) -> l_all_result
  
  print(as.character(tmp_date))
  
  # ================================================================================
  # Combine into one single data.frame ----------
  
  l_all_stat <- lapply(l_all_result, function(l) {
    return(l$network_stat)
  })
  
  df_all_stat <- do.call("rbind", l_all_stat)
  
  save(df_all_stat, file= here::here(
    proj_name, "RData", "tmp_simulation",
    sprintf("l_all_simulation_F_result_%s.RData", as.character(tmp_date))))
  
  time_end <- Sys.time()
  time_diff <- time_end - time_begin
  
  print(sprintf("Compute duration: %s", time_diff))
  
  return(seed+num_trial+1)
  
}

# ====================================================================
# ***** Perform simulation by edge density -----


# Setup for simulation ----------

# 164 countries; align with our focuses ----------
N <- 164
num_possible_edges <- N*(N-1) / 2

# Set number of trial ----------
# num_trial <- 2
# num_trial <- 10
num_trial <- 10000

cat("start simulation: ", as.character(Sys.time()), "\n")

time_begin <- Sys.time()

# --------------------------------------------------------------------
# Actual simulation

v_used_seed <- c()

if (TRUE) {
  
  target_file <- here::here("RData", sprintf("df_all_simulation_%s_r%s.RData", date_end, 
                                             gsub("[^0-9]+", "", threshold_val)))
  
  if (file.exists(target_file)==TRUE) {
    
    # df_all_simulation
    load(target_file)
    
  } else {
    
    # Create tmp_simulation folder for temporary usage ----------
    dir.create(file.path(here::here(proj_name, "RData"), "tmp_simulation"))
    
    tmp_network_stat <- df_final_network_stat
    
    # Run Simulation ----------
    
    v_date <- tmp_network_stat$date
    v_prob <- tmp_network_stat$edge_density
    
    
    # v_set_seeds <- initial_seed + num_trial*seq(0, NROW(df_final_network_stat)-1, by= 1) + 1 
    v_set_seeds <- initial_seed + num_trial*seq(0, NROW(df_final_network_stat)-1, by= 1) + 1
    
    cl <- makeCluster(NUM_CORE)
    registerDoSNOW(cl)
    
    # Simulation loop ----------
    # foreach(idx = seq(length(v_date))) %do% {
    foreach(idx = seq(length(v_date)), .packages= c("dplyr", "Rlab", "igraph")) %dopar% {
      
      tmp_p <- v_prob[idx]
      tmp_date <- v_date[idx]
      tmp_seed <- v_set_seeds[idx]
      
      cat(rep("-", 10), idx, " | ", as.character(tmp_date), rep("-", 10), "\n")
      print(tmp_seed)
      
      v_used_seed <- c(v_used_seed, tmp_seed)
      perform_simulation("Bern", tmp_p, num_trial, tmp_date, tmp_seed)
      
    }
    
    # Sequential processing ----------
    # for (idx in seq(length(v_date))) {
    #   
    #   tmp_p <- v_prob[idx]
    #   tmp_date <- v_date[idx]
    #   tmp_seed <- v_set_seeds[idx]
    #   
    #   cat(rep("-", 10), idx, " | ", as.character(tmp_date), rep("-", 10), "\n")
    #   print(tmp_seed)
    #   
    #   v_used_seed <- c(v_used_seed, tmp_seed)
    #   perform_simulation("Bern", tmp_p, num_trial, tmp_date, tmp_seed)
    #   
    # }
    
    l_all_simulation <- list()
    
    for (idx in seq(length(v_date))) {
      # cat(rep("-", 10), as.character(v_date[idx]), rep("-", 10), "\n")
      load(here::here(proj_name, "RData", "tmp_simulation",
                      sprintf("l_all_simulation_F_result_%s.RData", as.character(v_date[idx]))))
      
      l_all_simulation <- append(l_all_simulation, list(df_all_stat))
    }
    
    df_all_simulation <- do.call("rbind", l_all_simulation)
    
    save(df_all_simulation,
         file= here::here("RData", sprintf("df_all_simulation_%s_r%s.RData", date_end, 
                                           gsub("[^0-9]+", "", threshold_val))))
    
  }
  
  # Calculate min, max, mean, sd ----------
  df_summary <- df_all_simulation %>%
    dplyr::select(-c(ref_date)) %>%
    tidyr::pivot_longer(-trial_no, names_to= "stat", values_to= "value") %>%
    dplyr::group_by(stat) %>%
    dplyr::summarise(
      min= min(value),
      max= max(value),
      mean= mean(value),
      sd= sd(value))
  
  # Retrieve mean_val ONLY ----------
  tmp_summary <- df_all_simulation %>%
    tidyr::pivot_longer(-ref_date, names_to= "stat", values_to= "value") %>%
    dplyr::group_by(ref_date, stat) %>%
    dplyr::summarise(mean_val= mean(value, na.rm= TRUE))
  
  simulated_netStat <- tmp_summary
  
} else {
  simulated_netStat <- NULL
}

cat("end simulation: ", as.character(Sys.time()), "\n")

time_end <- Sys.time()
time_diff <- time_end - time_begin

print(time_diff)

# ====================================================================================
# ====================================================================================
# Visualization: Simulation -------------------------------------------------

if (TRUE) {
  
  tmp_simulated_netStat <- simulated_netStat %>%
    dplyr::filter(stat %in% c("num_edge", "edge_density", "CC_global", "assortativity")) %>%
    dplyr::rename(value= mean_val) %>%
    dplyr::mutate(type= "simulated") %>%
    ungroup()
  
  tmp_actual_netStat <- df_final_network_stat %>%
    dplyr::filter(threshold_val %in% c(threshold_val)) %>%
    dplyr::select(c("date", "num_edge", "edge_density", "CC_global", "assortativity")) %>%
    dplyr::rename(ref_date= date) %>%
    tidyr::pivot_longer(-ref_date, names_to= "stat", values_to= "value") %>%
    dplyr::mutate(type= "actual") %>%
    ungroup()
  
  # ------------------------------
  
  # Combine two network statistics results ----------
  tmp_all_netStat <- do.call("rbind", list(
    tmp_actual_netStat, tmp_simulated_netStat))
  
  tmp_all_netStat$stat <- factor(
    tmp_all_netStat$stat,
    levels= c("num_edge", "edge_density", "CC_global", "assortativity"))
  
  tmp_all_netStat <- tmp_all_netStat %>%
    dplyr::mutate(value= ifelse(type=="simulated" & stat=="num_edge", NA, value))
  
  
  # [Output] Simulated and Actual network statistics ----------
  tmp_all_netStat %>%
    write_csv(., here::here(
      proj_name, "output",
      sprintf("df_final_simulation_%s_%s%s_by_binary_Rit_hat_%s.csv", date_str, threshold_option,
              gsub("[^0-9]+", "", threshold_val), cutoff_I)))
  
}

# =============================================================================


