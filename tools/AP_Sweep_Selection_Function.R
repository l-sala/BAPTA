# ===================================================================
# Title: AP Sweep Selection 
#
# Purpose: This script allows the selection of contiguous AP at the steady state, based on Ediast and APD90 beat-to-beat variation.
# Author: Luca Sala, PhD
# Date: 2018-09-13
# Version: 0.95
# Revisions: 2018-09-21 - v0.8
#            2019-06-XX - v0.9
#            2020-04-29 - v0.91
#            2021-01-06 - v0.95
#
# ===================================================================

sweep_selection_function <- function(APDs,
                                     sweeps){

extended_neg_Ediast <- vector()
filtered_by_Ediast <- vector()
filtered_by_Ediast_STV <- vector()
diff_APD <- vector()
sweep_selection_function_output <- vector()
APDs_APD90 <- data.frame()
APD90_SS <- data.frame()

  if ((nrow(APDs)/length(APD_values)) <= sweeps) {
  
      APD90_SS <- APDs
    
  } else {
    
    APDs_APD90 <- subset(APDs, APDs[[2]] == "APD 90") # selecting only APD90 from the main df containing all APDs
    APD90_values <- APDs_APD90[[3]] # selecting only the column with APD90 values
    Ediast_values <- Ediast_df[[2]] # selecting only the column with Ediast values
    APA_values <- APA[[2]] # selecting only column with APA values
    Peak_values <- Peak[[3]] # selecting only column with Peak_y values
    neg_Ediast <- which(Ediast_values > -95 & Ediast_values < -40 & APA_values > 80 & Peak_values > 0) # This eliminates the vast majority of artifacts
    Ediast_quantile <- 0.55# default level is 0.85 (Ediast values in the lower 85%; the lower the quantile, the more stringent it becomes)
    most_neg_Ediast <- which(Ediast_values <= quantile((Ediast_values), Ediast_quantile)) # extracting the index of AP with an Ediast in the XX% of the most negative values (bottom XX%).

    while (length(most_neg_Ediast) < sweeps & Ediast_quantile <= 1) {
      Ediast_quantile <- Ediast_quantile + 0.05
    
      most_neg_Ediast <- which(Ediast_values < quantile(Ediast_values, Ediast_quantile)) # extracting the index fo APS with ad Ediast in the XX% of most negative values
      low_varibility_Ediast <-  which(c(abs(diff(neg_Ediast, 
                                                 lag=1, 
                                                 differences = 2))) == min(c(abs(diff(neg_Ediast, 
                                                                                      lag=1, 
                                                                                      differences = 2))))) # extracting the points with the minimal difference between one Ediast and two subsequent Ediasts 
      filtered_by_Ediast <- neg_Ediast[low_varibility_Ediast] # chose among the APs with low diast, those with a low variation among consecutive APs.
    }
    
    for (e in 1:(length(most_neg_Ediast)-(sweeps-1))){ # identifying `n` groups of `sweeps` elements among Ediasts
      n_consecutive_values <- seq(from = most_neg_Ediast[[e]], to = (most_neg_Ediast[[e]] + (sweeps - 1) ), by = 1)
      extended_neg_Ediast <- append(extended_neg_Ediast, n_consecutive_values)
    }
    for (a in seq(from = 1, to = length(extended_neg_Ediast), by = sweeps)){ # defining the starting element of each group of `sweeps`
      group <- seq(from = a, to = a + (sweeps-1)) # group of n elements (where n = sweeps) 
      difference <- sum(abs(diff(APD90_values[extended_neg_Ediast[group]]))) # moving difference __within__ each group. This is a *VALUE*
      diff_APD <- append(diff_APD, difference) # combining all the groups in a loop. These are *VALUES*
      #diff_APD <- na.omit(diff_APD)
    }  

    min_diff_APD <- which(diff_APD %in% min(na.omit(diff_APD))) #identifying the index of the group with the min() inter-group APD difference. This is an *INDEX*
    min_diff_APD <- tail(min_diff_APD,1) # If some min_diff_APD are equal, the last index is chosen
    filtered_by_Ediast_STV <- extended_neg_Ediast[(((min_diff_APD * sweeps) - (sweeps - 1)):(min_diff_APD * sweeps))]
    
    #if (all(Peak[seq_along(filtered_by_Ediast_STV), 3] > 0)){
    APD90_SS <- APDs_APD90[c(filtered_by_Ediast_STV),] # this extract the values of the APD from the main vector.
    #} 
  }

sweep_selection_function_output <<- list("APD90_SS" = APD90_SS,
                                        "APDs_APD90" = APDs_APD90)
return(sweep_selection_function_output)
}

