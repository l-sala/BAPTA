# ===================================================================
# Title: SD1 Function
#
# Purpose: This function allows the calculation of Short Term Variability (SD1) in APD of "n" contiguous APs
# Author: Luca Sala, PhD
# Date: 2018-09-20
# Version: 0.2
# Revisions: 2021-01-06
#
# ===================================================================

SD1_function <- function(x,
                         nbeats) { #default = 30
  APD90n = x[-nrow(x),]
  APD90n_plus1 = x[-1,]
  APD90mean = mean(APD90n[,3])
  APD90_BVR = data.frame(APD90n[,3], APD90n_plus1[,3])
  
  APD_diffs <- c()
  SD1_temp <- c()
  
    for(i in 1:nrow(APD90_BVR)){ #
      APD_diffs_temp <- abs(APD90_BVR[i,2] - APD90_BVR[i,1]) #SD1 from Heijman et al., https://doi.org/10.1371/journal.pcbi.1003202
      APD_diffs <- rbind(APD_diffs, APD_diffs_temp)
    } 
  
  SD1 <- sum(APD_diffs) / ((nbeats)*sqrt(2))
  SD1_function_output <<- list("SD1" = SD1, 
                              "APD90n" = APD90n, 
                              "APD90n_plus1" = APD90n_plus1, 
                              "APD90mean" = APD90mean, 
                              "APD90_BVR" = APD90_BVR,
                              "APD_diffs" = APD_diffs)
  return(SD1_function_output)
  
  }


