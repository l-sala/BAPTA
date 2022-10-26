# ===================================================================
# Title: SD2 Function
#
# Purpose: This function allows the calculation of Long Term Variability (SD2) in APD of "n" contiguous APs
# Author: Luca Sala, PhD
# Date: 2018-09-20
# Version: 0.2
# Revisions: 2021-01-06
#
# ===================================================================

SD2_function <- function(x,
                         nbeats) { #default = 30
  x = APD90_SS
  APD90n = x[-nrow(x),]
  APD90n_plus1 = x[-1,]
  APD90mean = mean(APD90n[,3])
  APD90_SD2 = data.frame(APD90n[,3], APD90n_plus1[,3])
  
  APD_diffs <- c()
  SD2_temp <- c()
  
    for(i in 1:nrow(APD90_SD2)){ #
      APD_diffs_temp <- abs(APD90_SD2[i,1] + APD90_SD2[i,2] - (2*APD90mean)) / ((nbeats)*sqrt(2)) #SD2
      APD_diffs <- rbind(APD_diffs, APD_diffs_temp)
    } 
  
  SD2 <- sum(APD_diffs) / ((nbeats)*sqrt(2))
  SD2_function_output <<- list("SD2" = SD2, 
                              "APD90n" = APD90n, 
                              "APD90n_plus1" = APD90n_plus1, 
                              "APD90mean" = APD90mean, 
                              "APD90_SD2" = APD90_SD2,
                              "APD_diffs" = APD_diffs)
  return(SD2_function_output)
  
  }


