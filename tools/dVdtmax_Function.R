# ===================================================================
# Title: dVdtmax
#
# Purpose: This script allows the automated extraction of the second peak of the dV/dt of an Action Potential to automatically detect the dV/dtmax.
# Author: Luca Sala, PhD
# Date: 2018-12-18
# Version: 0.1
# Revisions: Not yet
#
# ===================================================================

require("pracma") # this package includes the findpeaks function

Stim_art_interval_start <- (first_der_AP[,1][round(length(first_der_AP[,1])*1/64)]) # Duration of stimulus artifact. These are milliseconds. Can be converted into a variable?
Stim_art_interval_end <- Stim_art_interval_start + Stim_art_interval_duration
Stim_art_interval_end <- round(Stim_art_interval_end, 2)

# # The two lines of code below extract the x intervals (and the respective y values) from the AP peak to ~ 25 points before ####
first_der_AP_stim_int_x <- first_der_AP[,1][(which(first_der_AP[,1] == as.character(Stim_art_interval_end))):((which(first_der_AP[,1] == as.character(Stim_art_interval_end)))+500)]
first_der_AP_stim_int_y <- first_der_AP[,2][which(first_der_AP[,1] == first_der_AP_stim_int_x[1]): which(first_der_AP[,1] == first_der_AP_stim_int_x[length(first_der_AP_stim_int_x)])]
first_der_AP_stim_int <- data.frame(first_der_AP_stim_int_x,
                                    first_der_AP_stim_int_y)

p <- max(first_der_AP_stim_int$first_der_AP_stim_int_y)

latest_peak <- first_der_AP_stim_int[which(first_der_AP_stim_int_y == p),] 

# This extracts the y value (dV/dt max) of the latest peak.



dVdtmax_function_output <<- list("dVdt_max_x" <- latest_peak[,1],
                                 "dVdt_max_y" <- latest_peak[,2])
return(dVdtmax_function_output)

