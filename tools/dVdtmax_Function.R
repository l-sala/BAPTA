# ===================================================================
# Title: dVdtmax
#
# Purpose: This script allows the automated extraction of the second peak of the dV/dt of an Action Potential to automatically detect the dV/dtmax.
# Author: PhD Luca Sala, Vladislav Leonov
# Date: 2022-11-15
# Version: 0.2
# Revisions: Not yet
#
# ===================================================================

### Calculation of starting point to search for dV/dt max value considering stimuli duration. ###

if (file.names[f] %like% ".abf") {
  Stim_art_interval_start <- (first_der_AP[,1][round(length(first_der_AP[,1])*1/64)]) # Duration of stimulus artifact. These are milliseconds. Can be converted into a variable?
  Stim_art_interval_end <- Stim_art_interval_start + Stim_art_interval_duration
  Stim_art_interval_end <- round(Stim_art_interval_end, 2)
  ### dV/dt max identification for .abf ###
  first_der_AP_stim_int <- first_der_AP[
    (which(first_der_AP[,1] == as.character(Stim_art_interval_end))):(which(first_der_AP[,1] == as.character(Peak_x))), 1:2]

} else {
  ### dV/dt max identification for .txt or .csv ###
  first_der_AP_stim_int <- first_der_AP[1:(which(first_der_AP[,1] == as.character(Peak_x))), 1:2]  
  }
  
colnames(first_der_AP_stim_int) <- c("first_der_AP_stim_int_x", "first_der_AP_stim_int_y")
if (any(first_der_AP_stim_int[,2] < -0.3)){
  first_der_AP_stim_int <- first_der_AP_stim_int[!!cumsum(first_der_AP_stim_int[,2] < -0.3), 1:2]
}
  
p <- max(first_der_AP_stim_int$first_der_AP_stim_int_y)
latest_peak <- first_der_AP_stim_int[which(first_der_AP_stim_int[,2] == p),] 
dVdtmax_function_output <<- list("dVdt_max_x" <- latest_peak[,1], # This extracts the y value (dV/dt max) of the latest peak.
                                   "dVdt_max_y" <- latest_peak[,2])

return(dVdtmax_function_output)

