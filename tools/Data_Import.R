# ===================================================================
# Title: Data Import
#
# Purpose: 
# Author: PhD Luca Sala, Vladislav Leonov
# Date: 2022-11-13
# Version: 0.1
# Revisions: Not yet
#
# ===================================================================

#### FILE IMPORT ####
separators <- c(",", ";", "", "\t", ".")

if (type_of_recording == "run_TR") {
  if (file.names[f] %like% ".abf") {
    
    AP <- readABF(file.path(path,dir.names[d], file.names[f]))
    voltage_values <- data.frame((ncol = size(AP[["data"]])[2]))
    voltage_values <- voltage_values[,-1]
    
    ##### Extracting command waveform duration (Vlad) #####
    abf <- file(file.path(path, dir.names[d], file.names[f]), open="rb") #this opens the file as read-binary
    
    int16 <- function (n=1) readBin(abf, n=n, "integer", size=2, endian="little") 
    
    if (AP$formatVersion < 2){
      seek(abf, 2508) # this works for .abf < 2
    } else {
      seek(abf, 3598) # this works for .abf > 2. Searches for the address [binary [Position of EpochPerDACSection is 3584]
    } 
    
    #### Calculation duration of stimulation artifact ####
    Stim_duration_in_points <- (int16)() # in points
    Stim_art_interval_duration <- Stim_duration_in_points*(AP$samplingIntervalInSec*1000) # milliseconds
    close(abf)
    
    #---
    
    # this loop generates a list of APD traces in case multiple signals are present in the file
    for (ap in 1:size(AP[["data"]])[2]){
      voltage_values <- cbind(voltage_values,
                              data.frame(AP[["data"]][[ap]][,1]))
    }
    
    AP <- data.frame((seq(0,length(AP[["data"]][[1]][,1])*AP[[5]]-AP[[5]], by = AP[[5]])),
                     voltage_values)
    # #### UNIT CONVERSION ####
    AP[,1] <- AP[,1]*1000 # This will convert s into ms
    
  } else {
    s <- 1
    repeat {
      AP <- read.csv2(file.path(path, dir.names[d], file.names[f]), 
                      sep = separators[s])
      s <- s + 1
      if (ncol(AP) >= sweeps) {
        break
      }}
    
    AP[,1] <- as.numeric(AP[,1])
    AP[1] <- AP[1]*as.numeric(time_parametr)
    
    for (n in 2:ncol(AP)) {
      AP[, n] <- as.numeric(AP[,n])
    }
    
    si <- AP[2,1] - AP[1,1]
  }
  
  #### Calculation end of stimulation artifact ####
  Stim_art_interval_start <- AP[-1,1][length(AP[-1,1]+1)*1/64] # Duration of stimulus artifact in milliseconds.
  Stim_art_interval_end <- Stim_art_interval_start + Stim_art_interval_duration
  Stim_art_interval_end <- round(Stim_art_interval_end, 2)# in milliseconds

} else if (type_of_recording == "run_GF") {

  if (file.names[f] %like% ".abf") {
    abf <- readABF(file.path(path, dir.names[d], file.names[f])) #Reading ABF binary
    si <- abf$samplingIntervalInSec*1000 # Extracting sampling interval in milliseconds
    voltage_values <- data.frame((ncol = size(abf[["data"]])[2])) 
    df <- data.frame(seq(0, ((length(abf[["data"]][[1]])-1) * si), by = si),
                     abf[["data"]][[1]]) #Extracting Voltage and Time from ABF
    colnames(df) <- c("Time", "Voltage")
    return(si)
    
  } else {  
    
    s <- 1
    repeat {
      df <- read.csv2(file.path(path, dir.names[d], file.names[f]), 
                      sep = separators[s])
      s <- s + 1
      if (ncol(df) == 2) {
        break
      }}
    
    df[,1] <- as.numeric(df[,1])
    df[1] <- df[1]*as.numeric(time_parametr)
    df[,2] <- as.numeric(df[,2])
    colnames(df) <- c("Time", "Voltage")
    si <- df[2,1] - df[1,1]
  }
}