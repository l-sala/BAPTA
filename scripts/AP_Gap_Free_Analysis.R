# ===================================================================
# Title: AP Gap Free Analysis
#
# Purpose: This script allows the automated analysis of spontaneous APs
# Author: Luca Sala, PhD
# Date: 2022-06-20
# Version: 0.1
# Revisions: 2022-06-20 valleys. 0.1 First version
# ===================================================================

options(warn = -1) #Switching off warnings. In debug mod must be 1!

start_time_general <- Sys.time()

this.dir <- dirname(parent.frame(2)$ofile)
#this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("../libraries/libraries.R")
source("../tools/AP_Sweep_Selection_Function.R")

path = "../data"

dir.names <- list.dirs(path, recursive = F, full.names = F)  #list of directories, recursive = F removes the path directory from the list. 
dir.names.full <- list.dirs(path, recursive = F, full.names = T)
file.number <- sum(sapply(dir.names.full, function(dir){length(list.files(dir, pattern =".abf"))}))

#### INPUT VARIABLES - These are disables when used with Shiny app. Enable to use standalone .R file #### 
# APD_values <- c(10, 30, 50, 70, 90) #seq(10,90  , by = 20) # set the APD intervals. APD90 is mandatory.
# sweeps <- 5 # set the number of sweeps at steady state to be averaged in the analyses.
# sweeps_SD <- 30 # set the number of sweeps for the calculation of SD1 and SD2.#
# minpeakheight <- -10 #Threshold for peak amplitude (+2 in forward steps)

minpeakheight <- minpeakheight - 2 
mode = "Gap Free"
l <- 1
error_df <- data.frame()
#---

# Counting N of files
for(d in 1:length(dir.names)){
  file.names <- dir(path = paste(path,"/",dir.names[d], sep=""), pattern =".abf") #change this if you change the file type
  dir.create(paste("../output/analyses/",dir.names[d], sep = ""), showWarnings = F)
  
  # Creation of the dataframes for averages. These will be created here and will not be overwritten within next loops.
  means_temporary <- data.frame(matrix(ncol = (7 + length(APD_values)), nrow = 0))
  means_df <- data.frame(matrix(ncol = (7 + length(APD_values)), nrow = 0))
  APD_df_mean <- data.frame()
  
  for(f in 1:length(file.names)){
    print(paste("Analyzing file", file.names[f]))  
    start_time <- Sys.time()
    Ediast_SS <- data.frame()
    combined_APs <- data.frame()
    Ediast_list <- data.frame()
    valleys <- data.frame()
    start_time <- Sys.time()
    
    #### ABF FILE IMPORT ####
    abf <- readABF(file.path(path, dir.names[d], file.names[f])) #Reading ABF binary
    voltage_values <- data.frame((ncol = size(abf[["data"]])[2])) 
    si <- abf$samplingIntervalInSec*1000 # Extracting sampling interval in milliseconds
    df <- data.frame(seq(0, ((length(abf[["data"]][[1]])-1) * si), by = si),
                     abf[["data"]][[1]]) #Extracting Voltage and Time from ABF
    colnames(df) <- c("Time", "Voltage")
    source("../tools/Error_Plots.R")
    
    ## Automatic peak identification
    minpeakdistance <- 100/si # Multiply by the "si" to get time units (ms)
    pre_peaks <- data.frame(findpeaks(df$Voltage, zero = "0", minpeakheight = minpeakheight, minpeakdistance = minpeakdistance, sortstr = F)) # Trovo tutti i punti > di una certa soglia mobile di quantile calcolata sui picchi maggiori di una certa soglia.
    pre_peaks <- pre_peaks %>% 
      filter(X1 > (minpeakheight+2))
    pre_peaks <- pre_peaks[order(pre_peaks$X2), c(2,1)]
    
    int_df <- df %>%
      mutate(Voltage = as.integer(Voltage))
    
    ## Additional level to insure that all peaks piked up correctly
    peaks <- data.frame()
    
    for(i in seq(1, nrow(pre_peaks))){
      peak_x <- pre_peaks[i,]
      left <- df[which(int_df$Time < peak_x$X2*si & int_df$Voltage == minpeakheight),]
      right <- df[which(int_df$Time > peak_x$X2*si & int_df$Voltage == minpeakheight),]
      left <- max(left$Time)
      right <- min(right$Time)
      interval <- df[which(df$Time > left & df$Time < right),]  
      peak_temp <- interval[which(interval$Voltage == max(interval$Voltage)),]
      if (peak_temp$Voltage < 70) {
        peaks <- rbind(peaks, peak_temp[1,])
      }
    }
    
    peaks <- unique(peaks) 
    peaks <- peaks[order(peaks$Time), c(2,1)]
    peaks <- peaks %>%
      mutate(relative_Time = Time-Time[1]) %>% 
      filter(Voltage < 70 & Voltage > minpeakheight)

    ## Valley identification
    if(nrow(peaks) > 2) {
      for(i in 1:(nrow(peaks)-1)){
        temp_peak1_x <- peaks[i, "Time"]# finding the p_i_x
        temp_peak2_x <- peaks[i+1, "Time"]# finding p_i+1_x
        
        interval <- 
          df %>%
          rownames_to_column('rn') %>%
          filter(Time > temp_peak1_x & Time < temp_peak2_x) %>%
          mutate(sweep = i, relative_Time = Time-Time[1])
        
        v_y <- min(interval$Voltage)
        v_x_index <- which(interval$Voltage == v_y)
        v_temp <- interval[v_x_index[length(v_x_index)],]
        valleys <- rbind(valleys, v_temp)
      }
    } else {
      print("ERROR: Fail to identify peaks!")
      dir.create(paste("../output/error", sep = ""), showWarnings = F) # creates dir error
      dir.create(paste("../output/error/",dir.names[d], sep = ""), showWarnings = F) # creates file dir in error
      ggsave(paste("../output/error/", dir.names[d], "/",file_path_sans_ext(file.names[f])," Gap-Free ERROR.jpeg", sep = ""), gap_free_error_plot, height = 8, width = 16)
      error_temp <- data.frame("Time" = Sys.time(), "File" = file.names[f], "ERROR" = "Fail to identify peaks!")
      error_df <- rbind(error_df, error_temp)
      write.csv(error_df, paste("../output/error/","Error.csv", sep =""), row.names=FALSE) # saves the csv
      l <- l + 1
      next
    }

    Ediast_list <- valleys
    
    for (k in 1:(nrow(Ediast_list)-1)){ #Except last one. Protection against incomplete AP
      Ediast_interval <- data.frame()
      temp_Ediast1_x <- Ediast_list[k, 2] # trovo x primo Ediast nell'intervallo
      temp_Ediast2_x <- Ediast_list[k+1, 2] # trovo x secondo Ediast nell'intervallo
      Ediast_interval <-
        df %>%
        rownames_to_column('rn') %>%
        filter(Time > temp_Ediast1_x &
                 Time < temp_Ediast2_x) %>%
        mutate(sweep = k,
               relative_Time = Time-Time[1],
               Peak_y = peaks$Voltage[k+1])
      combined_APs <- rbind(combined_APs, Ediast_interval)
    }
    
    Ediast_list <- Ediast_list[-c((nrow(Ediast_list))),]
    
    #Defining dfs  
    Ediast <- data.frame(matrix(ncol = 2, nrow = 0))
    Peak <- data.frame(matrix(ncol = 3, nrow = 0))
    APA <- data.frame(matrix(ncol = 2, nrow = 0))
    dVdt_max <- data.frame(matrix(ncol = 3, nrow = 0))
    AP <- data.frame(matrix(ncol = 5, nrow = 0))
    neg_dVdt_max <- data.frame(matrix(ncol = 3, nrow = 0))
    APD_df <- data.frame(matrix(ncol = 4, nrow = 0)) # create the df for single APD values
    
    # Since the APs will have different length (untidy df), I'll analyze one by one and combine values afterwards
    for(s in 1:length(unique(combined_APs$sweep))){
      AP <-
        combined_APs %>%
        filter(sweep == s) %>% 
        pivot_wider(names_from = sweep,
                    values_from = Voltage) 
      AP <- as.data.frame(AP)
      Peak_y <- AP$Peak_y[1] # this will load the y value of the peak (max value) from peaks
      AP <- AP[, c(3, 5, 1, 2)]
      
      #### Ediast ####
      Ediast_temp <- data.frame(s, mean(head(AP[[2]], 10))) # select 10 points before AP
      
      if (Ediast_temp[1, 2] < -100 & nrow(Ediast) > 0) { # Attempt to fix artifact of RMP
        Ediast_temp[1, 2] <- Ediast[nrow(Ediast), 2]
      }
      
      Ediast <- rbind(Ediast, Ediast_temp)
      
      #### Peak ####
      Peak_x <- AP[,1][which(AP[,2] == Peak_y)] # identify the x value of peak_y
      Peak_x <- Peak_x[1]
      Peak_temp <- data.frame((s),
                              Peak_x, 
                              Peak_y) # combines the coordinates of the peak with the sweep number.
      Peak <- rbind(Peak, Peak_temp)
      
      #### APA ####
      APA_temp <- data.frame(s,
                             (Peak_y - Ediast_temp[,2]))
      APA <- rbind(APA, APA_temp)  # variable that add one row for every sweep.
      APA <- na.omit(APA)
      
      #### dVdt_max ####
      first_der_AP <- data.frame(AP[-1,1], 
                                 diff(AP[,2])/diff(AP[,1])) # generates the first derivative of the AP
      
      dVdt_max_y <- max(first_der_AP[,2]) #divided by 10^3 as expressed in mV/s. Now in V/s
      dVdt_max_x <- first_der_AP[,1][which(first_der_AP[,2] == dVdt_max_y)]
      dVdt_max_temp <- data.frame(s, dVdt_max_x, dVdt_max_y)
      
      if (dVdt_max_temp[1, 3] > 500 & nrow(dVdt_max) > 0) { # Attempt to fix artifact of dV/dt
        dVdt_max_temp[1, 3] <- dVdt_max[nrow(dVdt_max), 3]
      }
      
      dVdt_max <- rbind(dVdt_max, 
                        dVdt_max_temp)  # variable that add one row for every sweep.
      dVdt_max_y <- dVdt_max_y/1000 #divided by 10^3 as expressed in mV/s. Now in V/s
      
      #### Max Slope Repolarization -dV/dt max #### 
      neg_dVdt_max_y <- min(first_der_AP[,2])  # identifies the min value of first derivative, i.e. the negative dVdtmax. #divided by 10^3 as expressed in mV/s. Now in V/s 
      neg_dVdt_max_x <- first_der_AP[,1][which(first_der_AP[,2] == neg_dVdt_max_y)] # finds the coordinates of the dVdtmax
      # combines the coordinates of the -dV/dt max with the sweep number.
      neg_dVdt_max_x <- neg_dVdt_max_x[1] #if there are more than 1, selects the first
      neg_dVdt_max_temp <- data.frame(s, neg_dVdt_max_x, neg_dVdt_max_y)
      neg_dVdt_max <- rbind(neg_dVdt_max, 
                            neg_dVdt_max_temp)
      neg_dVdt_max_y <- neg_dVdt_max_y/1000
      
      #### Action Potential Durations (APDs) ####
      AP_after_peak <- subset(AP, 
                              AP[,1] >= Peak_x) # Subset the data to get the interval from peak to end
      
      for(apds in 1:length(APD_values)){
        APD_y <- (Ediast_temp[,2] + (APA_temp[,2] * ((100-APD_values[apds])/100))) # this calculates the voltage at which we have XX% of the AP span.
        
        if (min(abs(AP_after_peak[,2] - APD_y)) < 0.1) {
          closest_y_value_APD <- min(which(abs(AP_after_peak[,2] - APD_y) < 0.1)) # this calculates the closest point in the AP voltage vector to that.
          } else {
          closest_y_value_APD <- which.min(abs(AP_after_peak[,2] - APD_y)) # in case if true APD value can not be reached 
        } 
        
        APD <- AP_after_peak[closest_y_value_APD,] # this extracts the x and y coordinates of that point.
        APD[,1] <- (APD[,1] - Peak_x) # this is used to subtract the time before the peak. 
        APD_temp <- data.frame(s,
                               paste("APD", APD_values[apds]), 
                               APD[,1], APD[,2], APD[,4]) # this adds the APD index, the APD time value and the APD voltage value
        
        APD_df <- rbind(APD_df, APD_temp) # combine at every loop
        APD_df_all <- APD_df }
      
      source("../tools/AP_Sweep_Selection_Function.R")
      Ediast_df <- Ediast
      sweep_selection_function(APD_df_all, 
                               sweeps)
      
      APD90_SS <- sweep_selection_function_output[[1]]
      
      colnames(APD_df_all) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)", "APD Absolute Time (ms)") # changes names of the columns
    }
    
    #### RR  intervals and Frequency ####
    RR <- data.frame("Sweep (n)" = 1:nrow(peaks), "RR (ms)" = peaks[2] - lag(peaks[2]))
    
    #### SAVING DATA ####
    colnames(Ediast) <- c("Sweep (n)", "Ediast (mV)") # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/Ediast", sep = ""), showWarnings = F, recursive = T) # creates dir Ediast
    write.csv(Ediast, paste("../output/analyses/",dir.names[d],"/Ediast/",file_path_sans_ext(file.names[f]), " Ediast.csv", sep =""), row.names=FALSE) # saves the csv
    
    colnames(Peak) <- c("Sweep (n)", "Peak x (s)", "Peak y (mV)")  # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/Peak", sep = ""), showWarnings = F) # creates dir Peak
    write.csv(Peak, paste("../output/analyses/",dir.names[d],"/Peak/",file_path_sans_ext(file.names[f]), " Peak.csv", sep =""), row.names=FALSE) # saves the csv
    
    colnames(APA) <- c("Sweep (n)", "APA (mV)")  # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/APA/", sep = ""), showWarnings = F) # creates dir APA
    write.csv(APA, paste("../output/analyses/",dir.names[d],"/APA/",file_path_sans_ext(file.names[f]), " APA.csv", sep =""), row.names=FALSE) # saves the csv
    
    colnames(dVdt_max) <- c("Sweep (n)", "dV/dt max x (s)", "dV/dt max y (V/s)")  # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/dVdt_max/", sep = ""), showWarnings = F) # creates dir dVdt_max
    write.csv(dVdt_max, paste("../output/analyses/",dir.names[d],"/dVdt_max/",file_path_sans_ext(file.names[f]), " dVdt_max.csv", sep =""), row.names=FALSE) # saves the csv
    
    colnames(neg_dVdt_max) <- c("Sweep (n)", "Negative dV/dt max x (ms)", "Negative dV/dt max y (V/s)")  # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/Negative_dVdt_max/", sep = ""), showWarnings = F) # creates dir Negative_dVdt_max
    write.csv(neg_dVdt_max, paste("../output/analyses/",dir.names[d],"/Negative_dVdt_max/",file_path_sans_ext(file.names[f]), " Negative dVdt_max.csv", sep =""), row.names=FALSE) # saves the csv
    
    colnames(APD_df) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)", "Absolute Time (s)") # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/APD/", sep = ""), showWarnings = F) # creates dir APD
    write.csv(APD_df, paste("../output/analyses/",dir.names[d],"/APD/",file_path_sans_ext(file.names[f]),".csv", sep =""), row.names=FALSE) # saves the csv
    
    colnames(RR) <- c("Sweep (n)", "RR (ms)") # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/RR/", sep = ""), showWarnings = F) # creates dir RR
    write.csv(RR, paste("../output/analyses/",dir.names[d],"/RR/",file_path_sans_ext(file.names[f]),".csv", sep =""), row.names=FALSE) # saves the csv
    
    
    
    #### SD1 and SD2 calculation #### 
    source("../tools/SD1_Function.R")
    
    if(nrow(APD_df_all)/length(APD_values) >= sweeps_SD){
      sweep_selection_function(APDs = APD_df_all, 
                               sweeps = sweeps_SD+1)
    } else {
      sweep_selection_function(APDs = APD_df_all, 
                               sweeps = sweeps)
    }
    
    APD90_SS <- sweep_selection_function_output[[1]]
    SD1_function(APD_df_all, 
                 nbeats = sweeps)
    
    SD1 <- SD1_function_output[[1]]
    APD90n <- SD1_function_output[[2]]
    APD90n_plus1 <- SD1_function_output[[3]]
    APD90_SD1 <- SD1_function_output[[5]]
    
    SD1_temp <- data.frame(file.names[f],
                           mean(SD1))
    SD1 <- data.frame(SD1)
    SD1_df <- cbind(SD1, SD1_temp)
    Ediast_SD1 <- subset(Ediast_df,
                         Ediast_df$`Sweep (n)` %in% APD90_SS$`Sweep (n)`)
    
    source("../tools/SD2_Function.R")
    SD2_function(APD90_SS, 
                 nbeats = sweeps_SD)
    
    SD2 <- SD2_function_output[[1]]
    
    SD2_temp <- data.frame(file.names[f])
    SD2 <- data.frame(SD2)
    SD2_df <- cbind(SD2, SD2_temp)
    Ediast_SD2 <- subset(Ediast_df,
                         Ediast_df$`Sweep (n)` %in% APD90_SS$`Sweep (n)`)
    
    #### Average Data ####  
    means_temporary <- data.frame(file_path_sans_ext(file.names[f]),
                                  mean(Ediast[,2]),
                                  mean(Peak[,3]),
                                  mean(APA[,2]),
                                  mean(dVdt_max[,3]),
                                  mean(neg_dVdt_max[,3]),
                                  mean(na.omit(RR[,2])),
                                  1000/mean(na.omit(RR[,2])),
                                  sum(SD1[,1]),
                                  sum(SD2[,1]),
                                  sum(SD1[,1])/sum(SD2[,1]))
    
    colnames(means_temporary) <- c("File Name", "Ediast (mV)", "Peak (mV)",
                                   "APA (mV)", "dV/dt max y (V/s)", "Negative dV/dt max y (V/s)", "RR (ms)",
                                   "Frequency (Hz)", "SD1", "SD2", "SD1_SD2 Ratio") # changes names of the columns
    
    means_df <- rbind(means_df,
                      means_temporary)
    
    #### Plots ####
    source("../tools/Plots.R")
    
    dir.create(paste("../output/img/",dir.names[d], sep = ""), 
               showWarnings = F, 
               recursive = T) # creates one dir for each folder
    
    ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," APD Values.jpeg", sep = ""), APD_values_plot, height = 3, width = 8)
    ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," Gap Free.jpeg", sep = ""), gap_free_plot, height = 8, width = 16)
    ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," AP Overlaid Plot.jpeg", sep = ""), ap_overlaid_plot, height = 8, width = 16)
    ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," BVR_APD90.jpeg", sep = ""), BVR_plot, height = 4, width = 4)
    
    #### APD Averages ####
    colnames(APD_df) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)", "Absolute Time (s)") # changes names of the columns
    APD_mean_temporary_data <- 
      APD_df %>% 
      select(-`Absolute Time (s)`) %>% 
      group_by(APD) %>% 
      dplyr::summarise(`Mean APD (ms)` = mean(`APD value (ms)`))
    APD_mean_temporary_data_wide <- spread(APD_mean_temporary_data, APD, `Mean APD (ms)`)
    APD_mean_temporary_data_wide <- cbind(File = file.names[f], APD_mean_temporary_data_wide) # add a column called "File" before the APDs
    APD_df_mean <- smartbind(APD_df_mean,
                             APD_mean_temporary_data_wide) # combine at every loop
    
    print(paste("Finished analysis of file", file.names[f],  "in time:"))
    print(Sys.time() - start_time)
    print(paste(file.number - l, "files remaining."))
    l <- l+1
  }
  
  #### Automatic APD column renaming ####
  apd_names <- c("File") # creating a list with only the first column name
  for (n in 1:length(APD_values)){
    temp_apd_names <- c()
    temp_apd_names <- c(paste("APD", APD_values[n], "(ms)"))
    apd_names <- cbind(apd_names, temp_apd_names) 
  }
  colnames(APD_df_mean) <- apd_names
  
  means_df <- cbind(means_df, APD_df_mean[-1])
  
  write.csv(means_df, 
            paste("../output/analyses/",dir.names[d],"/",dir.names[d], " Mean Values.csv", sep = ""), 
            row.names = FALSE) # saves the csv
}

print("Finished analysis of files. Total time:")
print(Sys.time() - start_time_general)



