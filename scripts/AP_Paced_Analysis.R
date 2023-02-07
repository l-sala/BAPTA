# ===================================================================
# Title: AP Batch Analysis
#
# Purpose: This script allows the automated analysis of APs from adult, neonatal and hiPSC-derived cardiomyocytes.
# Author: Luca Sala, PhD
# Date: 2018-09-21
# Version: 0.95
# Revisions: 2018-09-21 - v 0.8
#            2018-12-18 - v 0.81
#            2020-04-14 - v 0.9
#            2020-04-28 - v 0.91  
#            2020-09-04 - v 0.92
#            2021-01-06 - v 0.95
# ===================================================================

options(warn = -1) #Switching off warnings. In debug mod must be 1!

start_time <- Sys.time()

this.dir <- dirname(parent.frame(2)$ofile)
#this.dir <- dirname(rstudioapi::getSourceEditorContext()$path) #activate for debugging
setwd(this.dir)

source("../libraries/libraries.R")
source("../tools/AP_Sweep_Selection_Function.R")

path = "../data"
dir.names <- list.dirs(path, recursive = F, full.names = F)  #list of directories, recursive = F removes the path directory from the list. 
mode = "Triggered"
saving_all_or_SS <- "SS"

#### INPUT VARIABLES - These are disabled when used with Shiny app. Enable to use standalone .R file #### 
  #APD_values <- c(10, 30, 50, 70, 90) #seq(10,90  , by = 20) # set the APD intervals. APD90 is mandatory.
  #sweeps <- 5 # set the number of sweeps at steady state to be averaged in the analyses.
  #sweeps_SD <- 30 # set the number of sweeps for the calculation of SD1 and SD2.
  #time_parametr <- 1000 # 1000 in case of seconds
  #data_pattern <- ".abf"
  #type_of_recording <- "run_TR"
#--- 

#### Initialize variable ####
file.number <- vector()
error_df <- data.frame()
l <- 1 # variable to count files remaining
#---

# Counting N of files
for(d in 1:length(dir.names)){
  file.number.temp <- length(dir(paste(path,"/",dir.names[d], sep=""), pattern = data_pattern)) #change this if you change the file type
  file.number <- sum(file.number, file.number.temp)
}

for(d in 1:length(dir.names)){
    file.names <- dir(paste(path,"/",dir.names[d], sep=""), pattern = data_pattern) #change this if you change the file type
    dir.create(paste("../output/analyses/",dir.names[d], sep = ""), showWarnings = F)
  
    # Creation of the dataframes for averages. These will be created here and will not be overwritten within loops.
    means_temporary <- data.frame(matrix(ncol = (7 + length(APD_values)),
                                         nrow = 0))
    means_df <- data.frame(matrix(ncol = (7 + length(APD_values)), 
                                  nrow = 0))
    APD_df_mean <- data.frame()
    #
    
    for(f in 1:length(file.names)){
    possibleError <- F
    print(paste("Analyzing file", file.names[f]))
    
    tryCatch({
      Ediast <- data.frame(matrix(ncol = 2, nrow = 0))
      Peak <- data.frame(matrix(ncol = 3, nrow = 0))
      APA <- data.frame(matrix(ncol = 2, nrow = 0))
      dVdt_max <- data.frame(matrix(ncol = 3, nrow = 0))
      neg_dVdt_max <- data.frame(matrix(ncol = 3, nrow = 0))
      APD_df <- data.frame(matrix(ncol = 4, nrow = 0)) # create the df for single APD values
      
    
      #### FILE IMPORT ####
    
      source("../tools/Data_Import.R")
      
      if (ncol(AP) <= sweeps) { #bypass files that have less sweeps than the required `n` (useless for the analysis) 
    
        f = f+1
    
      } else {
      
        for (k in 2:(ncol(AP))){
      
          AP_plot_data <- data.frame(AP[,1], 
                                     AP[,k]) # these are used to store data for the plot
          
          #### Ediast ####
          Ediast_pre <- AP[c(1:10),k] # select 10 points before AP
          
          #Ediast_post <- AP[c(nrow(AP):(nrow(AP)-9)), k] # select 10 points at the end of the file (careful with this if you have DAD/EAD)
          Ediast_post <- Ediast_pre # Replace this line with the line above to use also the end Ediast (not recommended)
          
          Ediast_trace <- data.frame((k-1),
                                     mean(c(Ediast_pre, 
                                            Ediast_post))) # combines the mean of Ediasts pre and post.
          Ediast <- smartbind(Ediast, Ediast_trace) # variable that add one row for every sweep.
          Ediast_df <- Ediast
          
          #### Peak ####
          Peak_y <- max(AP[2:nrow(AP)*0.1,k]) # this will identify the y value of the peak (max value in the first 10% of the trace)
          Peak_x <- AP[,1][which(AP[,k] == Peak_y)] # identify the x value of peak_y
          Peak_x <- Peak_x[1] # takes the first point to ensure that even if they are multiple points with the same voltage (e.g. a flat peak) only one is taken 
          Peak_trace <- data.frame((k-1),
                                   Peak_x, 
                                   Peak_y) # combines the coordinates of the peak with the sweep number.
          Peak <- smartbind(Peak, Peak_trace)
          
          #### APA ####
          APA_trace <- data.frame((k-1),
                                  (Peak_y - mean(c(Ediast_pre, Ediast_post))))
          APA <- smartbind(APA, APA_trace)  # variable that add one row for every sweep.
          
          #### dVdt_max ####
          first_der_AP <- data.frame(AP[-1,1], 
                                     diff(AP[,k])/diff(AP[,1])) # generates the first derivative of the AP
          
          source("../tools/dVdtmax_Function.R")
          
          dVdt_max_trace <- data.frame((k-1),
                                       dVdtmax_function_output[[1]], 
                                       dVdtmax_function_output[[2]]) # combines the coordinates of the dVdtmax with the sweep number.
          
          dVdt_max <- smartbind(dVdt_max, dVdt_max_trace)  # variable that add one row for every sweep.
          
          #### Max Slope Repolarization -dV/dt max #### 
          neg_dVdt_max_y <- min(first_der_AP[,2]) # identifies the max value of first derivative, i.e. the negative dVdtmax 
          neg_dVdt_max_x <- first_der_AP[,1][which(first_der_AP[,2] == neg_dVdt_max_y)] # finds the coordinates of the dVdtmax
          neg_dVdt_max_trace <- data.frame((k-1),
                                           neg_dVdt_max_x, 
                                           neg_dVdt_max_y) # combines the coordinates of the dVdtmax with the sweep number.
          neg_dVdt_max <- smartbind(neg_dVdt_max, 
                                    neg_dVdt_max_trace)  # variable that add one row for every sweep.
          ####
          
          #### Action Potential Durations (APDs) ####
          AP_after_peak <- subset(AP[,c(1,k)], 
                                  AP[,1] >= Peak_x) # Subset the data to get the interval from peak to end
      
              for(apds in 1:length(APD_values)){
            
                APD_y <- (mean(c(Ediast_pre, Ediast_post)) + (APA_trace[,2] * ((100-APD_values[apds])/100))) # this calculates the voltage at which we have XX% of the AP span.
                closest_y_value_APD <- which.min(abs(AP_after_peak[,2] - APD_y)) # this calculates the closest point in the AP voltage vector to that.
                APD <- AP_after_peak[closest_y_value_APD,] # this extracts the x and y coordinates of that point.
                APD[,1] <- (APD[,1] - Peak_x) # this is used to subtract the time before the peak. 
                APD_temporary_data <- data.frame((k-1),
                                                 paste("APD", APD_values[apds]), 
                                                 APD[,1], APD[,2]) # this adds the APD index, the APD time value and the APD voltage value
            
                APD_df <- smartbind(APD_df, APD_temporary_data) # combine at every loop
                APD_df_all <- APD_df 
              }
        }
      
      colnames(APD_df) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)") # changes names of the columns
      colnames(APD_df_all) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)") # changes names of the columns
      colnames(Ediast_df) <- c("Sweep (n)", "Ediast (mV)")
      
      #### Loading the AP Sweep Selection script
      #### This script identifies a steady state region based on Ediast and APD90 variability 
      
      sweep_selection_function(APDs = APD_df_all, sweeps = sweeps)
      APD90_SS <- sweep_selection_function_output[[1]]
      # APD90_SS_APDs <- 
      #   APD90_SS %>% 
      #   filter(`Sweep (n)` >= Ediast$`Sweep (n)`[1] &
      #          `Sweep (n)` <= Ediast$`Sweep (n)`[length(Ediast$`Sweep (n)`)] )
      APD_df_APD90 <- sweep_selection_function_output[[2]]
    
      #### SAVING DATA ####
      source("../tools/Saving_Data.R")
        
      APD_90_plot <- APD_df %>% filter(APD == "APD 90") # APD values used for plotting red dots
      colnames(APD_df_all) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)") # changes names of the columns
        
      #### SD1 calculation #### 
      source("../tools/SD1_Function.R")
        
        if(nrow(APD_df_all)/length(APD_values) >= sweeps_SD){
          sweep_selection_function(APDs = APD_df_all, 
                                   sweeps = sweeps_SD+1)
        } else {
          sweep_selection_function(APDs = APD_df_all, 
                                   sweeps = sweeps)
          }
        
        APD90_SS_SD <- sweep_selection_function_output[[1]]
        SD1_function(APD_df_all, 
                      nbeats = sweeps_SD)
        
        SD1 <- SD1_function_output[[1]]
        APD90n <- SD1_function_output[[2]]
        APD90n_plus1 <- SD1_function_output[[3]]
        APD90_SD1 <- SD1_function_output[[5]]
        
        SD1_temp <- data.frame(file.names[f],
                               mean(SD1))
        SD1 <- data.frame(SD1)
        SD1_df <- cbind(SD1, SD1_temp)
        Ediast_SD1 <- subset(Ediast_df,
                             Ediast_df$`Sweep (n)` %in% APD90_SS_SD$`Sweep (n)`)
        
        source("../tools/SD2_Function.R")
        SD2_function(APD90_SS_SD, 
                     nbeats = sweeps_SD)
        
        SD2 <- SD2_function_output[[1]]
        #APD90n <- SD2_function_output[[2]]
        #APD90n_plus1 <- SD2_function_output[[3]]
        #APD90_SD2 <- SD2_function_output[[5]]
        
        SD2_temp <- data.frame(file.names[f])
        SD2 <- data.frame(SD2)
        SD2_df <- cbind(SD2, SD2_temp)
        Ediast_SD2 <- subset(Ediast_df,
                             Ediast_df$`Sweep (n)` %in% APD90_SS_SD$`Sweep (n)`)
    
        #### Plots ####
        
        source("../tools/Plots.R")
        
        dir.create(paste("../output/img/",dir.names[d], sep = ""), 
                   showWarnings = F, 
                   recursive = T) # creates one dir for each folder
      
          ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," APD Values.jpeg", sep = ""), APD_values_plot, height = 3, width = 8)
          ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," AP Parameters.jpeg", sep = ""), AP_plots_grid, height = 8, width = 16)
          ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," BVR_APD90.jpeg", sep = ""), BVR_plot, height = 4, width = 4)
          ggsave(paste("../output/img/", dir.names[d], "/",file_path_sans_ext(file.names[f])," SS Selection.jpeg", sep = ""), SS_Selection_plots, height = 8, width = 8)
       
      #### APD Averages ####
      colnames(APD_df) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)") # changes names of the columns
      APD_mean_temporary_data <- 
        APD_df %>% 
        group_by(APD) %>% 
        dplyr::summarise(`Mean APD (ms)` = mean(`APD value (ms)`))
      APD_mean_temporary_data_wide <- spread(APD_mean_temporary_data, APD, `Mean APD (ms)`)
      APD_mean_temporary_data_wide <- cbind(File = file.names[f], APD_mean_temporary_data_wide) # add a column called "File" before the APDs
      APD_df_mean <- smartbind(APD_df_mean,
                               APD_mean_temporary_data_wide) # combine at every loop
    
      #### Average Data ####  
      means_temporary <- data.frame(file_path_sans_ext(file.names[f]),
                                      mean(Ediast[,2]),
                                      mean(Peak[,3]),
                                      mean(APA[,2]),
                                      mean(dVdt_max[,3]),
                                      mean(neg_dVdt_max[,3]),
                                      sum(SD1[,1]),
                                      sum(SD2[,1]),
                                      sum(SD1[,1])/sum(SD2[,1]))
      
      colnames(means_temporary) <- c("File Name", "Ediast (mV)", "Peak (mV)",
                                     "APA (mV)", "dV/dt max y (V/s)", "Negative dV/dt max y (V/s)",
                                     "SD1", "SD2", "SD1_SD2 Ratio") # changes names of the columns
      
      means_df <- rbind(means_df,
                            means_temporary)
        }
      
      print(paste("Finished analysis of file", file.names[f], "-", file.number - l, "files remaining."))
      l = l+1
      }, error=function(e){
        possibleError <<- T
      
      l <<- l+1
      
      print(paste("ERROR: File", file.names[f], "contains data that can not be analyzed properly"))
      source("../tools/Error_Plots.R")
      dir.create(paste("../output/error/",dir.names[d], sep = ""), showWarnings=F, recursive=T) # creates error dir
      ggsave(paste("../output/error/", dir.names[d], "/",file_path_sans_ext(file.names[f])," ERROR.jpeg", sep = ""), error_plot, height = 8, width = 16)
      error_temp <- data.frame("Time" = Sys.time(), "File" = file.names[f], "ERROR" = "File contains data that can not be analyzed properly")
      error_df <- rbind(error_df, error_temp)
      write.csv(error_df, paste("../output/error/","Error.csv", sep =""), row.names=FALSE) # saves the csv
      })
      if(possibleError == T) {next}
  } #End of file analysis
    
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
    
} #Total END

source("../tools/Combined_Table.R")

end_time <- Sys.time()
paste("This analysis took", round(end_time - start_time, 2), "seconds", sep = " ")

