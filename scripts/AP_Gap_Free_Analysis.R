# ===================================================================
# Title: AP Gap Free Analysis
#
# Purpose: This script allows the automated analysis of spontaneous APs
# Author: Luca Sala, PhD
# Date: 2022-06-20
# Version: 0.1
# Revisions: 2022-06-20 v. 0.1 First version
# ===================================================================

start_time <- Sys.time()

#this.dir <- dirname(parent.frame(2)$ofile)
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("../libraries/libraries.R")
source("../tools/AP_Sweep_Selection_Function.R")

path = "../data"

dir.names <- list.dirs(path, recursive = F, full.names = F)  #list of directories, recursive = F removes the path directory from the list. 

#### INPUT VARIABLES - These are disables when used with Shiny app. Enable to use standalone .R file #### 
APD_values <- c(10, 30, 50, 70, 90) #seq(10,90  , by = 20) # set the APD intervals. APD90 is mandatory.
sweeps <- 5 # set the number of sweeps at steady state to be averaged in the analyses.
sweeps_SD <- 30 # set the number of sweeps for the calculation of SD1 and SD2.#
mode = "Gap Free"
#--- 

#### Initialize variable ####
file.number <- vector()
l <- 1 # variable to count files remaining
#---

# Creation of the dataframes for averages. These will be created here and will not be overwritten within loops.
means_temporary <- data.frame(matrix(ncol = (7 + length(APD_values)),
                                     nrow = 0))
means_df <- data.frame(matrix(ncol = (7 + length(APD_values)), 
                              nrow = 0))
APD_df_mean <- data.frame()

# Counting N of files
for(d in 1:length(dir.names)){
  file.names <- dir(path = paste(path,"/",dir.names[d], sep=""), pattern =".abf") #change this if you change the file type
  dir.create(paste("../output/analyses/",dir.names[d], sep = ""), showWarnings = F)
  
  for(f in 1:length(file.names)){
    print(paste("Analyzing file", file.names[f]))  
    pvofin <- data.frame()
    Ediast_SS <- data.frame()
    combined_APs <- data.frame()
    Ediast_list <- data.frame()
    v <- data.frame()
    #diff_Ediast <- vector()
    #min_diff_APD <- vector()
    
    #### ABF FILE IMPORT ####
    #abf <-  readABF::readABF("/Users/lsala/Documents/GitHub/AP-batch-analysis/manuscript/raw_data/data_mouse_nodal/2021_06_02_0008.abf") #does work
    abf <- readABF::readABF("/Users/lsala/Documents/GitHub/AP-batch-analysis/manuscript/raw_data/data_mouse_nodal/Extracted ABF/2021_08_14_0000.abf copy 5")
    #abf <- readABF(file.path(path, dir.names[d], file.names[f]))
    voltage_values <- data.frame((ncol = size(abf[["data"]])[2]))
    #voltage_values <- voltage_values[,-1]
    si <- abf$samplingIntervalInSec*1000 # sampling interval now in milliseconds
    df <- data.frame(seq(0, ((length(abf[["data"]][[1]])-1) * si), by = si),
                     abf[["data"]][[1]])
    df <- df[c(1:100000), ]
    
    ## Automatic peak calculation - Finds all the points above the 0.8 quartile of voltage. These are A LOT of points, but it's the first filter
    colnames(df) <- c("Time", "Voltage")
    p <- data.frame(findpeaks(df$Voltage, zero = "0", minpeakheight  = quantile(df$Voltage, 0.9), sortstr=F))
    
    # X1 is voltage, X2 is minimum time in case equal values of X1, X3 is time, X4 is maximum time in case equal values of X1
    # Finds all the points > a certain mobile quantile threshold. This defines the coordinates for each peak.
    # dico di fermarsi quando i punti sono non consecutivi (da fine ) --> questo è l'intervallo dei picchi
    
    # Filtering peaks < 65 mV as not physiological for CMs
    p <- 
      p %>% 
      filter(X1 < 65)
    
    # Defining a cutoff as minimal distance (in points) + sd (in points) between points identified as peaks by the `findpeaks` function above
    cutoff <- min(na.omit(p$X3-lag(p$X3))) + sd(na.omit(p$X3-lag(p$X3)))
    # since I expect all the peak values to cluster together. **Cutoff is expressed in points here, not in time units. Multiply bu the "si" to get time units"
    
    pk <- which((p$X3-lag(p$X3)) > cutoff) # points that should be peaks. These are INDICES, not values.
    pkk <- p[pk,] # indices transformed in values
    
    p <- data.frame(findpeaks(df$Voltage, zero = "0", minpeakdistance = cutoff, sortstr = F)) # Trovo tutti i punti > di una certa soglia mobile di quantile calcolata sui picchi maggiori di una certa soglia.
    p_df <- data.frame()
    p_df_temp <- data.frame()
    z <- 2
    n <- 1
    p_first = data.frame(p[1,"X3"], p[1,"X1"], n, z-1)
    names(p_first) = c("Time", "Voltage", "n", "z")
    
    # per ogni valore di voltaggio nel df dei picchi, se il tempo tra i due picchi è > 1000 (punti) oppure
    # se il valore assoluto tra i tempi dei due picchi è > 1000 (punti)
    # crea un df contenente i valori che rispettano i criteri. Combina ogni risultato per ogni riga di p
    for(n in 2:(nrow(p)-1)){
      if(p$X3[n+1]-p$X3[n] > 1000 | abs(p$X3[n]-p$X3[n-1]) > 1000){
        p_df_temp <- data.frame(p[n,"X3"], p[n, "X1"], n, z)
        p_df <- rbind(p_df, p_df_temp)
        #z = z+1
      }
    }
    
    names(p_df) = c("Time", "Voltage", "n", "z")
    p_last = data.frame(p[nrow(p),"X3"], p[nrow(p),"X1"], n, z)
    names(p_last) = c("Time", "Voltage", "n", "z")
    p_first_last = rbind(p_first, setNames(p_last, names(p_first)))
    p_df <- rbind(p_df, setNames(p_first_last, names(p_first_last)))
    p_df <- p_df[order(p_df$Time),c(1:4)] #ordering peak and valleys by the column indicating points (X3)
    
    p_df <-
      p_df %>%
      filter(((Time - lag(Time)) > 15/si)) # this is a threshold, maybe can be modified depending on the species? It works well with 250 ms (50 points/0.2 ms)
    
    p_df <- p_df[-c(1),] #removing first two points as they may cause issues with incomplete APs
    #p_df <- p_df[-c((nrow(p_df))),] #removing last point as they may cause issues with incomplete APs
    
    p <- data.frame()
    max_interval_temp <- data.frame()
    
    for(pt in seq(1, nrow(p_df)-1)){
      p_i1 <- p_df[pt,]
      p_i2 <- p_df[pt+1,]
      p_i_x1 <- round(p_i1[[1]]*si)
      p_i_x2 <- round(p_i2[[1]]*si)
      interval <- df[which(df$Time > p_i_x1 & df$Time < p_i_x2),]
      max_interval_temp_y <- max(interval$Voltage)
      max_interval_temp <-
        data.frame(
          interval$Time[interval$Voltage == max_interval_temp_y],
          max_interval_temp_y)
      max_interval_temp <- max_interval_temp[1,]
      p <- rbind(p, max_interval_temp)
    }
    
    colnames(p) <- c("Time", "Voltage")
    p <- 
      p %>% 
      filter(p$Voltage > mean(df$Voltage))
    # plot(df$Time, df$Voltage)
    # points(p$Time,
    #        p$Voltage, col = "red")
    
    ## fine analisi peaks    
    
    p$type <- "p" #p stands for peak
    #p <- p[,-c(2,4)]
    #colnames(p) <- c("Voltage", "Time", "type")
    po <- p[order(p$Time),c(1:3)] #ordering peak and valleys by the column indicating points (X3)
    po <- 
      po %>%
      #rownames_to_column('rn') %>%
      mutate(#sweep = i,
        relative_Time = Time-Time[1],
        Time = (Time/si+df$Time[1]/si)) %>% ##Fattore 20 molto dubbio, nn capisco perché serva
      filter(po$Voltage < 65 &
               po$Voltage > -10) #conditions < 65 and > -10 should guarantee that most of the artifacts are avoided
    
    po <-
      po %>%
      filter(((Time/si - lag(Time/si)) > 50/si)) # this is a threshold, maybe can be modified depending on the species? It works well with 250 ms (50 points/0.2 ms)
    
    
    if(nrow(po) > 1){
      for(i in 1:(nrow(po)-1)){ # per ogni picco...
        print(paste("i =", i))
        temp_peak1_x <- po[i, "Time"]*si # finding the p_i_x
        temp_peak2_x <- po[i+1, "Time"]*si # finding p_i+1_x
        
        interval <- 
          df %>%
          rownames_to_column('rn') %>%
          filter(Time > temp_peak1_x &
                   Time < temp_peak2_x) %>% 
          mutate(sweep = i,
                 relative_Time = Time-Time[1])
        
        #v <- data.frame(findpeaks(-interval$Voltage, minpeakdistance = 40000, zero = "+", threshold = quantile(interval$Voltage, 0.05) , minpeakheight = 25, sortstr=F, nups=3))
        v_y <- min(interval$Voltage)
        v_x_index <- which(interval$Voltage == v_y)
        v_temp <- interval[v_x_index[length(v_x_index)],]
        v_temp$type <- "v" #v stands for valley
        #v$X1 <- -v$X1
        #v <- data.frame()
        v <- rbind(v, v_temp)
        
        pv <- full_join(po, v)
        print("join between po and v")
        pvo <- pv[order(pv$Time),c(1:5)] #ordering peak and valleys by the column indicating points (X3)
        # pvo <- 
        #   pvo %>% 
        #   mutate(X2 = X2*si,
        #          X3 = X3*si,
        #          X4 = X4*si) #converting points to time (seconds)
        
        # se ci sono due v adiacenti, prendi la più negativa delle due, l'altra al 99% è un falso.
        if(pvo$type[i] == pvo$type[i+1] & pvo$Voltage[i] < pvo$Voltage[i+1]){ # se trova due punti di valley uguali, seleziona il più negativo 
          pvotemp <- pvo[c(i+1),]
          pvofin <- rbind(pvofin, pvotemp)
        } else if(pvo$type[i] == pvo$type[i+1] & pvo$Voltage[i] > pvo$Voltage[i+1]){ # mette in un df provvisorio i più positivi
          pvotemp <- pvo[c(i),]
          pvofin <- rbind(pvofin, pvotemp)
        }
        
        if(nrow(pvofin) > 0){ # se non ci sono punti doppi, non fa la l'anti_join
          pvo <- anti_join(pvo, pvofin)
          print("antijoin between pvo and pvofin")
        }
      }}
    
    Ediast_list <- v
    for (k in 1:(nrow(Ediast_list)-1)){
      Ediast_interval <- data.frame()
      temp_Ediast1_x <- Ediast_list[k, 2] # trovo x primo Ediast nell'intervallo
      temp_Ediast2_x <- Ediast_list[k+1, 2] # trovo x secondo Ediast nell'intervallo
      Ediast_interval <-
        df %>%
        rownames_to_column('rn') %>%
        filter(Time > temp_Ediast1_x &
                 Time < temp_Ediast2_x) %>%
        mutate(sweep = k,
               relative_Time = Time-Time[1])
      combined_APs <- rbind(combined_APs, Ediast_interval)
    }
    
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
      print(paste("s is", s))
      AP <-
        combined_APs %>%
        filter(sweep == s) %>% 
        #select(-c(rn, Time)) %>% 
        pivot_wider(names_from = sweep,
                    values_from = Voltage) 
      AP <- as.data.frame(AP)
      AP <- AP[, c(3, 4, 1, 2)]
      
      #### Ediast ####
      Ediast_temp <- data.frame(s, mean(head(AP[[2]], 10))) # select 10 points before AP
      Ediast <- rbind(Ediast, Ediast_temp)
      
      #### Peak ####
      Peak_y <- max(AP[2:nrow(AP),2]) # this will identify the y value of the peak (max value)
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
      dVdt_max <- rbind(dVdt_max, 
                        dVdt_max_temp)  # variable that add one row for every sweep.
      dVdt_max_y <- dVdt_max_y/1000 #divided by 10^3 as expressed in mV/s. Now in V/s
      
      #### Max Slope Repolarization -dV/dt max #### 
      neg_dVdt_max_y <- min(first_der_AP[,2])  # identifies the min value of first derivative, i.e. the negative dVdtmax. #divided by 10^3 as expressed in mV/s. Now in V/s 
      neg_dVdt_max_x <- first_der_AP[,1][which(first_der_AP[,2] == neg_dVdt_max_y)] # finds the coordinates of the dVdtmax
      # combines the coordinates of the dVdtmax with the sweep number.
      neg_dVdt_max_x <- neg_dVdt_max_x[1] #if there are more than 1, selects the first
      neg_dVdt_max_temp <- data.frame(s, neg_dVdt_max_x, neg_dVdt_max_y)
      neg_dVdt_max <- rbind(neg_dVdt_max, 
                            neg_dVdt_max_temp)
      neg_dVdt_max_y <- neg_dVdt_max_y/1000
      
      #### Action Potential Durations (APDs) ####
      AP_after_peak <- subset(AP, 
                              AP[,1] >= Peak_x) # Subset the data to get the interval from peak to end
      
      for(apds in 1:length(APD_values)){
        print(paste("apds =", apds)) #flag
        APD_y <- (Ediast_temp[,2] + (APA_temp[,2] * ((100-APD_values[apds])/100))) # this calculates the voltage at which we have XX% of the AP span.
        closest_y_value_APD <- which.min(abs(AP_after_peak[,2] - APD_y)) # this calculates the closest point in the AP voltage vector to that.
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
    #APD90n <- SD2_function_output[[2]]
    #APD90n_plus1 <- SD2_function_output[[3]]
    #APD90_SD2 <- SD2_function_output[[5]]
    
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
                                  sum(SD1[,1]),
                                  sum(SD2[,1]),
                                  sum(SD1[,1])/sum(SD2[,1]))
    
    colnames(means_temporary) <- c("File Name", "Ediast (mV)", "Peak (mV)",
                                   "APA (mV)", "dV/dt max y (V/s)", "Negative dV/dt max y (V/s)",
                                   "SD1", "SD2", "SD1_SD2 Ratio") # changes names of the columns
    
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
print(paste("Finished analysis of file", file.names[f], "-", file.number - l, "files remaining."))



