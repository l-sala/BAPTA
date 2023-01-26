# ===================================================================
# Title: Saving_Data
#
# Purpose: Save all the AP values
# Author: Luca Sala, PhD; Vladislav Leonov
# Date: 2023-01-26
# Version: 0.2
# Revisions: 0.2 Second version 2023-01-26
#            0.1 First version 2022-10-15
# ===================================================================

#### SAVING DATA ####

# Saving Representative Traces #
if (representatives == T){  
  if (mode == "Triggered"){
    Representative_Traces <- AP_plot_data_SS_wide
    colnames(Representative_Traces) <- c("Time (ms)", paste("Sweep ", APD90_SS[,1], ". Voltage (mV)", sep=""))
    dir.create(paste("../output/analyses/",dir.names[d],"/Representative_Traces", sep = ""), showWarnings = F, recursive = T) # creates dir for Representative Traces
    write.csv(Representative_Traces, paste("../output/analyses/",dir.names[d],"/Representative_Traces/", file_path_sans_ext(file.names[f]), " Representative_Traces.csv", sep =""), row.names=FALSE) # saves the csv
    
  } else if (mode == "Gap Free"){
    Representative_Traces <- combined_APs[combined_APs$sweep %in% APD90_SS$`Sweep (n)`, c(3,4,5)]
    colnames(Representative_Traces) <- c("Voltage (mV)", "Sweep (n)", "Time (ms)")
    dir.create(paste("../output/analyses/",dir.names[d],"/Representative_Traces/", file_path_sans_ext(file.names[f]), sep = ""), showWarnings = F, recursive = T) # creates dir for Representative Traces
    for(r in unique(Representative_Traces$`Sweep (n)`)){
      Representative_Trace <- Representative_Traces %>% 
        filter(`Sweep (n)` == r)
      Representative_Trace <- Representative_Trace[, c(3, 1)]
      write.csv(Representative_Trace, paste("../output/analyses/",dir.names[d],"/Representative_Traces/", 
                                            file_path_sans_ext(file.names[f]), "/Sweep-", r,".csv", sep =""), row.names=FALSE) # saves the csv
    }
  }
}

# Saving All Parameters #
if (saving_all_or_SS == "SS") {
  
  colnames(Ediast) <- c("Sweep (n)", "Ediast (mV)") # changes names of the columns
  dir.create(paste("../output/analyses/",dir.names[d],"/Ediast", sep = ""), showWarnings = F, recursive = T) # creates dir Ediast
  Ediast <- Ediast[Ediast$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
  write.csv(Ediast, paste("../output/analyses/",dir.names[d],"/Ediast/", file_path_sans_ext(file.names[f]), " Ediast.csv", sep =""), row.names=FALSE) # saves the csv
  
  colnames(Peak) <- c("Sweep (n)", "Peak x (ms)", "Peak y (mV)")  # changes names of the columns
  dir.create(paste("../output/analyses/",dir.names[d],"/Peak", sep = ""), showWarnings = F) # creates dir Peak
  Peak <- Peak[Peak$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
  write.csv(Peak, paste("../output/analyses/",dir.names[d],"/Peak/",file_path_sans_ext(file.names[f]), " Peak.csv", sep =""), row.names=FALSE) # saves the csv
  
  colnames(APA) <- c("Sweep (n)", "APA (mV)")  # changes names of the columns
  dir.create(paste("../output/analyses/",dir.names[d],"/APA/", sep = ""), showWarnings = F) # creates dir APA
  APA <- APA[APA$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
  write.csv(APA, paste("../output/analyses/",dir.names[d],"/APA/",file_path_sans_ext(file.names[f]), " APA.csv", sep =""), row.names=FALSE) # saves the csv
  
  colnames(dVdt_max) <- c("Sweep (n)", "dV/dt max x (ms)", "dV/dt max y (V/s)")  # changes names of the columns
  dir.create(paste("../output/analyses/",dir.names[d],"/dVdt_max/", sep = ""), showWarnings = F) # creates dir dVdt_max
  dVdt_max <- dVdt_max[dVdt_max$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
  write.csv(dVdt_max, paste("../output/analyses/",dir.names[d],"/dVdt_max/",file_path_sans_ext(file.names[f]), " dVdt_max.csv", sep =""), row.names=FALSE) # saves the csv
  
  colnames(neg_dVdt_max) <- c("Sweep (n)", "Negative dV/dt max x (ms)", "Negative dV/dt max y (V/s)")  # changes names of the columns
  dir.create(paste("../output/analyses/",dir.names[d],"/Negative_dVdt_max/", sep = ""), showWarnings = F) # creates dir Negative_dVdt_max
  neg_dVdt_max <- neg_dVdt_max[neg_dVdt_max$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
  write.csv(neg_dVdt_max, paste("../output/analyses/",dir.names[d],"/Negative_dVdt_max/",file_path_sans_ext(file.names[f]), " Negative dVdt_max.csv", sep =""), row.names=FALSE) # saves the csv
  
  colnames(APD_df) <- c("Sweep (n)", "APD", "APD value (ms)", "APD value (mV)") # changes names of the columns
  APD_df <- APD_df[APD_df$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
  dir.create(paste("../output/analyses/",dir.names[d],"/APD/", sep = ""), showWarnings = F) # creates dir APD
  write.csv(APD_df, paste("../output/analyses/",dir.names[d],"/APD/",file_path_sans_ext(file.names[f]),".csv", sep =""), row.names=FALSE) # saves the csv
  
  if (mode == "Gap Free"){
    colnames(RR) <- c("Sweep (n)", "RR (ms)") # changes names of the columns
    RR <- RR[RR$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
    dir.create(paste("../output/analyses/",dir.names[d],"/RR/", sep = ""), showWarnings = F) # creates dir RR
    write.csv(RR, paste("../output/analyses/",dir.names[d],"/RR/",file_path_sans_ext(file.names[f]),".csv", sep =""), row.names=FALSE) # saves the csv
  }

} else if(saving_all_or_SS == "All"){

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
  
  if (mode == "Gap Free"){
    colnames(RR) <- c("Sweep (n)", "RR (ms)") # changes names of the columns
    dir.create(paste("../output/analyses/",dir.names[d],"/RR/", sep = ""), showWarnings = F) # creates dir RR
    write.csv(RR, paste("../output/analyses/",dir.names[d],"/RR/",file_path_sans_ext(file.names[f]),".csv", sep =""), row.names=FALSE) # saves the csv
  }
}

