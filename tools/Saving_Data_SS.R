# ===================================================================
# Title: Saving_Data_SS
#
# Purpose: Save the selected AP values at SS - Useful for Gap Free
# Author: Luca Sala, PhD
# Date: 2022-10-15
# Version: 0.1
# Revisions: 0.1 First version
# ===================================================================

#### SAVING DATA ####
colnames(Ediast) <- c("Sweep (n)", "Ediast (mV)") # changes names of the columns
dir.create(paste("../output/analyses/",dir.names[d],"/Ediast", sep = ""), showWarnings = F, recursive = T) # creates dir Ediast
Ediast <- Ediast[Ediast$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
write.csv(Ediast, paste("../output/analyses/",dir.names[d],"/Ediast/",file_path_sans_ext(file.names[f]), " Ediast.csv", sep =""), row.names=FALSE) # saves the csv

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

colnames(RR) <- c("Sweep (n)", "RR (ms)") # changes names of the columns
RR <- RR[RR$`Sweep (n)` %in% APD90_SS$`Sweep (n)`, ]
dir.create(paste("../output/analyses/",dir.names[d],"/RR/", sep = ""), showWarnings = F) # creates dir RR
write.csv(RR, paste("../output/analyses/",dir.names[d],"/RR/",file_path_sans_ext(file.names[f]),".csv", sep =""), row.names=FALSE) # saves the csv
