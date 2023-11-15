#### This script takes the average data and combine them in one csv file

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

path <- "../output/analyses"
df_averages <- data.frame()
df_averages_temp <- data.frame()
df_apd <- data.frame()
df_apd_temp <- data.frame()

dir.names <- list.dirs(path, recursive = F, full.names = F)  #list of directories, recursive = F removes the path directory from the list. 

#### Averages ####
for(d in 1:length(dir.names)){
  possibleError <- F
  tryCatch({
      file.names <- dir(paste(path,"/",dir.names[d], sep=""), pattern ="Mean Values.csv") #change this if you change the file type
      
      df_averages_temp <- read.csv(paste(path,"/",dir.names[d],"/", file.names, sep=""), check.names=FALSE)
      df_averages_temp$`File Name` <- as.character(df_averages_temp$`File Name`)
      df_averages_temp$Folder <- dir.names[d] # extract file names
      var_names <- strsplit(dir.names[d], "_|\\s+") # splits the folder names after "_"
      
      for(n in 1:length(var_names[[1]])){
         df_averages_temp[[paste("Condition", as.character(n), sep = " ")]] = var_names[[1]][n]
      }
      df_averages <- smartbind(df_averages, df_averages_temp)
      
      ### The current section of the script loads the first representative trace from each file. 
      ### Subsequently, the traces are synchronized based on dV/dt, and Ediast is normalized to the mean value for all traces. 
      ### All intermediate tables are saved separately.
      
      file.path_APs <- dir(paste(path,"/",dir.names[d], "/", "Representative_Traces", sep=""), pattern =".csv", full.names = T)
      file.path_dVdt <- dir(paste(path,"/",dir.names[d], "/", "dVdt_max", sep=""), pattern =".csv", full.names = T)
      
      # Load representative traces from file.names_APs
      Representatives_APs <- NULL
      for (file in file.path_APs) {
        data <- read.table(file, header = T, sep = ",")
        if (is.null(Representatives_APs)) {
          Representatives_APs <- data[, 1]
        }
        Representatives_APs <- cbind(Representatives_APs, data[, 2])
      }
      
      # Rename columns in Representatives_APs
      colnames(Representatives_APs) <- c("Time (ms)", df_averages_temp$`File Name`)
      
      # saves merged table with representatives APs.
      write.csv(Representatives_APs, 
                paste("../output/analyses/", dir.names[d],"/", dir.names[d], " Representatives APs.csv", sep = ""), 
                row.names = FALSE)
      
      # Load representative traces from file.names_dVdt
      Representatives_dVdt <- data.frame()
      for (file in file.path_dVdt) {
        data <- read.table(file, header = T, sep = ",")
        Representatives_dVdt <- rbind(Representatives_dVdt, data[1,])
      }
      
      # Calculate shift for synchronization
      Ediast_shift_value <- Representatives_APs[1,-1] - mean(Representatives_APs[1:10,-1])
      aligment_indices <- match(Representatives_dVdt[,2], Representatives_APs[,1])
      
      # Synchronize traces based on alignment indices
      for (n in 1:length(aligment_indices)){
        shift_value <- max(aligment_indices) - aligment_indices[n]
        Representatives_APs[, n+1] <- c(rep(NA, shift_value), Representatives_APs[, n+1][1:(nrow(Representatives_APs) - shift_value)])
      }
      
      Representatives_APs[,-1] - (Representatives_APs[,-1] - mean(Representatives_APs[1:10,-1]))
      
      # saves merged table with dV/dt aligned representatives APs.
      write.csv(Representatives_APs, 
                paste("../output/analyses/", dir.names[d],"/", dir.names[d], " dV.dt Aligned Representatives APs.csv", sep = ""), 
                row.names = FALSE) # saves the csv
      
      
      Representatives_APs_Norm.Ediast <- data.frame()
      Representatives_APs_Norm.Ediast <- Representatives_APs
      
      for (n in 1:length(Ediast_shift_value)) {
        Representatives_APs_Norm.Ediast[, n+1] <-  Representatives_APs_Norm.Ediast[, n+1] - Ediast_shift_value[n]
      }
      
      # saves merged table with dV/dt aligned representatives APs.
      write.csv(Representatives_APs_Norm.Ediast, 
                paste("../output/analyses/", dir.names[d],"/", dir.names[d], " dV.dt & Ediast Aligned Representatives APs.csv", sep = ""), 
                row.names = FALSE) # saves the csv
      
  }, error=function(e){
    possibleError <<- T
  })
  if(possibleError == T) {next}
}

Conditions <- colnames(df_averages)[(ncol(df_averages)-length(var_names[[1]])+1):ncol(df_averages)]

df_averages_melt <- reshape2::melt(df_averages, id = c(as.character("File Name"), "Folder", Conditions))

write.csv(df_averages, 
          paste("../output/analyses/Combined Mean Values_wide.csv", sep = ""), 
          row.names = FALSE) # saves the csv

write.csv(df_averages_melt, 
          paste("../output/analyses/Combined Mean Values.csv", sep = ""), 
          row.names = FALSE) # saves the csv