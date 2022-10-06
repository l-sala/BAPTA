this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("libraries/libraries.R")

path = "data"

dir.names <- list.dirs(path, recursive = F, full.names = F)  #list of directories, recursive = F removes the path directory from the list. 
df <- data.frame()
df_temp <- data.frame()

for(d in 1:length(dir.names)){
  file.names <- dir(paste(path,"/",dir.names[d], sep=""), pattern =".abf") #change this if you change the file type
  var_names <- strsplit(dir.names[d], "_|\\s+") # splits the folder names after "_"
  for(f in 1:length(file.names)){
  df_temp <- data.frame(file.names[f], dir.names[d], var_names[[1]][1], var_names[[1]][2], var_names[[1]][3])
  df <- rbind(df, df_temp)
  }
}


colnames(df) <- c("File Name", "Dir Name", "Cell Line", "Condition", "Frequency")

write.csv(df, paste("IClamp Table_", Sys.Date(), ".csv", sep = ""))

