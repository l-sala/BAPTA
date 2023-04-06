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
  }, error=function(e){
    possibleError <<- T
  })
  if(possibleError == T) {next}
}

Conditions <- colnames(df_averages)[(ncol(df_averages)-n+1):ncol(df_averages)]

df_averages_melt <- reshape2::melt(df_averages, id = c(as.character("File Name"), "Folder", Conditions))

write.csv(df_averages, 
          paste("../output/analyses/Combined Mean Values_wide.csv", sep = ""), 
          row.names = FALSE) # saves the csv

write.csv(df_averages_melt, 
          paste("../output/analyses/Combined Mean Values.csv", sep = ""), 
          row.names = FALSE) # saves the csv
