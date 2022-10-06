#### This script takes the average data and plots data grouped by Cycle Length (e.g. 1 Hz, 2 Hz, etc) 
#### and divided by condition (e.g. CTR vs DRUG).

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../tools/AP_Summary_Plots.R")
source("../libraries/libraries.R")
source("../tools/SD1_Function.R")
source("../tools/SD2_Function.R")

path <- "../output/analyses"
df_averages <- data.frame()
df_averages_temp <- data.frame()
df_apd <- data.frame()
df_apd_temp <- data.frame()

dir.names <- list.dirs(path, recursive = F, full.names = F)  #list of directories, recursive = F removes the path directory from the list. 

#### Averages ####
for(d in 1:length(dir.names)){
      file.names <- dir(paste(path,"/",dir.names[d], sep=""), pattern ="Mean Values.csv") #change this if you change the file type
      
      df_averages_temp <- read.csv(paste(path,"/",dir.names[d],"/", file.names, sep=""), check.names=FALSE)
      df_averages_temp$`File Name` <- as.character(df_averages_temp$`File Name`)
      df_averages_temp$Folder <- dir.names[d] # extract file names
      var_names <- strsplit(dir.names[d], "_|\\s+") # splits the folder names after "_"
      
      for(n in 1:length(var_names[[1]])){
         df_averages_temp[[paste("Condition", as.character(n), sep = " ")]] = var_names[[1]][n]
      }
      df_averages <- smartbind(df_averages, df_averages_temp)
}

Conditions <- colnames(df_averages)[(ncol(df_averages)-n+1):ncol(df_averages)]

df_averages_melt <- reshape2::melt(df_averages, id = c(as.character("File Name"), "Folder", Conditions))

write.csv(df_averages, 
          paste("../output/analyses/Combined Mean Values_wide.csv", sep = ""), 
          row.names = FALSE) # saves the csv

write.csv(df_averages_melt, 
          paste("../output/analyses/Combined Mean Values.csv", sep = ""), 
          row.names = FALSE) # saves the csv

for (i in levels(factor(df_averages_melt$variable))){

# This line below calculates the minimum number of variables to have all the columns in one row in the plot
#columns <- length(levels(factor(df_averages_melt$variable)))

plot_APD_split <-
   df_averages_melt %>%
   filter(variable == i) %>%
   final_scatter_plot_split(i)
plot_APD_combined <-
   df_averages_melt %>%
   filter(variable == i) %>%
   final_scatter_plot_combined(i)

# The plot size is calculated from the number of variables
width <- length(var_names[[1]])*4#Automatically resize graphs based on input conditions

# Some parameters (i) have "/" in their file names and cause error when saving the file name. ####
# This code will remove any / from the file name. 

i <- str_extract(i, "\\w*[^/()mV.ms]\\w+\\w?")

#### Saving plots as images ####
ggsave(paste("../output/img/", i, "_Averages_Combined.jpeg", sep = ""), 
       plot_APD_combined, 
       height = width/(1.5*length(var_names[[1]])), width = width/2)

   if(length(var_names[[1]]) == 1){
      next() 
   } else {
      ggsave(paste("../output/img/", i, "_Averages.jpeg", sep = ""), 
             plot_APD_split, 
             height = width/(1.5*length(var_names[[1]])), width = width/1.75)
   }
}

#### Saving interactive plots ####
zdas<- ggplotly(plot_APD_combined)
htmlwidgets::saveWidget(as_widget(zdas), paste(getwd(), "../output/img/", i, "_Averages_Combined.html", sep = ""))

# ---
# Use this to check single values
# df_averages %>%
   # filter(`Condition 4` == "1Hz") %>%
   # ggplot(aes(x = `Condition 2`,
   #            y = `APD 80 (ms)`))+
   # stat_summary(fun = "mean", geom = "point")+
   # stat_summary(fun.data = "mean_se", geom = "errorbar")+
   # geom_point()+
   # facet_wrap(~`Condition 1`)
