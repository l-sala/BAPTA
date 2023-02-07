# ===================================================================
# Parameter Comparison
#
# Purpose: this script creates a table w/ the data from the parameters obtained from the analysis of  
# mouse SAN data from E.Torre
# Author: Luca Sala, PhD
# Date: 2022-10-06
#
# ===================================================================

# Generating and saving the table with parameters automatically extracted from the software. 
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../../../libraries/libraries.R")

path <- "../../outputs/mouse_nodal/analyses"
dir.names <- list.dirs(path, recursive = F, full.names = F)  #list of directories, recursive = F removes the path directory from the list. 

df_averages <- data.frame()
df_averages_temp <- data.frame()

for(d in 1:length(dir.names)){
  file.names <- dir(paste(path,"/",dir.names[d], sep=""), pattern ="Mean Values.csv") #change this if you change the file type
  
  df_averages_temp <- read.csv(paste(path,"/",dir.names[d],"/", file.names, sep=""), check.names=FALSE)
  df_averages_temp$Folder <- dir.names[d] # extract file names
  df_averages$`File Name` <- as.character(df_averages$`File Name`)
  var_names <- strsplit(dir.names[d], "_|\\s+") # splits the folder names after "_"
  
  for(n in 1:length(var_names[[1]])){
    df_averages_temp[[paste("Condition", as.character(n), sep = " ")]] = var_names[[1]][n]
  }
  df_averages <- smartbind(df_averages, df_averages_temp)
}

write_csv(df_averages, "All Mean Values Automated.csv")

# Reading auto and manual data. Manual data have been automatically extracted from Excel tables from E.Torre
auto <- read_csv("All Mean Values Automated.csv")
man <- read_csv("All Mean Values Manual.csv")

auto$Operator <- "Automated"
man$Operator <- "Manual"

man <- remove_empty(man, which = c("cols"))

auto <- auto %>%
  gather("Parameter", "Value_Automated", 
         -c("File Name", "Operator", "Folder", "Condition 4")) 

  #mutate_at(vars(-Value_Automated), as.character)
auto$Value_Automated <- as.numeric(auto$Value_Automated)

man <- man %>%
  gather("Parameter", "Value_Manual", 
         -c("File Name", "Operator", "Folder"))
  # mutate_at(vars(-Value_Manual), as.character)
man$Value_Manual <- as.numeric(man$Value_Manual)


df <- inner_join(auto, man,
                 by = c("File Name", "Folder", "Parameter"))

df <- na.omit(df)
# REMOVING ONE OUTLIER
# # REMOVING OUTLIERS
# df <-
#   df %>%
#   filter() # there are more. The main error is the selection of APs with stimulus in the upstroke phase by the manual operator...

# Plotting correlations
# Linear model and extraction of coefficients
df_mod <- df %>%
  group_by(Parameter) %>%
  do(mod1 = lm(Value_Automated ~ Value_Manual, data = .)) 

#df_coeff <- tidy(df_mod, mod1)

plots <- list() # new empty list
mod <- list()
r2 <- list()

# Looping over all the parameters
for (i in unique(df$Parameter)){
  df_sub <- subset(df, Parameter == i)
  mod[[i]] <- lm(df_sub$Value_Automated ~ df_sub$Value_Manual)
  r2[[i]] <- round(summary(mod[[i]])$r.squared, 2)

  plots[[i]] <-
    ggplot(data = df_sub, aes(x = Value_Manual,
                      y = Value_Automated))+
    stat_smooth(method = "lm", colour = "#F4B942", fill = "#F4B942")+
    geom_point(colour = "black", fill = "gray",pch = 21, size = 2)+
    labs(x = "Manual",
         y = "Automated")+
    ggtitle(i)+
    theme_classic()+
    geom_abline(slope=1, intercept=0, linetype = 2)+
    annotate("text",x=Inf,y=-Inf,
             hjust=1, vjust=-.5,label = paste("R2 =", r2[[i]]))+
    theme(legend.position = "none")
}

# Arranging the plots in a grid
g <- grid.arrange(plots[[1]],
             plots[[2]],
             plots[[3]],
             plots[[4]],
             plots[[5]],
             plots[[6]],
             nrow = 2)

# Saving the grid
ggsave("Parameter_Comparison.jpeg", g, width = 10, height = 6)

# Outlier identification
# Since the STV plot for Manual analysis identifies only values up to 1.25, 
# I select also in the automated analysis the plots that give STV > 1.25
# to identify certain outliers

ratios_plot <- 
  df %>% 
  na.omit() %>%
  select(-c(Operator.x, Operator.y)) %>%
  mutate("Value" = Value_Manual/Value_Automated) %>%
  select(-c(Value_Manual, Value_Automated)) %>%
  gather(Operator, "Value", -c(`File Name`, Folder, Parameter)) %>%
  #filter(Parameter == "STV") %>%
  ggplot(aes(x = `File Name`,
             y = `Value`,
             group = `Operator`))+
  geom_point(alpha = 0.8, fill = "gray", colour = "black", pch = 21)+
  theme_clean()+
  theme(#axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.x=element_blank())+
  facet_wrap(~`Parameter`, ncol = 1)+
  xlab("File")+
  ggtitle("Ratio Manual/Automated Values")

outliers_plot <- 
  df %>% 
  select(-c(Operator.x, Operator.y, `Condition 4`)) %>%
  gather(Operator, "Value", -c(`File Name`, Folder, Parameter)) %>%
  ggplot(aes(x = `File Name`,
             y = `Value`,
             colour = `Operator`,
             group = `Operator`))+
  geom_point(alpha = 0.8)+
  theme_clean()+
  theme(axis.text.x=element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "white"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.line=element_line(),
        axis.title.x = element_blank(),
        plot.background = element_rect(color = "white"),
        legend.position = c(0.95,0.085))+
  scale_colour_manual(values = c("#F4B942", "black"), 
                      labels = c("BAPTA", "Manual"))+
  facet_wrap(~`Parameter`, ncol = 1, scales = "free")+
  labs(x = "File")

  
ggsave("Outlier_check_plot.jpg", outliers_plot, width = 16, height = 10)
ggsave("Ratios_check_plot.jpg", ratios_plot, width = 16, height = 10)

outliers <- 
  df %>% 
  select(-c(Operator.x, Operator.y)) %>%
  mutate("Value" = Value_Manual/Value_Automated) %>%
  select(-c(Value_Manual, Value_Automated)) %>%
  gather(Operator, "Value", -c(`File Name`, Folder, Parameter)) %>%
  filter(Value >= 1.1 | Value <= 0.9) #Values +-10% from manual

write.table(outliers, "Outliers.csv", sep = ",")

correlations_outlier_plot <- plot_grid(outliers_plot,
                                       g, 
                                       ncol = 2,
                                       labels = c("A", "B"))
  
#summary(lm(good$Value_Automated ~ good$Value_Manual))