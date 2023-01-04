# ===================================================================
# Code to generate Figure 5
#
# Purpose: this script generates Figure 5
# Author: Luca Sala, PhD
# Date: 2022-10-04
#
# ===================================================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../../../libraries/libraries.R")

# Loading ABFs ########
gp_ventricular_05Hz_abf <- readABF::readABF("../../raw_data/GP/GP_CTRL_Tyr_0.5Hz/18215005.abf")
gp_ventricular_1Hz_abf <- readABF::readABF("../../raw_data/GP/GP_CTRL_Tyr_1Hz/18215006.abf")
gp_ventricular_2Hz_abf <- readABF::readABF("../../raw_data/GP/GP_CTRL_Tyr_2Hz/18215007.abf")
gp_ventricular_4Hz_abf <- readABF::readABF("../../raw_data/GP/GP_CTRL_Tyr_4Hz/18215008.abf")

# Transforming the traces and adding the x axis ########
gp_ventricular_05Hz <- data.frame(seq.int(0,
                                          ((nrow(gp_ventricular_05Hz_abf$data[[1]])-1)*gp_ventricular_05Hz_abf$samplingIntervalInSec),
                                          gp_ventricular_05Hz_abf$samplingIntervalInSec),
                                  gp_ventricular_05Hz_abf$data[[1]])
gp_ventricular_1Hz <- data.frame(seq.int(0,
                                         ((nrow(gp_ventricular_1Hz_abf$data[[1]])-1)*gp_ventricular_1Hz_abf$samplingIntervalInSec),
                                         gp_ventricular_1Hz_abf$samplingIntervalInSec),
                                 gp_ventricular_1Hz_abf$data[[1]])
gp_ventricular_2Hz <- data.frame(seq.int(0,
                                         ((nrow(gp_ventricular_2Hz_abf$data[[1]])-1)*gp_ventricular_2Hz_abf$samplingIntervalInSec),
                                         gp_ventricular_2Hz_abf$samplingIntervalInSec),
                                 gp_ventricular_2Hz_abf$data[[1]])
gp_ventricular_4Hz <- data.frame(seq.int(0,
                                         ((nrow(gp_ventricular_4Hz_abf$data[[1]])-1)*gp_ventricular_4Hz_abf$samplingIntervalInSec),
                                         gp_ventricular_4Hz_abf$samplingIntervalInSec),
                                 gp_ventricular_4Hz_abf$data[[1]])

# Plots ######## 
## Common plot features  ######## 
q <- 
  theme(plot.background = element_rect(color = "white"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.position = c(0.85, 0.90),
        legend.background = element_rect(colour="white", 
                                         size=0.5, linetype="solid"),
        legend.title = element_blank())

## GP Ventricular ######## 
gp_ventricular_05Hz %>% 
  ggplot(aes(x = gp_ventricular_05Hz[,1],
             y = gp_ventricular_05Hz[,2]))+
  geom_line()+
  coord_cartesian(x = c(0,0.4),
                  y = c(-80, 60))+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  theme_clean()+
  ggtitle("Guinea Pig Ventricular")+
  q

gp_ventricular_plot <- 
  ggplot() +
  geom_line(data = gp_ventricular_05Hz,
            aes(x = gp_ventricular_05Hz[,1]-0.028, # subtracted time to synchronize the peaks
                y = gp_ventricular_05Hz[,2],
                colour = "0.5 Hz"))+
  geom_line(data = gp_ventricular_1Hz,
            aes(x = gp_ventricular_1Hz[,1]-0.013, # subtracted time to synchronize the peaks
                y = gp_ventricular_1Hz[,2],
                colour = "1 Hz"))+
  geom_line(data = gp_ventricular_2Hz,
            aes(x = gp_ventricular_2Hz[,1]-0.005, # subtracted time to synchronize the peaks
                y = gp_ventricular_2Hz[,2],
                colour = "2 Hz"))+
  geom_line(data = gp_ventricular_4Hz,
            aes(x = gp_ventricular_4Hz[,1],
                y = gp_ventricular_4Hz[,2],
                colour = "4 Hz"))+
  coord_cartesian(x = c(0,0.4),
                  y = c(-80, 60))+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  theme_clean()+
  scale_colour_manual(values = c("black", "#0077b6", "#0096c7", "#00b4d8"))+
  q

# Averages ####
df <- read_csv("../../outputs/GP/analyses/Combined Mean Values.csv")

# # REMOVING OUTLIERS
df <-
  df %>%
  filter(`File Name` != "17502015", # Artifact in peak - Manual wrong
         `File Name` != "17n30003", #Issues with the file
         `File Name` != "17502016", # Artifact in peak - Manual wrong
         `File Name` != "17421028", # No AP - Manual wrong
         `File Name` != "17421051", # Artifact in peak - Manual wrong
         `File Name` != "18d03009", # No file?
         `File Name` != "18d03014", # No AP - Manual wrong
         `File Name` != "18d03026", # No file?
         `File Name` != "17421047", # Maybe artifact in peak?
         `File Name` != "17427009", # Artifact in peak - Manual wrong
         `File Name` != "19321005", # No file?
         `File Name` != "17421020", # Artifact in peak - Manual wrong
         `File Name` != "17502017", # Artifact in peak - Manual wrong
         `File Name` != "17502009", # Maybe artifact in peak?
         `File Name` != "17n30009", # Wrong file?
         `File Name` != "18215024", # Artifact in peak
         `File Name` != "17502005", # Artifact in peak - Manual wrong
         `File Name` != "17421024", # Artifact in peak - Manual wrong
         `File Name` != "17502013", # dubbio?
         `File Name` != "17421027", # Artifact in peak - Manual wrong
         `File Name` != "17421018", # Artifact in peak - Manual wrong
         `File Name` != "17502014", # Artifact in peak - Manual wrong
         `File Name` != "18d03015" # Maybe artifact in peak?
  )

GP_freq_means_plot <- 
  df %>% 
  filter(variable != "SD1_SD2 Ratio",
           variable != "Peak (mV)",
           variable != "SD2") %>% 
  ggplot(aes(x = `Condition 4`,
             y = value,
             fill = `Condition 4`))+
  stat_summary(fun = "mean", geom = "bar", colour = "black")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)+
  geom_point()+
  facet_wrap(~variable, scales = "free_y", ncol = 4)+
  theme_classic()+
  scale_fill_manual(values = c("gray", "#0077b6", "#0096c7", "#00b4d8"), labels = c("0.5 Hz", "1 Hz", "2 Hz", "4 Hz"))+
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.line=element_line())

# Combine Panels ####
combined_freq_means_plot <-
  plot_grid(gp_ventricular_plot,
            GP_freq_means_plot,
            nrow = 1,
            rel_widths = c(0.8, 1.5),
            labels = c("C", "D"))

# Combining with mean parameters
source("Figure 5 - Parameter_Comparison.R")

combined <- 
  plot_grid(correlations_outlier_plot,
            combined_freq_means_plot,
            ncol = 1,
            rel_heights = c(1.5,1))

# Save Combined Figure
ggsave("../../figures/Figure 5.png", combined, height = 24, width = 42, units = "cm")
