# ===================================================================
# Code to generate Figure 4
#
# Purpose: this script generates Figure 4
# Author: Luca Sala, PhD
# Date: 2022-10-06
#
# ===================================================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../../../libraries/libraries.R")

# Loading ABFs ########
human_atrial_0_5Hz_abf <- readABF::readABF("../../raw_data/human_atrial/HS_Atrial_Tyr_0.5Hz/14205006.abf")

# Transforming the traces and adding the x axis ########
human_atrial_0_5Hz <- data.frame(seq.int(0,
                                          ((nrow(human_atrial_0_5Hz_abf$data[[1]])-1)*human_atrial_0_5Hz_abf$samplingIntervalInSec),
                                         human_atrial_0_5Hz_abf$samplingIntervalInSec),
                                 human_atrial_0_5Hz_abf$data[[4]])

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

## hiPSC-CMs Ventricular ######## 
human_atrial_plot <- 
  ggplot() +
  geom_line(data = human_atrial_0_5Hz,
            aes(x = human_atrial_0_5Hz[,1]-0.015, # subtracted time to synchronize the peaks
                y = human_atrial_0_5Hz[,2],
                colour = "Atrial"))+
  coord_cartesian(x = c(0,1),
                  y = c(-80, 40))+
  scale_y_continuous(breaks = seq(-80,40, by=40))+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  theme_clean()+
  scale_colour_manual(values = c("#d62828"))+
  q

# Averages ####
df <- read_csv("../../outputs/human_atrial/analyses/Combined Mean Values.csv")

## REMOVING OUTLIERS
# df <-
#   df %>%
#   filter(`File Name` != "")

human_atrial_freq_means_plot <- 
  df %>% 
  filter(variable != "SD1_SD2 Ratio",
           variable != "Peak (mV)",
           variable != "SD2",
          variable != "Negative dV/dt max y (V/s)",
         `Condition 4` != "2Hz") %>% 
  ggplot(aes(x = `Condition 2`,
             y = value,
             fill = `Condition 2`))+
  stat_summary(fun = "mean", geom = "bar", colour = "black")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)+
  geom_point()+
  facet_wrap(~variable, scales = "free_y", ncol = 4)+
  theme_classic()+
  scale_fill_manual(values = c("#d62828"))+
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.line=element_line(),
        axis.title.x = element_blank())

# Combine Panels ####
combined_freq_means_plot <-
  plot_grid(human_atrial_plot,
            human_atrial_freq_means_plot,
            nrow = 1,
            rel_widths = c(0.8, 1.5),
            labels = c("C", "D"))

# Combining with mean parameters
source("Figure 4 - Parameter_Comparison.R")

combined <-
  plot_grid(correlations_outlier_plot,                          
            combined_freq_means_plot,
            ncol = 1,
            rel_heights = c(1.5,1))

# Save Combined Figure
ggsave("../../figures/Figure 4.png", combined, height = 24, width = 42, units = "cm")
