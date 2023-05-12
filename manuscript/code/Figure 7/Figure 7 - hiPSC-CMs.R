# ===================================================================
# Code to generate Figure 7
#
# Purpose: this script generates Figure 7
# Author: Luca Sala, PhD
# Date: 2022-10-06
#
# ===================================================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../../../libraries/libraries.R")

# Loading ABFs ########
hiPSC_ventricular_immature_abf <- readABF::readABF("../../raw_data/hiPSC-CMs/WTC11_Expanded.D14_Tyr_1Hz/21420035.abf")
hiPSC_ventricular_mature_abf <- readABF::readABF("../../raw_data/hiPSC-CMs/WTC11_Expanded.Maturated_Tyr_1Hz/21504029.abf")

# Transforming the traces and adding the x axis ########
hiPSC_ventricular_immature <- data.frame(seq.int(0,
                                          ((nrow(hiPSC_ventricular_immature_abf$data[[1]])-1)*hiPSC_ventricular_immature_abf$samplingIntervalInSec),
                                          hiPSC_ventricular_immature_abf$samplingIntervalInSec),
                                          hiPSC_ventricular_immature_abf$data[[4]])
hiPSC_ventricular_mature <- data.frame(seq.int(0,
                                          ((nrow(hiPSC_ventricular_mature_abf$data[[1]])-1)*hiPSC_ventricular_mature_abf$samplingIntervalInSec),
                                          hiPSC_ventricular_mature_abf$samplingIntervalInSec),
                                          hiPSC_ventricular_mature_abf$data[[3]])

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
hiPSC_ventricular_plot <- 
  ggplot() +
  geom_line(data = hiPSC_ventricular_immature,
            aes(x = hiPSC_ventricular_immature[,1], # subtracted time to synchronize the peaks
                y = hiPSC_ventricular_immature[,2],
                colour = "Immature"))+
  geom_line(data = hiPSC_ventricular_mature,
            aes(x = hiPSC_ventricular_mature[,1], # subtracted time to synchronize the peaks
                y = hiPSC_ventricular_mature[,2],
                colour = "Mature"))+
  coord_cartesian(x = c(0,0.4),
                  y = c(-80, 60))+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  theme_clean()+
  scale_colour_manual(values = c("black", "#50a668"), labels = c("Immature", "Mature"))+
  q

# Averages ####
df <- read_csv("../../outputs/hiPSC-CMs/analyses/Combined Mean Values.csv")

## REMOVING OUTLIERS
df <-
  df %>%
  filter(`File Name` != "21621049") #Alternanses

hiPSC_freq_means_plot <- 
  df %>% 
  filter(variable != "SD1_SD2 Ratio",
           variable != "Peak (mV)",
           variable != "SD2",
           `Condition 4` != "1Hz") %>% 
  ggplot(aes(x = `Condition 2`,
             y = value,
             fill = `Condition 2`))+
  stat_summary(fun = "mean", geom = "bar", colour = "black")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)+
  geom_point()+
  facet_wrap(~variable, scales = "free_y", ncol = 4)+
  theme_classic()+
  scale_fill_manual(values = c("gray", "#50a668"), labels = c("Immature", "Mature"))+
  scale_x_discrete(labels = c("Immature", "Mature"))+
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.line=element_line(),
        axis.title.x = element_blank())+
  stat_compare_means(aes(label = ..p.signif..),
                     comparisons = list(c("Expanded.D14", "Expanded.Maturated")),
                     method = "t.test", ref.group = "Expanded.D14", size = 4,
                     vjust = -0.2,
                     paired = F,
                     label.y.npc = "center",
                     step.increase = 0.4)+
  scale_y_continuous(expand = c(.1, 0, .5, 0))

# Combine Panels ####
combined_freq_means_plot <-
  plot_grid(hiPSC_ventricular_plot,
            hiPSC_freq_means_plot,
            nrow = 1,
            rel_widths = c(0.8, 1.5),
            labels = c("C", "D"))

# Combining with mean parameters
source("Figure 7 - Parameter_Comparison.R")

combined <-
  plot_grid(correlations_outlier_plot,                          
            combined_freq_means_plot,
            ncol = 1,
            rel_heights = c(1.5,1))

# Save Combined Figure
ggsave("../../figures/Figure 7.png", combined, height = 24, width = 42, units = "cm")
