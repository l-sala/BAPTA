# ===================================================================
# Code to generate Figure 5 - Healthy and Diseased Rat CMs
#
# Purpose: this script generates Figure 5
# Author: Luca Sala, PhD
# Date: 2022-10-04
#
# ===================================================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("../../../libraries/libraries.R")

# Loading ABFs ########
rat_healthy_abf <- readABF::readABF("../../raw_data/rat/RN_CTR_DMSO_1Hz/18711000.abf")
rat_diseased_abf <- readABF::readABF("../../raw_data/rat/RN_STZ_DMSO_1Hz/18703019.abf")

# Transforming the traces and adding the x axis ########
rat_healthy <- data.frame(seq.int(0,
                                  ((nrow(rat_healthy_abf$data[[1]])-1)*rat_healthy_abf$samplingIntervalInSec),
                                  rat_healthy_abf$samplingIntervalInSec),
                          rat_healthy_abf$data[[4]])
rat_diseased <- data.frame(seq.int(0,
                                   ((nrow(rat_diseased_abf$data[[1]])-1)*rat_diseased_abf$samplingIntervalInSec),
                                   rat_diseased_abf$samplingIntervalInSec),
                           rat_diseased_abf$data[[3]])

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
rat_ventricular_plot <- 
  ggplot() +
  geom_line(data = rat_healthy,
            aes(x = rat_healthy[,1], # subtracted time to synchronize the peaks
                y = rat_healthy[,2],
                colour = "Immature"))+
  geom_line(data = rat_diseased,
            aes(x = rat_diseased[,1], # subtracted time to synchronize the peaks
                y = rat_diseased[,2],
                colour = "Mature"))+
  coord_cartesian(x = c(0,0.4),
                  y = c(-80, 60))+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  theme_clean()+
  scale_colour_manual(values = c("black", "#8C5471"), 
                      labels = c("Healthy", "Diseased"))+
  q

# Averages ####
df <- read_csv("../../outputs/rat/analyses/Combined Mean Values.csv")

## REMOVING OUTLIERS
df <-
  df %>%
  filter(`File Name` != "18704028",
         `File Name` != "18704029",
         `File Name` != "18d03014",
         `File Name` != "18712050")

rat_freq_means_plot <- 
  df %>% 
  filter(variable != "SD1_SD2 Ratio",
         variable != "Peak (mV)",
         variable != "SD2",
         `Condition 4` == "1Hz") %>% 
  ggplot(aes(x = `Condition 2`,
             y = value,
             fill = `Condition 2`))+
  stat_summary(fun = "mean", geom = "bar", colour = "black")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)+
  geom_point()+
  facet_wrap(~variable, scales = "free_y", ncol = 4)+
  theme_classic()+
  scale_fill_manual(values = c("gray", "#8C5471"), labels = c("Healthy", "Diseeased"))+
  scale_x_discrete(labels = c("Healthy", "Diseased"))+
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.line=element_line(),
        axis.title.x = element_blank())+
  stat_compare_means(aes(label = ..p.signif..),
                     comparisons = list(c("CTR", "STZ")),
                     method = "t.test", ref.group = "CTR", size = 4, position = "identity",
                     vjust = -0.5,
                     paired = F)+
  scale_y_continuous(expand = c(.1, 0, .3, 0))

# Combine Panels ####
combined_freq_means_plot <-
  plot_grid(rat_ventricular_plot,
            rat_freq_means_plot,
            nrow = 1,
            rel_widths = c(0.8, 1.5),
            labels = c("C", "D"))

# Combining with mean parameters
source("Figure 5 - Parameter_Comparison.R")

combined <-
  plot_grid(correlations_outlier_plot,                           # correlations_outlier_plot
            combined_freq_means_plot,
            ncol = 1,
            rel_heights = c(1.5,1))

# Save Combined Figure
ggsave("../../figures/Figure 5.png", combined, height = 24, width = 42, units = "cm")
