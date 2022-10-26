# ===================================================================
# Code to generate Figure 1 - Spontaneously Beating Nodal Mouse CMs
#
# Purpose: this script generates Figure 3
# Author: Luca Sala, PhD
# Date: 2022-10-04
#
# ===================================================================

#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("../../../libraries/libraries.R")

# Loading ABFs ########
mouse_san_abf <- readABF::readABF("../../raw_data/mouse_nodal/07_12_21/CelIL  07_12_21_0000.abf")

# Transforming the traces and adding the x axis ########

mouse_san <- data.frame(seq.int(0, 
                                5499999*mouse_san_abf$samplingIntervalInSec,
                                mouse_san_abf$samplingIntervalInSec),
                        mouse_san_abf$data[[1]][1:5500000])
mouse_san_fake_iva <- mouse_san
mouse_san_fake_iva$seq.int.0..5499999...mouse_san_abf.samplingIntervalInSec..mouse_san_abf.samplingIntervalInSec. <- mouse_san_fake_iva$seq.int.0..5499999...mouse_san_abf.samplingIntervalInSec..mouse_san_abf.samplingIntervalInSec./0.7 

# Plots ######## 
## Common plot features  ######## 
q <- 
  theme(plot.background = element_rect(color = "white"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.position = c(0.85, 0.95),
        legend.background = element_rect(colour="white", 
                                         size=0.5, linetype="solid"),
        legend.title = element_blank())

## Mouse  SAN ######## 
mouse_san_plot <- 
  ggplot()+
  geom_line(data = mouse_san,
            aes(x = mouse_san[,1],
                y = mouse_san[,2]))+
  geom_line(data = mouse_san_fake_iva,
            aes(x = mouse_san_fake_iva[,1],
                mouse_san_fake_iva[,2]),
            col = "#F4B942")+
  theme_clean()+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  coord_cartesian(x = c(0, 10),
                  y = c(-80, 60))+
  q

mouse_san_freq_means_plot <- 
  auto %>% 
  filter(Parameter != "Condition 1",
         Parameter != "Condition 2",
         Parameter != "Condition 3",
         Parameter != "Peak (mV)",
         Parameter != "SD1_SD2 Ratio",
         Parameter != "SD2",
         Parameter != "RR (ms)",
         Parameter != "Negative dV/dt max y (V/s)") %>% 
  ggplot(aes(x = "Nodal",
             y = Value_Automated))+
  stat_summary(fun = "mean", geom = "bar", colour = "black", fill = "#F4B942")+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)+
  geom_point()+
  facet_wrap(~Parameter, scales = "free_y", ncol = 4)+
  theme_classic()+
  scale_fill_manual(values = c("gray"), labels = c("Healthy"))+
  scale_x_discrete(labels = c("Baseline"))+
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.line=element_line(),
        axis.title.x = element_blank())

# Combine Panels ####
combined_freq_means_plot <-
  plot_grid(mouse_san_plot,
            mouse_san_freq_means_plot,
            nrow = 1,
            rel_widths = c(0.8, 1.5),
            labels = c("C", "D"))

# Combining with mean parameters
source("Figure 3 - Parameter_Comparison.R")

combined <-
  plot_grid(correlations_outlier_plot,                           # correlations_outlier_plot
            combined_freq_means_plot,
            ncol = 1,
            rel_heights = c(1.5,1))

# Save Combined Figure
ggsave("../../figures/Figure 3.png", combined, height = 24, width = 42, units = "cm")
