# ===================================================================
# Code to generate Figure S1
#
# Purpose: this script generates Figure S1
# Author: Luca Sala, PhD
# Date: 2022-10-27
#
# ===================================================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../../../libraries/libraries.R")

# Loading ABFs ########
GP_example <- readABF::readABF("../../raw_data/GP/GP_CTRL_Tyr_1Hz/18d03012.abf")

# Transforming the traces and adding the x axis ########
GP_example <- data.frame(seq.int(0,
                                 ((nrow(GP_example$data[[1]])-1)*GP_example$samplingIntervalInSec),
                                 GP_example$samplingIntervalInSec),
                         GP_example$data[[4]])

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
AP_example_parameter_plot <- 
  ggplot() +
  geom_line(data = GP_example,
            aes(x = GP_example[,1], # subtracted time to synchronize the peaks
                y = GP_example[,2]),
            colour = "black")+
  coord_cartesian(x = c(-0.1,0.3),
                  y = c(-80, 60))+
  annotate("segment", x = -0.04, xend = -0.04, y = -79, yend = 57, colour = "#E57A44", arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm")))+
  annotate("text", x = -0.07, y = -10, label = c("APA"), colour = "#E57A44")+
  annotate("pointrange", x = 0, y = -78, ymin = -79, ymax = -79, colour = "#BCED09", size = 1)+
  annotate("text", x = 0.05, y = -75, label = c("Ediast"), colour = "#BCED09")+
  annotate("pointrange", x = 0.02, y = 57, ymin = 57, ymax = 57, colour = "#63B0CD", size = 1)+
  annotate("text", x = 0.06, y = 57, label = c("Peak"), colour = "#63B0CD")+
  annotate("pointrange", x = 0.02, y = -15, ymin = -15, ymax = -15, colour = "#F9CB40", size = 1)+
  annotate("text", x = 0.07, y = -15, label = c("dV/dt max"), colour = "#F9CB40")+
  annotate("pointrange", x = 0.09, y = 30.81, ymin = 30.81, ymax = 30.81, colour = "#8E5572", size = 1)+
  annotate("text", x = 0.15, y = 31, label = c("APD20"), colour = "#8E5572")+
  annotate("pointrange", x = 0.16, y = -9.94, ymin = -9.94, ymax = -9.94, colour = "#AF4D98", size = 1)+
  annotate("text", x = 0.21, y = -9.94, label = c("APD50"), colour = "#AF4D98")+
  annotate("pointrange", x = 0.18, y = -64.7, ymin = -64.7, ymax = -64.7, colour = "#FF99C8", size = 1)+
  annotate("text", x = 0.23, y = -64.7, label = c("APD90"), colour = "#FF99C8")+
  annotate("segment", x = 0.02, xend = 0.32, y = 65, yend = 65, colour = "#28965A", arrow = arrow(length = unit(.2, "cm")))+
  annotate("text", x = 0.18, y = 60, label = c("RR"), colour = "#28965A")+
  annotate("segment", x = 0.32, xend = 0.32, y = -78, yend = 60, colour = "black")+
  annotate("pointrange", x = 0.32, y = 57, ymin = 57, ymax = 57, colour = "#63B0CD", size = 1)+
  annotate("text", x = 0.285, y = 57, label = c("Peak"), colour = "#63B0CD")+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  theme_clean()+
  q

# Save Combined Figure
ggsave("../../figures/Figure S1.png", AP_example_parameter_plot, height = 8, width = 12, units = "cm")
