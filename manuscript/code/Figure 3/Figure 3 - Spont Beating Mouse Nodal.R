# ===================================================================
# Code to generate Figure 1 - Spontaneously Beating Nodal Mouse CMs
#
# Purpose: this script generates Figure 3
# Author: Luca Sala, PhD
# Date: 2022-10-04
#
# ===================================================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../../../libraries/libraries.R")

# Loading ABFs ########
mouse_san_abf <- readABF::readABF("../../raw_data/mouse_nodal/2021_06_02_0008.abf")

# Transforming the traces and adding the x axis ########

mouse_san <- data.frame(seq.int(0, 
                                49999*mouse_san_abf$samplingIntervalInSec,
                                mouse_san_abf$samplingIntervalInSec),
                        mouse_san_abf$data[[1]][1:50000])
mouse_san_fake_iva <- mouse_san
mouse_san_fake_iva$seq.int.0..49999...mouse_san_abf.samplingIntervalInSec..mouse_san_abf.samplingIntervalInSec. <- mouse_san_fake_iva$seq.int.0..49999...mouse_san_abf.samplingIntervalInSec..mouse_san_abf.samplingIntervalInSec./0.7 

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
            col = "red")+
  theme_clean()+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  coord_cartesian(x = c(0, 10),
                  y = c(-80, 60))+
  q

# Save Combined Figure
ggsave("../../figures/Figure 3.png", mouse_san_plot, width = 42, height = 8, units = "cm")

