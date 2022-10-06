# ===================================================================
# Code to generate Figure 1
#
# Purpose: this script generates Figure 1
# Author: Luca Sala, PhD
# Date: 2022-10-04
#
# ===================================================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("../../../libraries/libraries.R")

# Loading ABFs ########
mouse_san_abf <- readABF::readABF("../../raw_data/mouse_nodal/LS/2021_06_02_0009.abf")
rat_healthy_abf <- readABF::readABF("../../raw_data/rat/RN_CTR_DMSO_1Hz/18711000.abf")
gp_ventricular_1Hz_abf <- readABF::readABF("../../raw_data/GP/GP_CTRL_Tyr_1Hz/17427007.abf")
hiPSC_ventricular_mature_abf <- readABF::readABF("../../raw_data/hiPSC-CMs/WTC11_Expanded.Maturated_Tyr_0.5Hz/21621026.abf")

# Transforming the traces and adding the x axis ########
## Mouse SAN ####
mouse_san <- data.frame(seq.int(0, 
                                49999*mouse_san_abf$samplingIntervalInSec,
                                mouse_san_abf$samplingIntervalInSec),
                        mouse_san_abf$data[[1]][41001:91000])
mouse_san_fake_iva <- mouse_san
mouse_san_fake_iva$seq.int.0..49999...mouse_san_abf.samplingIntervalInSec..mouse_san_abf.samplingIntervalInSec. <- mouse_san_fake_iva$seq.int.0..49999...mouse_san_abf.samplingIntervalInSec..mouse_san_abf.samplingIntervalInSec./0.7 

## Rat ####
rat_healthy <- data.frame(seq.int(0,
                                  ((nrow(rat_healthy_abf$data[[1]])-1)*rat_healthy_abf$samplingIntervalInSec),
                                  rat_healthy_abf$samplingIntervalInSec),
                          rat_healthy_abf$data[[4]])

## GP ####
gp_ventricular <- data.frame(seq.int(0,
                                     ((nrow(gp_ventricular_1Hz_abf$data[[1]])-1)*gp_ventricular_1Hz_abf$samplingIntervalInSec),
                                         gp_ventricular_1Hz_abf$samplingIntervalInSec),
                            gp_ventricular_1Hz_abf$data[[1]])

## hiPSC-CMs ####
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
  theme_clean()+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  ggtitle("Mouse Pacemaker")+
  coord_cartesian(x = c(0, 0.5),
                  y = c(-80, 60))+
  q

## Rat ####
rat_healthy_plot <- 
  ggplot()+
  geom_line(data = rat_healthy,
            aes(x = rat_healthy[,1],
                y = rat_healthy[,2]))+
  theme_clean()+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  ggtitle("Rat Ventricular")+
  coord_cartesian(x = c(0, 0.25),
                  y = c(-80, 60))+
  q

## GP ####
gp_ventricular_plot <- 
  ggplot()+
  geom_line(data = gp_ventricular,
            aes(x = gp_ventricular[,1],
                y = gp_ventricular[,2]))+
  theme_clean()+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  ggtitle("Guinea Pig Ventricular")+
  coord_cartesian(x = c(0, 0.5),
                  y = c(-80, 60))+
  q

## hiPSC-CMs ####
hiPSC_ventricular_mature_plot <- 
  ggplot()+
  geom_line(data = hiPSC_ventricular_mature,
            aes(x = hiPSC_ventricular_mature[,1],
                y = hiPSC_ventricular_mature[,2]))+
  theme_clean()+
  labs(x = "Time (s)",
       y = "Voltage (mV)")+
  ggtitle("Human Ventricular-like iPSC-CMs")+
  coord_cartesian(x = c(0, 0.5),
                  y = c(-80, 60))+
  q

# Combine
combined <- 
  plot_grid(mouse_san_plot,
            rat_healthy_plot,
            gp_ventricular_plot,
            hiPSC_ventricular_mature_plot,
            ncol = 4,
            labels = "AUTO")
# Save Combined Figure
ggsave("../../figures/Figure 1.png", combined, width = 42, height = 8, units = "cm")

