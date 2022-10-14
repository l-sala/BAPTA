# ===================================================================
# Title: Plots
#
# Purpose: Code that generates the plots used for AP Batch Analyser
# Author: Luca Sala, PhD
# Date: 2022-06-21
# Version: 0.1
# Revisions: 2022-06-21
#
# ===================================================================

#### Shared plots ####
  ##### APD values plot #####
APD_values_plot <- 
  ggplot(APD_df, aes(x = `Sweep (n)`,
                     y = `APD value (ms)`,
                     colour = APD))+
  geom_point(size = 1)+
  geom_line(aes(group = APD, colour = APD), alpha = 0.5)+
  scale_colour_discrete(guide="none")+
  facet_wrap(~ APD, nrow = 1)+
  labs(x = "Sweeps (n)", 
       y = "APD (ms)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

  ##### SD1 plot ####
BVR_plot <<- ggplot(APD90_SD1, aes(APD90_SD1[,1], 
                                   APD90_SD1[,2]))+
  geom_path(colour = "red", lineend = "square", linetype = 2)+
  geom_point(alpha = 0.5, size = 2)+
  labs(x = expression(paste("APD"["n"], " (ms)")), 
       y = expression(paste("APD"["n+1"], " (ms)")))+
  ggtitle(paste("Poincaré Plot -", file.names[f]))+
  coord_cartesian(x = c(0,(max(APD90_SD1[,2])*1.5)),
                  y = c(0,(max(APD90_SD1[,2])*1.5)))+
  annotate("segment", x = 0, xend = max(APD90_SD1[,2])*1.1, y = 0, yend = max(APD90_SD1[,2])*1.1,
           colour = "black", linetype = 3)+
  scale_colour_discrete()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

if(mode == "Triggered"){
  
#### Triggered Mode ####
##### AP plot and first derivative (Only Triggered Mode) ####
  
if(all(Ediast[[1]] %in% APD90_SS[[1]]) == FALSE){
  AP_plot_data_SS_wide <- 
    data.frame(AP_plot_data[,1], 
               AP[, APD90_SS[[1]]+1]) # takes the ms value of AP_plot_data and attach only the sweep selected in the steady state
} else {
  AP_plot_data_SS_wide <- 
    data.frame(AP_plot_data[,1], 
               AP[, APD90_SS[[1]][which(APD90_SS[,1] %in% Ediast$`Sweep (n)`)]+1])
}
  
AP_plot_data_SS <- 
  reshape2::melt(AP_plot_data_SS_wide, 
                 id = c("AP_plot_data...1.")) # melt the data frame

AP_plot <<- 
  ggplot()+
  geom_line(data = AP_plot_data_SS, aes(x = AP_plot_data...1., 
                                        y = value,
                                        colour = variable))+
  scale_colour_discrete(guide="none")+
  scale_y_continuous(limits=c(-90, 75), breaks=seq(-90,60,30))+
  labs(x = "Time (ms)", 
       y = "Voltage (mV)")+
  geom_point(data = APD90_SS[Ediast$`Sweep (n)`,], aes(x = (`APD value (ms)`+ Peak$`Peak x (ms)`),
                                  y = `APD value (mV)`))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

first_der_AP_SS_wide <- data.frame(AP_plot_data_SS_wide[-1,1])

# the foor loop below creates the first derivative (over time) of each of the columns except the first one (time column).
for (c in c(2:ncol(AP_plot_data_SS_wide))){
  colmn <- data.frame(diff(AP_plot_data_SS_wide[,c])/diff(AP_plot_data_SS_wide[,1]))
  first_der_AP_SS_wide <- data.frame(first_der_AP_SS_wide, colmn)
}
first_der_AP_SS <- reshape2::melt(first_der_AP_SS_wide, id = c("AP_plot_data_SS_wide..1..1.")) # melt the data frame

#To zoom the dv/dt plot vertically so it is possible to see the repolarisation peak,
#I extracted the min dV/dt value after the AP peak. This value is multiplied by 10 and an interval between these 
#two values is selected as coordinate automatically.
first_der_zoom_y <- min(first_der_AP_SS[((1/(first_der_AP_SS[3,1]-first_der_AP_SS[2,1]))*100):200,3])

first_der_AP_plot <<- 
  ggplot(first_der_AP_SS, 
         aes(AP_plot_data_SS_wide..1..1., 
             value,
             colour = variable))+
  geom_line()+
  scale_colour_discrete(guide="none")+
  coord_cartesian(y = c(first_der_zoom_y*10,
                        -first_der_zoom_y*10))+
  labs(x = "Time (ms)", y = "dV/dt (V/s)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))


AP_plot_zoom <<- 
  AP_plot + coord_cartesian(x = c(0,Peak_x+5))

first_der_AP_plot_zoom <<- 
  first_der_AP_plot +  
  geom_point(data = dVdt_max, 
             aes(x = `dV/dt max x (ms)`,
                 y = `dV/dt max y (V/s)`), 
             colour = "black") +
  coord_cartesian(x = c(0,Peak_x+5))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

AP_plots_grid <<- 
  plot_grid(AP_plot, AP_plot_zoom,
            first_der_AP_plot, first_der_AP_plot_zoom,
            ncol = 2,
            rel_widths = c(0.6,0.3,
                           0.6,0.3))

 ##### Steady State Selection Plot (Only Triggered Mode) ####
SS_Selection_plot_AP <<- 
  ggplot(data = APD_df_APD90, 
         aes(x = `Sweep (n)`, 
             y = `APD value (ms)`))+
  geom_point() + 
  geom_line()+
  geom_point(data = APD90n, 
             aes(x = `Sweep (n)`, 
                 y = `APD value (ms)`), 
             colour = "#59CD90")+ #green
  geom_point(data = APD_90_plot, 
             aes(x = `Sweep (n)`, 
                 y = `APD value (ms)`), 
             colour = "#D00000")+ #red
  labs(x = "Sweeps (n)", y = expression(paste("APD"[90], " (ms)")))+
  ggtitle(expression(paste("Steady State Selection Plot ", "APD"[90])))+
  scale_colour_discrete()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

SS_Selection_plot_Ediast <<- 
  ggplot(data = Ediast_df, 
         aes(x = `Sweep (n)`, 
             y = `Ediast (mV)`))+
  geom_point() + 
  geom_line()+
  geom_point(data = Ediast_SD1, 
             aes(x = `Sweep (n)`, 
                 y = `Ediast (mV)`), 
             colour = "#59CD90")+
  geom_point(data = Ediast, 
             aes(x = `Sweep (n)`, 
                 y = `Ediast (mV)`), 
             colour = "#D00000")+
  labs(x = "Sweeps (n)", y = expression(paste("E"[diast], " (mV)")))+
  ggtitle(expression(paste("Steady State Selection Plot ", "E"[diast])))+
  scale_colour_discrete()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

SS_Selection_plots <<- 
  plot_grid(SS_Selection_plot_AP,
            SS_Selection_plot_Ediast,
            ncol = 1)

} else if(mode == "Gap Free"){
  
#### Gap Free Mode ####
  ##### Gap Free plot (only Gap Free mode) ####
  gap_free_plot <-
    ggplot(data = df,
           aes(x = Time,
               y = Voltage))+
    geom_line()+
    geom_point(data = peaks, aes(Time, Voltage), col = "#ff9900")+
    geom_point(data = Ediast_list, aes(x = Time,
                                       y = Voltage), col = "#48BAB8")+
    geom_point(data = APD_df_all, aes(x = `APD Absolute Time (ms)`,
                                      y = `APD value (mV)`), col = "#F55054")+
    annotate("segment", x = peaks$Time, xend = peaks$Time,
             y = mean(valleys$Voltage), yend = peaks$Voltage, linetype = 3, size = 0.5)+
    labs(x = "Time (ms)",
         y = "Voltage (mV)")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  ##### AP Overlaid Plot (only Gap Free mode) ####
  ap_overlaid_plot <<-
    ggplot(combined_APs, aes(x = relative_Time,
                             y = Voltage,
                             group = sweep))+
    labs(x = "Relative Time (ms)", 
         y = "Voltage (mV)")+
    geom_line(alpha = 0.25)+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}
