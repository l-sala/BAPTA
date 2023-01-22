# ===================================================================
# Title: Error Plots
#
# Purpose: Code that generates the plots used for AP Batch Analyser
# Author: Luca Sala, PhD
# Date: 2022-06-21
# Version: 0.1
# Revisions: 2022-06-21
#
# ===================================================================

if(mode == "Triggered") {
  
  AP_plot_data <- reshape2::melt(AP, 
                                 id = names(AP[1])) # melt the data frame
  colnames(AP_plot_data) <- c("time", "variable", "value")
  error_plot <-
    ggplot()+
    geom_line(data = AP_plot_data, 
              aes(x = time,
                  y = value,
                  colour = variable))+
    scale_colour_discrete(guide = "none")+
    scale_y_continuous(limits=c(-90, 75), breaks=seq(-90, 60, 30))+
    labs(x = "Time (ms)", y = "Voltage (mV)")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

} else if(mode == "Gap Free"){
##### Gap Error Free plot (only Gap Free mode) ####
  gap_free_error_plot <-
    ggplot(data = df, 
           aes(x = Time,
               y = Voltage))+
    geom_line()+
    ylim(-100, 100)+
    geom_line(aes(y = minpeakheight+2), col = "#F55054")+
    annotate(geom = "text", y=minpeakheight + 6, x = max(df$Time)/5, label = "Min peaks hight", col="#F55054")+
    labs(x = "Time (ms)", y = "Voltage (mV)")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}



  

