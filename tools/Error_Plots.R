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






