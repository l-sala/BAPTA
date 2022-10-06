# AP Summary Plots

final_scatter_plot_combined <- function(df, variable){
  
  ggplot(data = df, aes(x = .data[[tail(Conditions,1)]],
                        y = value,
                        key = `File Name`,
                        fill = .data[[Conditions[length(Conditions)-1]]]))+
    #geom_violin(alpha = 0.15, position = position_dodge(width = 1))+
    geom_jitter(size = 2, aes(colour = .data[[Conditions[length(Conditions)-1]]]), alpha = 0.5, position = position_dodge(width=1))+
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, position = position_dodge(width = 1))+
    stat_summary(fun = "mean", geom = "point", size = 4, colour = "black", position = position_dodge(width = 1), pch = 21)+
    scale_x_discrete(limits = as.vector(unique(.data[[Conditions[length(Conditions)-1]]])))+
    labs(x = "", y = variable)+
    ggtitle(variable)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.key = element_blank(), strip.background = element_rect(colour = "black", fill="white", size = 0.5),
          legend.title = element_blank())

}

final_scatter_plot_split <- function(df, variable){

    ggplot(data = df, aes(x = .data[[tail(Conditions,1)]],
                        y = value,
                        fill = .data[[Conditions[length(Conditions)-1]]]))+
    #geom_violin(alpha = 0.15, position = position_dodge(width = 1))+
    geom_jitter(size = 2, aes(colour = .data[[Conditions[length(Conditions)-1]]]), alpha = 0.5, position = position_dodge(width=1))+
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, position = position_dodge(width = 1))+
    stat_summary(fun = "mean", geom = "point", size = 4, colour = "black", position = position_dodge(width = 1), pch = 21)+
    scale_x_discrete(limits = as.vector(unique(.data[[Conditions[length(Conditions)-1]]])))+
    facet_grid(~.data[[Conditions[1]]])+ #default is 2
    labs(x = "", y = variable)+
    ggtitle(variable)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.key = element_blank(), strip.background = element_rect(colour = "black", fill="white", size = 0.5),
          legend.title = element_blank())
}


