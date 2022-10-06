     #   # provo a calcolare più di un punto di minimo in maniera stabile
      #   Ediast_values <- interval[[3]] # seleziono solo la colonna di valori di voltaggio (questi sono mV)
      #   neg_Ediast <- Ediast_values[which(Ediast_values > -95 & Ediast_values < -10)] # Seleziono i valori dell'intervallo tra -95 e -30 mV. Valori in mV
      #   #Ediast_quantile <- 0.02# default level is 0.025 (Ediast values in the lower 2.5%; the lower the quantile, the more stringent it becomes)
      #   Ediast_quantile <- quantile(interval$Voltage, 0.2)
      #   most_neg_Ediast_index <- which(Ediast_values <= Ediast_quantile) #  indice degli AP con un Ediast nell'XX% dei valori più negativi. INDICE
      #   most_neg_Ediast_values <- neg_Ediast[most_neg_Ediast_index] # These are mV values
      #   
      #   while (length(most_neg_Ediast_index) < sweeps & Ediast_quantile <= 1) {
      #     Ediast_quantile <- Ediast_quantile + 0.02
      #     
      #     most_neg_Ediast_index <- which(Ediast_values <= quantile(interval$Voltage, 0.2)) #  indice degli AP con un Ediast nell'XX% dei valori più negativi. INDICE  # extracting the index fo APS with ad Ediast in the XX% of most negative values
      #     most_neg_Ediast_values <- neg_Ediast[most_neg_Ediast_index] # These are mV values
      #     low_variability_Ediast <-  which(c(abs(diff(most_neg_Ediast_values, 
      #                                                 lag=1, 
      #                                                 differences = 2))) == min(c(abs(diff(most_neg_Ediast_values, 
      #                                                                                      lag=1, 
      #                                                                                      differences = 2))))) # extracting the points with the minimal difference between one Ediast and two subsequent Ediasts 
      #     most_neg_Ediast_index <- most_neg_Ediast_index[low_variability_Ediast] # chose among the APs with low diast, those with a low variation among consecutive APs.
      #   }
      # 
      #   extended_neg_Ediast <- vector() # identifying `n` groups of `sweeps` elements among Ediasts
      #   for (e in 1:(length(most_neg_Ediast_index)-(sweeps-1))){ # identifying `n` groups of `sweeps` elements among Ediasts
      #     n_consecutive_values <- seq(from = most_neg_Ediast_index[[e]], to = (most_neg_Ediast_index[[e]] + (sweeps - 1) ), by = 1)
      #     extended_neg_Ediast <- append(extended_neg_Ediast, n_consecutive_values)
      #   }
      # 
      #   diff_Ediast <- vector()
      #   for (a in seq(from = 1, to = length(extended_neg_Ediast), by = sweeps)){ # defining the starting element of each group of `sweeps`
      #     group <- seq(from = a, to = a + (sweeps-1)) # group of n elements (where n = sweeps) 
      #     difference <- sum(abs(diff(Ediast_values[extended_neg_Ediast[group]]))) # moving difference __within__ each group. This is a *VALUE*
      #     diff_Ediast <- append(diff_Ediast, difference) # combining all the groups in a loop. These are *VALUES*
      #   }  
      # 
      #   min_diff_Ediast <- which(diff_Ediast %in% min(na.omit(diff_Ediast))) #identifying the index of the group with the min() inter-group APD difference. This is an *INDEX*
      #   min_diff_Ediast <- tail(min_diff_Ediast, 1) # If some min_diff_APD are equal, the last index is chosen. This is an INDEX
      #   #filtered_by_Ediast <- extended_neg_Ediast[(((min_diff_Ediast)):(min_diff_Ediast+sweeps))]
      #   filtered_by_Ediast <- extended_neg_Ediast[(((min_diff_Ediast * sweeps) - (sweeps - 1)):(min_diff_Ediast * sweeps))]
      #   
      #   Ediast_SS_temp <- data.frame()
      #   Ediast_SS_temp <- interval[c(filtered_by_Ediast),] # this extract the values of the APD from the main vector.
      #   Ediast_SS <- rbind(Ediast_SS, Ediast_SS_temp)
      #   
      #   # Questa parte sotto taglia ogni AP da Ediast1 a Ediast2 
      #   Ediast_list <- rbind(Ediast_list, Ediast_SS_temp[1,])
      #   }
      # } else {
      #   next
      # }