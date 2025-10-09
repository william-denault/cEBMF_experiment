rm(list=ls())

load( "C:/Document/Serieux/Travail/Data_analysis_and_papers/Neurips2025_experiments/ml_results.Rdata")
ML_results <- read.csv("C:/Document/Serieux/Travail/python_work/cEBMF_additional_simulation_VAE/vae_cvae_ncf_results/benchmark_results.csv")
library(dplyr)
x <- rep(ML_results[,2], 3)

# Convert to character labels
training_labels <- recode(x,
                          `0.5` = "50%",
                          `0.7` = "70%",
                          `0.9` = "90%"
)
df= data.frame(method= rep(c("VAE","cVAE","NCF"), each =nrow(ML_results)),
              RMSE=c(ML_results[,3],ML_results[,4],ML_results[,5]),
              training_ratio= training_labels)
 data_final= rbind( df, data)

 # Factor levels for ordered display
 data_final$method <- factor( data_final$method, levels = c("cEBNMF", "EBNMF", "MFAI", "VAE","cVAE","NCF"))
 data_final$training_ratio <- factor( data_final$training_ratio, levels = c("50%", "70%", "90%"))
 library(ggplot2)
 # Plot
 P_out= ggplot( data_final, aes(x = method, y = RMSE)) +
   geom_boxplot(outlier.size = 1) +
   facet_wrap(~ training_ratio, nrow = 1) +
   labs(y = "RMSE", x = NULL) +
   #scale_y_continuous(limits = c(0.79, 0.825)) +
   theme_bw(base_size = 14) +
   #scale_y_log10()+
   theme(
     strip.text = element_text(size = 14),
     axis.text.x = element_text(size = 12),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     strip.background = element_rect(fill = "white")
   )


 ggsave(P_out, file="plot/movie_lens.pdf",
        width = 36.7,
        height =9,
        units = "cm")
