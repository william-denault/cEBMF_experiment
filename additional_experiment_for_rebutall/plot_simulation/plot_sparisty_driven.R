load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_1.RData")
tres <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_2.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_3.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_4.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_1.RData")
plot(tres[,1],tres[,3])
abline(a=0,b=1)

plot(tres[,1],tres[,2])
abline(a=0,b=1)

tres[,1] <- apply(tres[ ,c(1,2)],1, min)
tlist <- list()
for( i in 1:length(res[[10]]$rmse ) ){
  toremove <- (1:length(res[[10]]$rmse))[-i]
  tt <- tres[,- toremove ]
  tlist[[i]] <- data.frame ( rep(colnames(tres)[i], nrow(tt)), tt)
  colnames(tlist[[i]])[c(1,2)] <- c("Method", "RMSE")
}
df_simu <- do.call(rbind, tlist)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_1.RData")
CMFres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
CMFres <- data.frame ( Method= rep("CMF", nrow(CMFres )), CMFres )
colnames(CMFres)<- colnames(df_simu )
df_simu <- rbind(df_simu, CMFres)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_2.RData")
CMFres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
CMFres <- data.frame ( Method= rep("CMF", nrow(CMFres )), CMFres )
colnames(CMFres)<- colnames(df_simu )
df_simu <- rbind(df_simu, CMFres)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_3.RData")
CMFres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
CMFres <- data.frame ( Method= rep("CMF", nrow(CMFres )), CMFres )
colnames(CMFres)<- colnames(df_simu )
df_simu <- rbind(df_simu, CMFres)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_4.RData")
CMFres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
CMFres <- data.frame ( Method= rep("CMF", nrow(CMFres )), CMFres )
colnames(CMFres)<- colnames(df_simu )
df_simu <- rbind(df_simu, CMFres)





load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_1.RData")
mfairres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
mfairres <- data.frame ( Method= rep("MFAI", nrow(mfairres )), mfairres )
colnames(mfairres)<- colnames(df_simu )
df_simu <- rbind(df_simu, mfairres)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_2.RData")
mfairres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
mfairres <- data.frame ( Method= rep("MFAI", nrow(mfairres )), mfairres )
colnames(mfairres)<- colnames(df_simu )
df_simu <- rbind(df_simu, mfairres)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_3.RData")
mfairres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
mfairres <- data.frame ( Method= rep("MFAI", nrow(mfairres )), mfairres )
colnames(mfairres)<- colnames(df_simu )
df_simu <- rbind(df_simu, mfairres)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_4.RData")
mfairres  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
mfairres <- data.frame ( Method= rep("MFAI", nrow(mfairres )), mfairres )
colnames(mfairres)<- colnames(df_simu )
df_simu <- rbind(df_simu, mfairres)



df_simu <- as.data.frame(df_simu)
df_simu$Method <- as.factor(df_simu$Method)

df_simu <- df_simu[-which(df_simu$Method=="cEBMF"),]
df_simu$Method[which(df_simu$Method=="cEBMF0")] <- "cEBMF"

library(dplyr)

# Change the entries of df_simu$Method
df_simu <- df_simu %>%
  mutate(Method = case_when(
    Method == "SVD" ~ "PMD",
    Method == "SSVD" ~ "SVD",
    Method == "PMD" ~ "SSVD",
    TRUE ~ Method
  ))

# Verify the changes
table(df_simu$Method)

library(ggplot2)
P1<- ggplot(df_simu, aes(x=Method, y=RMSE, color=Method)) +
  geom_boxplot() +
  facet_wrap(~noise_level, scale='free', labeller = as_labeller(function(x) paste("noise sd =", x))) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +
  ggtitle("Sparsity driven covariate") +
  scale_y_log10()
P1

ggsave(P1, file="C:/Document/Serieux/Travail/Data_analysis_and_papers/redo_plot_ICLR/plots/simple_simu.pdf",
       width = 29.7,
       height = 21,
       units = "cm"
)




library(dplyr)
summary_data <- df_simu  %>%
  group_by(Method,noise_level) %>%
  summarise(
    mean = mean(RMSE, na.rm = TRUE),
    se = 1.96*sd(RMSE, na.rm = TRUE) / sqrt(n())
  )


summary_data$hline <- rep( mean(summary_data$mean), nrow(summary_data) )
for ( i in 1:nrow(summary_data) ){

  summary_data$hline[i] <- min ( summary_data$mean[which(summary_data$noise_level==summary_data$noise_level[i]  )])
}


# Create the plot
P11<- ggplot(summary_data, aes(x = Method, y = mean, color=Method)) +
  geom_point() +  # or geom_bar(stat = "identity") for bars representing the mean
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  facet_wrap(~noise_level, scale='free', labeller = as_labeller(function(x) paste("noise sd =", x))) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +
  geom_hline(aes(yintercept = hline), linetype = "dashed", color = "black") +
  ggtitle("Sparsity driven covariate") +
  scale_y_log10()
P11
ggsave(P11, file="C:/Document/Serieux/Travail/Data_analysis_and_papers/redo_plot_ICLR/plots/simple_simu_bar.pdf",
       width = 29.7,
       height = 21,
       units = "cm"
)
