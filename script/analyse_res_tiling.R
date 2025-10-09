library(ggplot2)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/tiling_1.RData")
tres <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$ noise_level ) ))

res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/tiling_2.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/tiling_3.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))

res[, ncol(res)] <- 3
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/tiling_4.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/tiling_1.RData")
colnames(tres)[8] <- "noise_level"
tlist <- list ()
h=1
for ( i in 1:nrow(tres)){
  for (j in 1:length(res[[1]]$rmse)){
    tlist[[h]]<- c( colnames(tres)[j], tres[ i ,j], tres [i,8])
    h=h+1
  }
}


df_simu <- do.call(rbind, tlist)
df_simu <- as.data.frame(df_simu)
colnames(df_simu) <- c("Method", "RMSE", "noise_level")
df_simu$Method [which(df_simu$Method=="Spatial PCA")] <- "spaPCA"
df_simu$Method <- as.factor(df_simu$Method)
df_simu$RMSE <- as.numeric(df_simu$RMSE)
library(ggplot2)
P1 <- ggplot(df_simu[-which(df_simu$Method=="cEBMF0"),], aes(x=Method, y=RMSE, color=Method)) +
  geom_boxplot()  +

#  scale_y_log10()+
  facet_wrap(~noise_level ,scale="free",
             labeller = as_labeller(function(x) paste("noise sd =", x)))+
  theme_bw()+

  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +

  xlab("")+
  ggtitle("Tiled clustering model")+
  scale_y_log10()
P1
 ggsave(P1, file="plot/tiling_simu.pdf",
        width = 29.7,
        height = 21,
        units = "cm"
 )

table(df_simu$Method, df_simu$noise_level)






library(dplyr)
summary_data <- df_simu[-which(df_simu$Method=="cEBMF0"),]  %>%
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
P11<-ggplot(summary_data, aes(x = Method, y = mean, color=Method)) +
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
  geom_hline(aes(yintercept = hline), linetype = "dashed", color = "black") +
  xlab("")+
  ggtitle("Tiled clustering model")+
  scale_y_log10()
P11
ggsave(P11, file="plot/tiling_simu_bar.pdf",
       width = 29.7,
       height = 21,
       units = "cm"
)
