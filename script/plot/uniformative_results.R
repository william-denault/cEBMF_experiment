load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_0.RData")
tres <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_01.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_02.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_03.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_03.RData")
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
df_simu <- as.data.frame(df_simu)
df_simu$Method <- as.factor(df_simu$Method)

df_simu <- df_simu[-which(df_simu$Method=="cEBMF0"),]
#df_simu$Method[which(df_simu$Method=="cEBMF0")] <- "cEBMF"

library(dplyr)

# Change the entries of df_simu$Method
df_simu <- df_simu %>%
  mutate(Method = case_when(
    Method == "SVD" ~ "PMD",
    Method == "SSVD" ~ "SVD",
    Method == "PMD" ~ "SSVD",
    TRUE ~ Method
  ))
ml_res_denoising_results <- read.csv("C:/Document/Serieux/Travail/python_work/cEBMF_additional_simulation_VAE/uniformative_denoising_results.csv")




ml_res_denoising_results2 <- read.csv("C:/Document/Serieux/Travail/python_work/cEBMF_additional_simulation_VAE/uniformative_denoising_results2.csv")
ml_res_denoising_results=rbind(ml_res_denoising_results,ml_res_denoising_results2)


tdf=data.frame(Method=
                 rep(c("VAE","cVAE","NCF"),
                     each =nrow(ml_res_denoising_results)),
               RMSE=c(ml_res_denoising_results[,1],
                      ml_res_denoising_results[,2],
                      ml_res_denoising_results[,3]),
               N=rep(2000,nrow(ml_res_denoising_results)),
               L=rep(100,nrow(ml_res_denoising_results)),
               K=rep(2 ,nrow(ml_res_denoising_results)),
               P1=rep(10,nrow(ml_res_denoising_results)),
               P2=rep(10,nrow(ml_res_denoising_results)),

               beta0=rep(10,nrow(ml_res_denoising_results)),
               beta1=rep(10,nrow(ml_res_denoising_results)),
               noise_level= c(ml_res_denoising_results[,4],
                              ml_res_denoising_results[,4],
                              ml_res_denoising_results[,4]),
               max_iter_cEBMF=rep(50,nrow(ml_res_denoising_results)),
               max_iter_como=rep(10,nrow(ml_res_denoising_results))
               )

c(ml_res_denoising_results[,1],
  ml_res_denoising_results[,2])

df_simu= rbind(df_simu, tdf)




load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_1.RData")
mfaires  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par) ))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_2.RData")
mfaires  <-rbind( mfaires, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par ) )))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_3.RData")
mfaires  <-rbind( mfaires, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par ) )))

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_4.RData")
mfaires  <-rbind( mfaires, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par ) )))

mfaires= cbind( rep("MFAI", nrow(mfaires)),mfaires)
mfaires= data.frame(mfaires)
colnames(mfaires)[1:2]= c("Method","RMSE")

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_1.RData")
CMFes  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par) ))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_2.RData")
CMFes  <-rbind( CMFes, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par ) )))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_3.RData")
CMFes  <-rbind( CMFes, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par ) )))

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_4.RData")
CMFes  <-rbind( CMFes, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse ,  res[[i]]$ par ) )))

CMFes= cbind( rep("CMF", nrow(CMFes)),CMFes)
CMFes= data.frame(CMFes)
colnames(CMFes)[1:2]= c("Method","RMSE")






df_simu= rbind(df_simu,mfaires,CMFes)
df_simu$RMSE=as.numeric(df_simu$RMSE)
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
  ggtitle("Uninformative covariate") +
  scale_y_log10()
P1

ggplot(df_simu , aes(x=Method, y=RMSE, color=Method)) +
  geom_boxplot() +
  facet_wrap(~noise_level,scale='free') +
  scale_y_log10()
table(df_simu$Method, df_simu$noise_level)

ggsave(P1, file="plot/uninfo_simu.pdf",
       width = 45.7,
       height = 40,
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

summary_data$RMSE=summary_data$mean
# Create the plot
P11<-ggplot(summary_data, aes(x = Method, y =RMSE, color=Method)) +
  geom_point() +  # or geom_bar(stat = "identity") for bars representing the mean
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  facet_wrap(~noise_level, scale='free', labeller = as_labeller(function(x) paste("noise sd =", x))) +
  theme_bw() +
  xlab("RMSE")+
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +
  geom_hline(aes(yintercept = hline), linetype = "dashed", color = "black") +
  ggtitle("Uninformative covariate") +
  scale_y_log10()
P11
P11
ggsave(P11, file="plot/uninfo_simu_bar.pdf",
       width = 45.7,
       height = 40,
       units = "cm"
)

