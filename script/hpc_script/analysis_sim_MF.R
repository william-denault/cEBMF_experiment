rm(list=ls())
#### Noise level 0.5 ------
load("~/simu_cEBMF/sim/check_cEBMF_1.RData")

df_simu <- as.data.frame( do.call(rbind, lapply(1:length(res), function(i) res[[i]]$rmse)))
head(df_simu)
df_final  <- data.frame( rmse=  c(df_simu$rmse_cEBMF_susie, df_simu$rmse_flash),
                         noise_level=   rep(  0.5,nrow(df_simu)),
                         type= factor(  rep( c("cEBMF","flash"), each=nrow(df_simu)))
)




#### Noise level 1 ------
load("~/simu_cEBMF/sim/check_cEBMF_2.RData")

df_simu <-  as.data.frame( do.call(rbind, lapply(1:length(res), function(i) res[[i]]$rmse)))
head(df_simu)
tdf <- data.frame( rmse=  c(df_simu$rmse_cEBMF_susie, df_simu$rmse_flash),
                   noise_level=   rep(  1,nrow(df_simu)),
                   type= factor(  rep( c("cEBMF","flash"), each=nrow(df_simu)))
)

df_final<- rbind(df_final, tdf)

#### Noise level 3 ------
load("~/simu_cEBMF/sim/check_cEBMF_3.RData")

df_simu <-  as.data.frame( do.call(rbind, lapply(1:length(res), function(i) res[[i]]$rmse)))
head(df_simu)
tdf <- data.frame( rmse=  c(df_simu$rmse_cEBMF_susie, df_simu$rmse_flash),
                   noise_level=   rep(  2,nrow(df_simu)),
                   type= factor(  rep( c("cEBMF","flash"), each=nrow(df_simu)))
)

df_final<- rbind(df_final, tdf)


#### Noise level 3 ------
load("~/simu_cEBMF/sim/check_cEBMF_4.RData")

df_simu <-  as.data.frame( do.call(rbind, lapply(1:length(res), function(i) res[[i]]$rmse)))
head(df_simu)
tdf <- data.frame( rmse=  c(df_simu$rmse_cEBMF_susie, df_simu$rmse_flash),
                   noise_level=   rep(  4,nrow(df_simu)),
                   type= factor(  rep( c("cEBMF","flash"), each=nrow(df_simu)))
)

df_final<- rbind(df_final, tdf)

library(ggplot2)
ggplot( df_final, aes(x= factor( noise_level), y= rmse, col=type))+
  geom_boxplot()+
  geom_abline(intercept = 0,slope=0)+
  #ylim(c(-0.01,0.4))+
  xlab("Noise level sd")

