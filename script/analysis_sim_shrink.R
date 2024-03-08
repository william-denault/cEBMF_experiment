rm(list=ls())
#### Noise level 0.5 ------
load("~/simu_cEBMF/sim/check_shrinkage_1.RData")

df_simu <- as.data.frame( do.call(rbind, res))
head(df_simu)
df_final  <- data.frame( rmse=  c(df_simu$rmse_mco, df_simu$rmse_ash),
                   noise_level=   rep( df_simu$noise_level,2),
               type= factor(  rep( c("mococomo","ash"), each=nrow(df_simu)))
               )




#### Noise level 1 ------
load("~/simu_cEBMF/sim/check_shrinkage_2.RData")

df_simu <- as.data.frame( do.call(rbind, res))
head(df_simu)
tdf <- data.frame( rmse=  c(df_simu$rmse_mco, df_simu$rmse_ash),
                         noise_level=   rep( df_simu$noise_level,2),
                         type= factor(  rep( c("mococomo","ash"), each=nrow(df_simu)))
)

df_final<- rbind(df_final, tdf)

#### Noise level 2 ------
load("~/simu_cEBMF/sim/check_shrinkage_3.RData")

df_simu <- as.data.frame( do.call(rbind, res))
head(df_simu)
tdf <- data.frame( rmse=  c(df_simu$rmse_mco, df_simu$rmse_ash),
                   noise_level=   rep( df_simu$noise_level,2),
                   type= factor(  rep( c("mococomo","ash"), each=nrow(df_simu)))
)

df_final<- rbind(df_final, tdf)

#### Noise level 3 ------
load("~/simu_cEBMF/sim/check_shrinkage_4.RData")

df_simu <- as.data.frame( do.call(rbind, res))
head(df_simu)
tdf <- data.frame( rmse=  c(df_simu$rmse_mco, df_simu$rmse_ash),
                   noise_level=   rep( df_simu$noise_level,2),
                   type= factor(  rep( c("mococomo","ash"), each=nrow(df_simu)))
)

df_final<- rbind(df_final, tdf)


library(ggplot2)
ggplot( df_final, aes(x= factor( noise_level), y= rmse, col=type))+
  geom_boxplot()+
  xlab("Noise level (sd)")
