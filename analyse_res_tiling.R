library(ggplot2)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/tiling_1.RData")
tres <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,res[[i]]$ARI, res[[i]]$ noise_level ) ))

res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,res[[i]]$ARI, res[[i]]$ noise_level ) ))

load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/tiling_2.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,res[[i]]$ARI, res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/tiling_3.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$ARI,res[[i]]$ noise_level ) ))

res[, ncol(res)] <- 3
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/tiling_4.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,res[[i]]$ARI, res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/tiling_1.RData")

tlist <- list ()

for( i in 1:length(res[[1]]$rmse) ){
  toremove <-c( (1:length(res[[1]]$rmse))[-i], (1:length(res[[1]]$ARI))[-i]+ length(res[[1]]$rmse))
  tt <- tres[,- toremove ]
  tlist[[i]] <- cbind ( rep(colnames(tres)[i], nrow(tt)), tt  )
  colnames(tlist[[i]])[c(1,2,3,4)] <- c("Method", "RMSE", "ARI", "noise_level")
}


df_simu <- do.call(rbind, tlist)
df_simu <- as.data.frame(df_simu)
df_simu$Method <- as.factor(df_simu$Method)
df_simu$RMSE <- as.numeric(df_simu$RMSE)
df_simu$ARI <- as.numeric(df_simu$ARI)
library(ggplot2)
ggplot(df_simu[-which(df_simu$Method=="cEBMF0"),], aes(x=Method, y=RMSE, color=Method)) +
  geom_boxplot()  +

#  scale_y_log10()+
  facet_wrap(~noise_level ,scale="free",
             labeller = as_labeller(function(x) paste("noise sd =", x)))+
  theme_bw()+
  theme(legend.position="none",
        strip.background = element_rect(fill = "white"))+
  xlab("")
