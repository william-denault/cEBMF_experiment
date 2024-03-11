 load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_experiment/sim/local_res/check_cEBMF_1.RData")
tres <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_experiment/sim/local_res/check_cEBMF_2.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_experiment/sim/local_res/check_cEBMF_1.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_experiment/sim/local_res/check_cEBMF_2.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/check_cEBMF_1.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/check_cEBMF_2.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/check_cEBMF_3.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/check_cEBMF_4.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Package/comoR/sim/local_res/check_cEBMF_1.RData")

tlist <- list ()

for( i in 1:length(res[[1]]$rmse) ){
    toremove <- (1:length(res[[1]]$rmse))[-i]
    tt <- tres[,- toremove ]
    tlist[[i]] <- data.frame ( rep(colnames(tres)[i], nrow(tt)), tt)
    colnames(tlist[[i]])[c(1,2)] <- c("Method", "RMSE")
}


df_simu <- do.call(rbind, tlist)
df_simu <- as.data.frame(df_simu)
df_simu$Method <- as.factor(df_simu$Method)


library(ggplot2)
ggplot(df_simu, aes(x=Method, y=RMSE, color=Method)) +
  geom_boxplot() +
  facet_wrap(~noise_level, scales="free") +
  scale_y_log10()
