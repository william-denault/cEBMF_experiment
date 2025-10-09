
### sparisty  ------


desired_order <-c("cEBMF", "EBMF", "MFAI", "PMD", "Spatial PCA", "VAE", "cVAE", "NCF")

standardize_method_order <- function(df) {

  df$Method <- factor(df$Method, levels = desired_order)
  return(df)
}


colors <- c("#6BAED6", "#74C476", "#FD8D3C", "#F768A1")
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_1.RData")
tres <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_2.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_3.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_4.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/check_cEBMF_1.RData")
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



ml_res_denoising_results <- read.csv("C:/Document/Serieux/Travail/python_work/cEBMF_additional_simulation_VAE/uniformative_denoising_results.csv")

ml_res_denoising_results2 <- read.csv("C:/Document/Serieux/Travail/python_work/cEBMF_additional_simulation_VAE/sparse_denoising_results2.csv")
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

df_simu0= df_simu


# Apply to all simulation data used in plots
df_simu0 <- standardize_method_order(df_simu0)
df_simu0= df_simu0[which(  df_simu0$Method %in% c("cEBMF", "EBMF", "PMD" ,"VAE","cVAE","NCF","MFAI")),]
df_simu0$Method= factor(df_simu0$Method, levels=c("cEBMF", "EBMF", "MFAI", "PMD" , "VAE", "cVAE", "NCF"))

P11<- ggplot(df_simu0[which(df_simu0$noise_level==0.1 & df_simu0$Method %in% c("cEBMF", "EBMF", "PMD" ,"VAE","cVAE","NCF","MFAI")),],
             aes(x=Method, y=RMSE )) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  scale_y_log10()+
  ggtitle("Sparsity driven covariate")
P11
P12<- ggplot(df_simu0[which(df_simu0$noise_level==0.5 & df_simu0$Method %in% c("cEBMF", "EBMF", "PMD","VAE","cVAE","NCF","MFAI" )),],
             aes(x=Method, y=RMSE )) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle("Sparsity driven covariate") +
  scale_y_log10()
P12
P13<- ggplot(df_simu0[which(df_simu0$noise_level==1 & df_simu0$Method %in% c("cEBMF", "EBMF", "PMD","VAE","cVAE","NCF","MFAI"  )),],
             aes(x=Method, y=RMSE )) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle("Sparsity driven covariate") +
   scale_y_log10()
P13

P14<- ggplot(df_simu0[which(df_simu0$noise_level==2 & df_simu0$Method %in% c("cEBMF", "EBMF", "PMD","VAE","cVAE","NCF","MFAI"  )),],
             aes(x=Method, y=RMSE )) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle("Sparsity driven covariate") +
   scale_y_log10()
P14




### tiling ------




library(ggplot2)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/tiling_1.RData")
tres <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$ noise_level ) ))

res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/tiling_2.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/tiling_3.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))

res[, ncol(res)] <- 3
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/tiling_4.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)



load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/tiling_4.RData")



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

ml_res_denoising_results <- read.csv("C:/Document/Serieux/Travail/python_work/cEBMF_additional_simulation_VAE/tiling_benchmark_results.csv")

tdf=data.frame(Method=
                 rep(c("VAE","cVAE","NCF"),
                     each =nrow(ml_res_denoising_results)),
               RMSE=c(ml_res_denoising_results[,1],
                      ml_res_denoising_results[,2],
                      ml_res_denoising_results[,3]),

               noise_level= c(ml_res_denoising_results[,4],
                              ml_res_denoising_results[,4],
                              ml_res_denoising_results[,4])

)


df_simu= rbind(df_simu, tdf)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_mfair/tiling_1.RData.RData")
mfaires  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_mfair,  res[[i]]$ noise_level ) ))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_mfair/tiling_2.RData")
mfaires  <-rbind( mfaires, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_mfair,  res[[i]]$ noise_level ) )))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_mfair/tiling_3.RData")
mfaires  <-rbind( mfaires, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_mfair,  res[[i]]$ noise_level ) )))

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_mfair/tiling_4.RData")
mfaires  <-rbind( mfaires, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_mfair,  res[[i]]$ noise_level ) )))


mfaires= data.frame(Method=rep("MFAI",nrow(mfaires)),
                    RMSE=mfaires[,1],
                    noise_level=mfaires[,2] )


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_1.RData")
CMFes  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_CMF,  res[[i]]$ noise_level ) ))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_2.RData")
CMFes  <-rbind( CMFes, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_CMF,  res[[i]]$ noise_level ) )))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_3.RData")
CMFes  <-rbind( CMFes, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_CMF,  res[[i]]$ noise_level ) )))
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_4.RData")
CMFes  <-rbind( CMFes, do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$ rmse_CMF,  res[[i]]$ noise_level ) )))


CMFes= data.frame(Method=rep("CMF",nrow(CMFes)),
                  RMSE=CMFes[,1],
                  noise_level=CMFes[,2] )


df_simu= rbind(df_simu, mfaires,CMFes)
# Verify the changes


df_simu2=df_simu
table(df_simu2$Method)

df_simu2 <- standardize_method_order(df_simu2)
df_simu2 <- df_simu2[which( df_simu2$Method %in% c("cEBMF", "EBMF","MFAI", "PMD","spaPCA","VAE","cVAE" ,"NCF")),]

df_simu2$Method= factor(df_simu2$Method, levels=c("cEBMF", "EBMF","MFAI", "PMD","spaPCA","VAE","cVAE" ,"NCF"))

P21<- ggplot(df_simu2[which(df_simu2$noise_level==1 & df_simu2$Method %in% c("cEBMF", "EBMF", "PMD","spaPCA","VAE","cVAE" ,"NCF","MFAI")),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle("Tiled clustering model") +
  scale_y_log10()
P21
P22<- ggplot(df_simu2[which(df_simu2$noise_level==2 & df_simu2$Method %in% c("cEBMF", "EBMF", "PMD","spaPCA","VAE","cVAE","NCF","MFAI"  )),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle("Tiled clustering model") +
  scale_y_log10()
P22
P23<- ggplot(df_simu2[which(df_simu2$noise_level==3 & df_simu2$Method %in% c("cEBMF", "EBMF", "PMD",
                                                                             "spaPCA","VAE","cVAE","NCF" ,"MFAI" )),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle( "Tiled clustering model") +
   scale_y_log10()
P23

P24<- ggplot(df_simu2[which(df_simu2$noise_level==5 & df_simu2$Method %in% c("cEBMF", "EBMF", "PMD",
                                                                             "spaPCA","VAE","cVAE","NCF" ,"MFAI" )),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle( "Tiled clustering model") +
  scale_y_log10()
P24





## uniformative covar -----
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
df_simu1=df_simu

df_simu1= df_simu1[which(  df_simu1$Method %in% c("cEBMF", "EBMF", "PMD" ,"VAE","cVAE","NCF","MFAI" )),]
df_simu1$Method= factor(df_simu1$Method, levels=c("cEBMF", "EBMF","MFAI", "PMD" ,"VAE","cVAE" ,"NCF"))

P31<- ggplot(df_simu1[which(df_simu1$noise_level==0.1 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD" ,"VAE","cVAE","NCF","MFAI" )),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
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
P31

P32<- ggplot(df_simu1[which(df_simu1$noise_level==0.5 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD","VAE","cVAE","NCF","MFAI"  )),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
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
P32

P33<- ggplot(df_simu1[which(df_simu1$noise_level==1 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD","VAE","cVAE","NCF" ,"MFAI" )),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
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
P33

P34<- ggplot(df_simu1[which(df_simu1$noise_level==2 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD","VAE","cVAE","NCF","MFAI"  )),],
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.4) +
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
P34




#shifted tiled clustering  -----
library(ggplot2)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/shifted_tiled_results/shifted_tiling_1.RData")
tres <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$ noise_level ) ))

res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/shifted_tiled_results/shifted_tiling_2.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse, res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/shifted_tiled_results/shifted_tiling_3.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))


tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/additional_experiment_for_rebutall/shifted_tiled_results/shifted_tiling_4.RData")
res  <- do.call( rbind, lapply ( 1:length(res) , function(i) c(res[[i]]$rmse,  res[[i]]$ noise_level ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/sim/tiling_1.RData")
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
df_simu$Method [which(df_simu$Method=="cEBMF")] <- "cEBMF"
df_simu$Method <- as.factor(df_simu$Method)
df_simu$RMSE <- as.numeric(df_simu$RMSE)

ml_res_denoising_results <- read.csv("C:/Document/Serieux/Travail/python_work/cEBMF_additional_simulation_VAE/shifted_benchmark.csv")

tdf=data.frame(Method=
                 rep(c("VAE","cVAE","NCF"),
                     each =nrow(ml_res_denoising_results)),
               RMSE=c(ml_res_denoising_results[,1],
                      ml_res_denoising_results[,2],
                      ml_res_denoising_results[,3]),

               noise_level= c(ml_res_denoising_results[,4],
                              ml_res_denoising_results[,4],
                              ml_res_denoising_results[,4])

)


df_simu= rbind(df_simu, tdf)
library(dplyr)

# Change the entries of df_simu$Method
#df_simu <- df_simu %>%
#  mutate(Method = case_when(
#    Method == "SVD" ~ "PMD",
#    Method == "SSVD" ~ "SVD",
#    Method == "PMD" ~ "SSVD",
#    TRUE ~ Method
#  ))

# Verify the changes
table(df_simu$Method)

df_simu4=df_simu


df_simu4= df_simu4[which(  df_simu4$Method %in% c("cEBMF", "EBMF", "PMD" ,"VAE","cVAE","NCF","MFAI" ,"spaPCA")),]
df_simu4$Method= factor(df_simu4$Method, levels=c("cEBMF", "EBMF","MFAI", "PMD","spaPCA" ,"VAE","cVAE" ,"NCF"))

P41<- ggplot(df_simu4[which(df_simu4$noise_level==1 & df_simu4$Method %in% c("cEBMF", "EBMF", "PMD","spaPCA","VAE","cVAE" ,"NCF","MFAI")),]
,
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle("Shifted tiled clustering") +
  scale_y_log10()
P41
P42<- ggplot(df_simu4[which(df_simu4$noise_level==2 & df_simu4$Method %in% c("cEBMF", "EBMF", "PMD","spaPCA","VAE","cVAE" ,"NCF","MFAI")),]
,
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle("Shifted tiled clustering") +
  scale_y_log10()
P42
P43<- ggplot(df_simu4[which(df_simu4$noise_level==3 & df_simu4$Method %in% c("cEBMF", "EBMF", "PMD","spaPCA","VAE","cVAE" ,"NCF","MFAI")),]
,
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle( "Shifted tiled clustering") +
  scale_y_log10()
P43

P44<- ggplot(df_simu4[which(df_simu4$noise_level==5 & df_simu4$Method %in% c("cEBMF", "EBMF", "PMD","spaPCA","VAE","cVAE" ,"NCF","MFAI")),]
,
             aes(x=Method, y=RMSE)) +
  geom_boxplot(alpha=0.3) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20), # Apply to plot title
        axis.title = element_text(size = 20), # Apply to both axis titles
        strip.text = element_text(size = 20),
        axis.text.x = element_text(size = 15), # X axis tick marks
        axis.text.y = element_text(size = 15),# Apply to facet strip text
        strip.background = element_rect(fill = "white")) +
  xlab("") +ylab("") +
  ggtitle( "Shifted tiled clustering") +
  scale_y_log10()
P44



library(cowplot)
Pout<- plot_grid( P33,P14
                  , P23,P43,

                  nrow = 1 )


Pout

ggsave(Pout, file="plot/sims_main.pdf",
       width = 65.7,
       height =17,
       units = "cm")

