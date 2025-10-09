

colors <- c("#6BAED6", "#74C476", "#FD8D3C", "#F768A1","darkturquoise")



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

df_simu0= df_simu
P11<- ggplot(df_simu0[which(df_simu0$noise_level==0.1 & df_simu0$Method %in% c("cEBMF", "EBMF", "CMF" ,"PMD" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P11
P12<- ggplot(df_simu0[which(df_simu0$noise_level==0.5 & df_simu0$Method %in% c("cEBMF", "EBMF","CMF" , "PMD" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P12
P13<- ggplot(df_simu0[which(df_simu0$noise_level==1 & df_simu0$Method %in% c("cEBMF", "EBMF", "CMF" ,"PMD" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P13

P14<- ggplot(df_simu0[which(df_simu0$noise_level==2 & df_simu0$Method %in% c("cEBMF", "EBMF", "CMF" ,"PMD" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P14












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
library(ggplot2)
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


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_1.RData")

tt <- do.call( rbind, lapply ( 1:length(res) , function(i) c("CMF", res[[i]]$rmse, res[[i]]$ noise_level ) ))
df_CFM   <- as.data.frame(tt)
colnames(df_CFM) <- c("Method", "RMSE", "noise_level")
df_simu <- rbind(df_simu, df_CFM)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_2.RData")

tt <- do.call( rbind, lapply ( 1:length(res) , function(i) c("CMF", res[[i]]$rmse, res[[i]]$ noise_level ) ))
df_CFM   <- as.data.frame(tt)
colnames(df_CFM) <- c("Method", "RMSE", "noise_level")
df_simu <- rbind(df_simu, df_CFM)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_3.RData")

tt <- do.call( rbind, lapply ( 1:length(res) , function(i) c("CMF", res[[i]]$rmse, res[[i]]$ noise_level ) ))
df_CFM   <- as.data.frame(tt)
colnames(df_CFM) <- c("Method", "RMSE", "noise_level")
df_simu <- rbind(df_simu, df_CFM)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_CMF/tiling_4.RData")

tt <- do.call( rbind, lapply ( 1:length(res) , function(i) c("CMF", res[[i]]$rmse, res[[i]]$ noise_level ) ))
df_CFM   <- as.data.frame(tt)
colnames(df_CFM) <- c("Method", "RMSE", "noise_level")
df_simu <- rbind(df_simu, df_CFM)


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
df_simu2=df_simu



P21<- ggplot(df_simu2[which(df_simu2$noise_level==1 & df_simu2$Method %in% c("cEBMF", "EBMF", "PMD","CMF" ,"spaPCA" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P21
P22<- ggplot(df_simu2[which(df_simu2$noise_level==2 & df_simu2$Method %in% c("cEBMF", "EBMF", "PMD","CMF" ,"spaPCA" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P22
P23<- ggplot(df_simu2[which(df_simu2$noise_level==3 & df_simu2$Method %in% c("cEBMF", "EBMF", "PMD", "CMF" ,
                                                                             "spaPCA" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P23








load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_0.RData")
tres <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_01.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_02.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_03.RData")
res  <- do.call( rbind, lapply ( 1 :length(res) , function(i) c(res[[i]]$rmse, res[[i]]$par ) ))
tres <- rbind(tres, res)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/sim/check_cEBMF_03.RData")
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

# Verify the changes
table(df_simu$Method)

df_simu1=df_simu


P31<- ggplot(df_simu1[which(df_simu1$noise_level==0.1 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD", "CMF"  )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P31

P32<- ggplot(df_simu1[which(df_simu1$noise_level==0.5 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD", "CMF"  )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P32

df_simu1=df_simu
P33<- ggplot(df_simu1[which(df_simu1$noise_level==1 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD", "CMF"  )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P33

P34<- ggplot(df_simu1[which(df_simu1$noise_level==2 & df_simu1$Method %in% c("cEBMF", "EBMF", "PMD", "CMF" )),],
             aes(x=Method, y=RMSE, color=Method,   fill=Method)) +
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
  #scale_y_log10() +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors)
P34



library(cowplot)
Pout<- plot_grid(  P14           , P23,

                  nrow = 1 )


Pout
