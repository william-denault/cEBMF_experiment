library(Matrix)
library(ggplot2)
library(fclust)
library(scatterpie)
library(gridExtra)
library(NNLM)
library(flashier)
library(softImpute)
library(SpatialPCA)

# There are 12 different samples.
i = 9

# Here we take the 9th sample as example, in total there are 12
# samples (numbered as 1-12), the user can test on other samples if
# needed.
sample_names=c("151507", "151508", "151509", "151510", "151669",
  "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")

# each sample has different ground truth cluster number
clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7)

datadir <- "/project2/mstephens/DLPFC/data/"
load(paste0(datadir,"/DLPFC/LIBD_sample",i,".RData"))
load(paste0(datadir,"/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))
tt = as.matrix(count_sub)
truth = KRM_manual_layers_sub$layer_guess_reordered[
  match(colnames(LIBD@normalized_expr),colnames(count_sub))]

# tt0 is the n x m matrix of transformed/normalized counts.
# n = number of cells
# m = number of genes
loc = LIBD@location
tt0 = t(as.matrix(LIBD@normalized_expr))
X = loc
#define comoR object

l2_reg = 0.2
# Y <- t(t(tt0) - apply(tt0,2,min))

# NMF and flashier.
set.seed(1)
fit_nmf <- nnmf(tt0,k = clusterNum[i],method = "scd",loss = "mse",
                verbose = 0,n.threads = 4,rel.tol = 1e-8,max.iter = 100)
fit_nmf_init <- nnmf(Y,k = clusterNum[i],method = "scd",loss = "mse",
                     verbose = 0,n.threads = 2,rel.tol = 1e-8,max.iter = 10)
fit_flash <- flash_init(Y, var_type = 2, S=0.01)
fit_flash <- flash_factors_init(fit_flash,
                                list(fit_nmf_init$W, t(fit_nmf_init$H)),
                                ebnm_fn = c(ebnm_point_exponential, ebnm_point_exponential))
fit_flash <- flash_backfit(fit_flash)
fit_default<-fit_flash






fit_nmf$W<- fit_nmf$W+1e-8# avoid plotting problem

W <- fit_nmf$W+1e-8
W <- W / rowSums(W)


n_truth = as.numeric(as.factor(truth))
h=1
for (o in 1:length(unique(n_truth))){
  n_truth[which(n_truth==unique(n_truth)[o])]=h
  h=h+1

}
if (length(which(is.na(n_truth)))>0){
  res_NMF <- Fclust.compare( n_truth[-which(is.na(truth))],
                             W[-which(is.na(truth)),])
}  else{

  res_NMF <- Fclust.compare(n_truth, W)
}




fit_default$L_pm <- fit_default$L_pm+1e-8
fit_default$L_pm <- abs(fit_default$L_pm+1e-8)

W <- fit_default$L_pm+1e-8#avoid plot problem
W <- W / rowSums(W)


prop <- abs(W)

if (length(which(is.na(n_truth)))>0){
  res_ebnm <- Fclust.compare( n_truth[-which(is.na(truth))],
                              prop[-which(is.na(truth)),])
}  else{

  res_ebnm <- Fclust.compare(n_truth,   prop)
}



my_col= c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439" ,"#FF9896", "#FFDC91")

X=loc
#define comoR object









d0 <- data.frame(x=loc[,1], y=loc[,2])
if (clusterNum[i]==7){
  d0$A <- ifelse(truth=="Layer1", 1, 0)
  d0$B <- ifelse(truth=="Layer2", 1, 0)
  d0$C <-  ifelse(truth=="Layer3", 1, 0)
  d0$D <-  ifelse(truth=="Layer4", 1, 0)
  d0$E <-  ifelse(truth=="Layer5", 1, 0)
  d0$F <-  ifelse(truth=="Layer6", 1, 0)
  d0$G <-  ifelse(truth=="WM", 1, 0)
  P0 <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d0, cols=c("A", "B", "C", "D", "E", "F", "G"),
                                   pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
    scale_fill_manual(values =c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439" ,"#FF9896", "#FFDC91"))+
    ggtitle("Human labeled")+theme( axis.text.y=element_blank(),
                                    axis.ticks.y=element_blank(),
                                    axis.text.x=element_blank(),
                                    axis.ticks.x=element_blank(),
                                    legend.position = "none")





  d <- data.frame(x=loc[,1], y=loc[,2])
  tdf =   do.call ( cbind, lapply (1:ncol(fit_default$L_pm), function (i) {
    fit_default$L_pm[,i]
  }))
  colnames(tdf) <- LETTERS[1:ncol(fit_default$L_pm)]
  d <- cbind(d, tdf)
  d <- data.frame(d)

  P2  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(fit_default$L_pm)] ,
                                    pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
    scale_fill_manual(values =my_col[1:ncol(fit_default$L_pm)])+
    ggtitle(paste0("EBNMF" ))+theme( axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     legend.position = "none")

  d <- data.frame(x=loc[,1], y=loc[,2])

  W <- fit_nmf$W
  #W <- W / rowSums(W)

  tdf2 =   do.call ( cbind, lapply (1:ncol(W ), function (i) {
    W[,i]
  }))
  colnames(tdf2) <- LETTERS[1:ncol(W)]
  d <- cbind(d, tdf2)
  d <- data.frame(d)

  P3  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(W)] ,
                                    pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
    scale_fill_manual(values =my_col[1:ncol(W)])+
    ggtitle(paste0("NMF"  ))+theme( axis.text.y=element_blank(),
                                    axis.ticks.y=element_blank(),
                                    axis.text.x=element_blank(),
                                    axis.ticks.x=element_blank(),
                                    legend.position = "none")

}else{
  d0 <- data.frame(x=loc[,1], y=loc[,2])
  d0$A <- ifelse(truth=="Layer1", 1, 0)
  d0$B <- ifelse(truth=="Layer2", 1, 0)
  d0$C <-  ifelse(truth=="Layer3", 1, 0)
  d0$D <-  ifelse(truth=="Layer4", 1, 0)
  d0$E <-  ifelse(truth=="Layer5", 1, 0)
  d0$F <-  ifelse(truth=="Layer6", 1, 0)
  d0$G <-  ifelse(truth=="WM", 1, 0)
  P0 <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d0, cols=c("A", "B", "C", "D", "E", "F", "G"),
                                   pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
    scale_fill_manual(values =c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439" ,"#FF9896", "#FFDC91"))+
    ggtitle("Human labeled")+theme( axis.text.y=element_blank(),
                                    axis.ticks.y=element_blank(),
                                    axis.text.x=element_blank(),
                                    axis.ticks.x=element_blank(),
                                    legend.position = "none")







  d <- data.frame(x=loc[,1], y=loc[,2])
  tdf =   do.call ( cbind, lapply (1:ncol(fit_default$L_pm), function (i) {
    fit_default$L_pm[,i]
  }))
  colnames(tdf) <- LETTERS[1:ncol(fit_default$L_pm)]
  d <- cbind(d, tdf)
  d <- data.frame(d)






  P2  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(fit_default$L_pm)] ,
                                    pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
    scale_fill_manual(values =my_col[1:ncol(fit_default$L_pm)])+
    ggtitle(paste0("EBNMF " )  )+theme( axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank(),
                                        axis.text.x=element_blank(),
                                        axis.ticks.x=element_blank(),
                                        legend.position = "none")

  d <- data.frame(x=loc[,1], y=loc[,2])

  W <- fit_nmf$W
  #W <- W / rowSums(W)

  tdf2 =   do.call ( cbind, lapply (1:ncol(W ), function (i) {
    W[,i]
  }))
  colnames(tdf2) <- LETTERS[1:ncol(W)]
  d <- cbind(d, tdf2)
  d <- data.frame(d)

  P3  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(W)] ,
                                    pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
    scale_fill_manual(values =my_col[1:ncol(W)])+
    ggtitle(paste0("NMF "  ))+theme( axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     legend.position = "none")


}


res <- list( res_ebnm =res_ebnm,
            res_NMF=res_NMF)
P_out <- grid.arrange(P0  ,P2,P3, ncol=3)

ggsave(P_out,file= paste0("/project2/mstephens/DLPFC/plot/plot_slice_",i,".pdf"),
       width =29.7  ,
       height =  10,
       units = "cm"
)
save(res        , file=paste0( "/project2/mstephens/DLPFC/res_ARI_score/res_",i,".RData"))


