library(Matrix)
library(reshape2)
library(ggplot2)
library(cowplot)
library(fclust)
library(scatterpie)
library(gridExtra)
library(NNLM)
library(flashier)
library(softImpute)
library(SpatialPCA)

# These colors are from colorbrewer2.org.
factor_colors5 <- c("#d95f02","#386cb0","#e7298a","#66a61e","#ffff99")
factor_colors7 <- c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854",
                    "#ffd92f","#e5c494")

# Maps the estimated memberships onto the (x,y) coordinates using the
# "scatterpie" package.
plot_memberships_on_slice <- function (W, xy, title = "", min_prop = 0.05,
                                       pie_scale = 0.35, font_size = 12,
                                       colors = factor_colors5) {
  W <- W / rowSums(W)
  W[W < min_prop] <- 0
  W <- W / rowSums(W)
  k <- ncol(W)
  ks <- paste0("k",1:k)
  colnames(W) <- ks
  dat <- cbind(xy,W)
  dat <- as.data.frame(dat,stringsAsFactors = FALSE)
  return(ggplot(dat,aes(x = x,y = y)) +
         geom_scatterpie(data = dat,cols = ks,pie_scale = pie_scale,
                         color = NA) +
         coord_fixed() +
         scale_fill_manual(values = colors) +
         theme_cowplot(font_size = font_size) +
         theme(panel.background = element_rect(fill = "black",color = NA),
               plot.title = element_text(face = "plain",size = font_size)) +
         labs(x = "",y = "",fill = "k",title = title))
}

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
truth <- KRM_manual_layers_sub$layer_guess_reordered[
  match(colnames(LIBD@normalized_expr),colnames(count_sub))]

# X is the n x m matrix of transformed/normalized counts.
# n = number of cells
# m = number of genes
loc <- LIBD@location
colnames(loc) <- c("x","y")
X <- t(as.matrix(LIBD@normalized_expr))
X <- X - min(X) 

l2_reg = 0.2

# Get the human labels as factors (W_true).
truth <- as.character(truth)
truth[is.na(truth)] <- "NA"
truth <- factor(truth,levels = c(paste0("Layer",1:6),"WM","NA"))
W_true <- model.matrix(~0 + x,data.frame(x = truth))
Crownames(W_true) <- rownames(X)
colnames(W_true) <- levels(truth)

# NMF and flashier.
set.seed(1)
k   <- 5
n   <- nrow(X)
m   <- ncol(X)
fl0 <- flash(X,greedy_Kmax = 1,ebnm_fn = ebnm_point_exponential,
             S = 0.01,var_type = 2)
W0  <- cbind(fl0$L_pm,matrix(runif(n*k),n,k))
H0  <- t(cbind(fl0$F_pm,matrix(runif(m*k),m,k)))
nmf0 <- nnmf(X,k = k + 1,method = "scd",loss = "mse",n.threads = 4,
             init = list(W = W0,H = H0),max.iter = 4,rel.tol = 1e-8,
             verbose = 2)
nmf <- nnmf(X,k = k + 1,method = "scd",loss = "mse",n.threads = 4,
            init = list(W = W0,H = H0),max.iter = 50,rel.tol = 1e-8,
            verbose = 2)
fl <- flash_init(X,S = 0.01,var_type = 2)
fl <- flash_factors_init(fl,
                         list(nmf0$W,t(nmf0$H)),
                         ebnm_point_exponential)
fl <- flash_backfit(fl,maxiter = 100,verbose = 3)
L  <- ldf(fl,type = "i")$L

# Plot the results.
p1 <- plot_memberships_on_slice(W_true,loc,title = "human labeled",
                                colors = c(factor_colors7,"black"))
p2 <- plot_memberships_on_slice(nmf$W[,-1],loc,title = "NMF")
p3 <- plot_memberships_on_slice(L[,-1],loc,title = "EBNMF")
ggsave("plots.pdf",
       plot_grid(p1,p2,p3,nrow = 2,ncol = 2),
       height = 8,width = 8)

# ************************************************************************
#
#                            END OF MAIN ANALYSIS
#
# ************************************************************************

stop()

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

  P3  <- ggplot() +
    geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(W)] ,
                    pie_scale=0.35, color=NA) +
                      coord_fixed() +
                        theme_minimal()  +
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


