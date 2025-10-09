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
                 plot.title = element_text(face = "plain",size = font_size) , axis.text.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 legend.position = "none") +
           labs( title = title))
}

# There are 12 different samples.

for ( i in c(1:12)){
  # Here we take the 9th sample as example, in total there are 12
  # samples (numbered as 1-12), the user can test on other samples if
  # needed.
  sample_names=c("151507", "151508", "151509", "151510", "151669",
                 "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")

  # each sample has different ground truth cluster number
  clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7)

  load( paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
  load(paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))

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
  rownames(W_true) <- rownames(X)
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

  # Extract the NMF membership matrix,
  fl_nmf <- flash_init(X,S = 0.01,var_type = 2)
  fl_nmf <- flash_factors_init(fl_nmf,
                               list(nmf$W,t(nmf$H)),
                               ebnm_point_exponential)


  save(fl, file=paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_ebmf/fit_default_",i,".RData"))
  save(fl_nmf    , file=paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_nmf/fit_nmf_"     ,i,".RData"))

}
# ************************************************************************
#
#                            END OF MAIN ANALYSIS
#
# ************************************************************************

stop()
