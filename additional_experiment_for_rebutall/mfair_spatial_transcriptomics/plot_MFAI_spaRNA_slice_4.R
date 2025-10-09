




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
                 #legend.position = "none",
                 axis.ticks.x=element_blank()) +
           labs( title = title))
}

# There are 12 different samples.

i=4
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



load(paste( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_ebmf/fit_default_",i,".RData", sep=""))

load(paste( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_nmf/fit_nmf_",i,".RData", sep=""))

res <- ldf(fl_nmf,type = "1")
W   <- with(res,L %*% diag(D))

# Extract the flashier membership matrix.
res <- ldf(fl,type = "1")
L   <- with(res,L %*% diag(D))


load(paste( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData", sep=""))

#adpating script from flashier for LDF transform
loadings <- cEBMF.obj$loading
norms <- lapply(1:ncol(loadings), function(j) {sum(abs(loadings[,j]))})

norms<- do.call(c, norms)
# Zero factors are "normalized" to zero.
if( length(which( norms < 1e-30))>0 ){
  norms[which(norms < 1e-30)] <- Inf
}



cL <- loadings
for (j in 1:ncol(cL)) {
  cL[, j] <- cL[, j] / norms[j]
}

if(length(is.infinite(norms))>0){
  norms[which(is.infinite(norms))] <- 0
}

cL<- cL %*%diag(norms)

if( ncol(cL)==7){
  cL<- cL[,-7]
}
# Plot the results.
p1 <- plot_memberships_on_slice(W_true,loc,title = "human labeled",
                                colors = c(factor_colors7, "black" ))


p2 <- plot_memberships_on_slice(cL    ,loc,title = "cEBNMF" ,
                                colors = factor_colors7)

p3 <- plot_memberships_on_slice(L[,-1],loc,title = "EBNMF",
                                colors = factor_colors7)
p5 <- plot_memberships_on_slice(W[,-1],loc,title = "NMF",
                                colors = factor_colors7)




spec_cluster <- kmeans(t( LIBD@SpatialPCs ),centers = clusterNum[i], nstart = 100)
clusterlabel= as.numeric(walktrap_clustering(clusternum=clusterNum[i], (LIBD@SpatialPCs),knearest=70 ))
#clusterlabel_refine =as.numeric( refine_cluster_10x(clusterlabels=clusterlabel,
#                                                   location=LIBD@location,shape="hexagon"))

m <- matrix(0, nrow = length(clusterlabel), ncol = clusterNum[i])

# Fill the matrix
for (o in 1:clusterNum[i]) {
  m[, o] <- ifelse(spec_cluster$cluster  == o, 1, 0)
  #m[, o] <- ifelse(clusterlabel  == o, 1, 0)
}


p4 <- plot_memberships_on_slice(m,loc,title = "Spatial PCA + clustering",
                                colors = c(factor_colors7  ))


load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/mfair_spatial_transcriptomics_results/run_spatial_DLPFC_4.RData")


Z=res@Z
spec_cluster <- kmeans(Z,centers = clusterNum[i], nstart = 100)
clusterlabel= as.numeric(walktrap_clustering(clusternum=clusterNum[i], t(Z),knearest=70 ))
#clusterlabel_refine =as.numeric( refine_cluster_10x(clusterlabels=clusterlabel,
#                                                   location=LIBD@location,shape="hexagon"))

m <- matrix(0, nrow = length(clusterlabel), ncol = clusterNum[i])

# Fill the matrix
for (o in 1:clusterNum[i]) {
  m[, o] <- ifelse(spec_cluster$cluster  == o, 1, 0)
  #m[, o] <- ifelse(clusterlabel  == o, 1, 0)
}


p6 <- plot_memberships_on_slice(m,loc,title = "MFAI + clustering",
                                colors = c(factor_colors7  ))



  ggsave( plot_grid(p1,p4,p6,p2,p3, nrow = 1,ncol = 5),
           file= paste0("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/mfair_spatial_transcriptomics_results/plot_slice_",i,".pdf"),
           width =29.7  ,
           height =  6,
           units = "cm"
  )


  tdf=  data.frame(loading= Z[ , 1], x=loc[,1], y=loc[,2])

  p11<-  ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point(size=0.6) +
    scale_color_gradient(low="blue", high="red")+
    ggtitle("MFAI component 1")+
    theme(legend.position = "none")



  tdf=  data.frame(loading= cL[,1], x=loc[,1], y=loc[,2])

  p12 <- ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point(size=0.6) +
    scale_color_gradient(low="blue", high="red")+ggtitle("cEBMF component 1")+
    theme(legend.position = "none")



  tdf=  data.frame(loading= Z[ ,2 ], x=loc[,1], y=loc[,2])

  p21<-  ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point(size=0.6) +
    scale_color_gradient(low="blue", high="red")+ggtitle("MFAI component 2")+
    theme(legend.position = "none")



  tdf=  data.frame(loading= cL[,2], x=loc[,1], y=loc[,2])

  p22 <- ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point(size=0.6) +
    scale_color_gradient(low="blue", high="red")+ggtitle("cEBMF component 2")+
    theme(legend.position = "none")





  tdf=  data.frame(loading= Z[ , 3], x=loc[,1], y=loc[,2])

  p31<-  ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point(size=0.6) +
    scale_color_gradient(low="blue", high="red")+ggtitle("MFAI component 3")+
    theme(legend.position = "none")



  tdf=  data.frame(loading= cL[,3], x=loc[,1], y=loc[,2])

  p32 <- ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point(size=0.6) +
    scale_color_gradient(low="blue", high="red")+ggtitle("cEBMF component 3")+
    theme(legend.position = "none")





  tdf=  data.frame(loading= Z[ , 4], x=loc[,1], y=loc[,2])

  p41<-  ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point(size=0.6) +
    scale_color_gradient(low="blue", high="red")+ ggtitle("MFAI component 4")+
    theme(legend.position = "none")



  tdf=  data.frame(loading= cL[,5], x=loc[,1], y=loc[,2])

  p42 <- ggplot(tdf, aes(x=x, y=y, color=loading)) +
    geom_point( size=0.6) +
    scale_color_gradient(low="blue", high="red")+ ggtitle("cEBMF component 4")+
    theme(legend.position = "none")



  ggsave( plot_grid(p11,p12,
                    p21,p22,
                    p31,p32,
                    p41,p42                  ,
                    nrow = 4,ncol = 2),
          file= paste0("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/mfair_spatial_transcriptomics_results/comp_MFAI_cEBMF4.pdf"),
          width =21  ,
          height =  29.7,
          units = "cm"
  )

