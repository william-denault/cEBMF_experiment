# Peter's attempt at an "interesting" analysis of the "toy" data set.
library(NNLM)
library(ggplot2)
library(cowplot)
library(Rtsne)
library(ebnm)
library(flashier)
library(fastTopics)

set.seed(1)

# Prepare and plot the data.
cluster_colors <- c("darkorange","dodgerblue","darkblue")
load("fit_plot_Neurips.RData")



x= file_pc$x
y= file_pc$y

L= file_pc$L
res= file_pc$fit_custom
cEBMF.obj = res
fit_default=file_pc$fit_default
LIBD=file_pc$LIBD

Z <- file_pc$Z
Z <- scale(Z,center = TRUE,scale = TRUE)
sim <- with(file_pc,data.frame(x = x,y = y,cluster = 0))
for (k in 1:3)
  sim[file_pc$L[,k] > 0,"cluster"] <- k
sim <- transform(sim,cluster = factor(cluster))
p1 <- ggplot(sim,aes(x = x,y = y,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors,) +
  labs(title = "ground truth") +
  theme_cowplot(font_size = 10)

n    <- nrow(Z)
rows <- sample(n)

# PCA
# ---
pca <- prcomp(Z)
pdat2 <- cbind(sim,pca$x[,1:2])
pdat2 <- pdat2[order(pdat2$cluster,decreasing = TRUE),]
pdat2 <- pdat2[rows,]
p2 <- ggplot(pdat2,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "PCA") +
  theme_cowplot(font_size = 10)

# NMF
# ---
set.seed(1)
nmf <- nnmf(Z,k = 3,method = "scd",loss = "mse",verbose = 0,
            rel.tol = 1e-8,max.iter = 100)
W <- nmf$W
out <- prcomp(W)
pdat3 <- cbind(sim,out$x[,1:2] + matrix(rnorm(2*n,sd = 1),n,2))
pdat3 <- pdat3[rows,]
p3 <- ggplot(pdat3,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "NMF") +
  theme_cowplot(font_size = 10)

# Spatial PCA
# -----------
L <- t(file_pc$LIBD@SpatialPCs[1:2,])
colnames(L) <- c("PC1","PC2")
pdat4 <- cbind(sim,L)
pdat4 <- pdat4[rows,]
p4 <- ggplot(pdat4,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "spatial PCA") +
  theme_cowplot(font_size = 10)

# EBNMF
# -----
set.seed(1)
nmf0 <- nnmf(Z,k = 3,method = "scd",loss = "mse",verbose = 0,
             rel.tol = 1e-8,max.iter = 10)
fl <- flash_init(Z,var_type = 0)
fl <- flash_factors_init(fl,
                         list(nmf0$W,t(nmf0$H)),
                         ebnm_point_exponential)
fl <- flash_backfit(fl,maxiter = 100,verbose = 0)
L <- ldf(fl,type = "f")$L
out <- prcomp(L)
pdat5 <- cbind(sim,out$x[,1:2] + matrix(rnorm(2*n,sd = 0.01),n,2))
pdat5 <- pdat5[rows,]
p5 <- ggplot(pdat5,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "EBNMF") +
  theme_cowplot(font_size = 10)

# covariate-moderated EBNMF
# -------------------------
W <- file_pc$fit_custom$loading
W <- W / rowSums(W)
out <- prcomp(W)
pdat6 <- cbind(sim,out$x[,1:2] + matrix(rnorm(2*n,sd = 0.1),n,2))
pdat6 <- pdat6[rows,]
p6 <- ggplot(pdat6,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "cEBNMF") +
  theme_cowplot(font_size = 10)

# The prior in cEBNMF, in detail.
pdat7 <- data.frame(x = x,y = y,
                    pi0 = 1-exp(cEBMF.obj$check_l_prior[[1]][,1]))
pdat8 <- data.frame(x = x,y = y,
                    pi0 = 1-exp(cEBMF.obj$check_l_prior[[2]][,1]))
pdat9 <- data.frame(x = x,y = y,
                    pi0 =1-exp(cEBMF.obj$check_l_prior[[3]][,1]))
p7 <- ggplot(pdat7,aes(x = x,y = y,color = pi0))+
  geom_point(show.legend = FALSE) +
  scale_color_gradient2(low = "deepskyblue",mid = "gold",high = "red",
                        midpoint = 0.5) +
  ggtitle("prior, first factor") +
  theme_cowplot(font_size = 10)
p8 <- ggplot(pdat8,aes(x = x,y = y,color = pi0))+
  geom_point(show.legend = FALSE) +
  scale_color_gradient2(low = "red",mid = "gold",high = "deepskyblue",
                        midpoint = 0.5) +
  ggtitle("prior, second factor") +
  theme_cowplot(font_size = 10)
p9 <- ggplot(pdat9,aes(x = x,y = y,color = pi0))+
  geom_point(show.legend = FALSE) +
  scale_color_gradient2(low = "red",mid = "gold",high = "deepskyblue",
                        midpoint = 0.5) +
  ggtitle("prior, third factor") +
  theme_cowplot(font_size = 10)
p10 <- ggplot(pdat7,aes(x = x,y = y,color = pi0))+
  geom_point(show.legend = TRUE) +
  scale_color_gradient2(low = "red",mid = "gold",high = "deepskyblue",
                        midpoint = 0.5) +
  theme_cowplot(font_size = 10)

print(plot_grid(p1,p2,p4,
                p3,p5,p6,
                p7,p8,p9,
                p10,
                nrow = 4,ncol = 3))

# ------------------------------------------------------------------------
#
#                           END OF TOY EXAMPLE
#
# ------------------------------------------------------------------------

stop()

# flashier
# --------
library(flashier)
ks <- 1:2
fit_flash <- flash_init(Z,var_type = 0)
fit_flash <- flash_factors_init(fit_flash,
                           list(pca$x[,ks],pca$rotation[,ks]),
                           ebnm_point_laplace)
fit_flash <- flash_backfit(fit_flash)
# fit_flash <- flash(Z,greedy_Kmax = 2,
#                    ebnm_fn = ebnm_point_laplace,
#                    backfit = TRUE)
L <- ldf(fit_flash,type = "2")$L
colnames(L) <- c("PC1","PC2")
pdat3 <- cbind(sim,L)
pdat3 <- pdat3[order(pdat3$cluster,decreasing = TRUE),]
p3 <- ggplot(pdat3,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "flashier + point_laplace") +
  theme_cowplot(font_size = 10)

# PCA vs. flashier
# ----------------
pdat4 <- data.frame(pca      = as.vector(pca$rotation[,ks]),
                    flashier = as.vector(ldf(fit_flash)$F))
p4 <- ggplot(pdat4,aes(x = pca,y = flashier)) +
  geom_point(shape = 4,size = 0.75) +
  geom_abline(intercept = 0,slope = 1,color = "magenta",linetype = "dashed") +
  labs(y = "flashier + point_laplace",
       title = "rotation matrix") +
  theme_cowplot(font_size = 10)

print(plot_grid(p1,p2,p3,p4,nrow = 2,ncol = 2))

L <-file_pc$fit_custom$loading[,1:2]
colnames(L) <- c("loading1","loading2")
pdat3 <- cbind(sim,L)
pdat3 <- pdat3[order(pdat3$cluster,decreasing = TRUE),]
p11 <- ggplot(pdat3,aes(x = loading1,y = loading2,color = cluster)) +
  geom_point(shape = 1) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "cEBMF") +
  theme_cowplot(font_size = 10)
p11

L <-file_pc$fit_custom$loading[,2:3]
colnames(L) <- c("loading2","loading3")
pdat3 <- cbind(sim,L)
pdat3 <- pdat3[order(pdat3$cluster,decreasing = TRUE),]
p12 <- ggplot(pdat3,aes(x = loading2,y = loading3,color = cluster)) +
  geom_point(shape = 1) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "cEBMF") +
  theme_cowplot(font_size = 10)
p12

plot_grid(p11,p12)


## L <-t(file_pc$LIBD@SpatialPCs[2:3,])
## colnames(L) <- c("PC2","PC3")
## pdat22 <- cbind(sim,L)
## pdat22 <- pdat22[order(pdat22$cluster,decreasing = TRUE),]
## p22 <- ggplot(pdat22,aes(x = PC2,y = PC3,color = cluster)) +
##   geom_point(show.legend = FALSE, alpha=0.5) +
##   scale_color_manual(values = cluster_colors) +
##   labs(title = "spaPCA") +
##   theme_cowplot(font_size = 10)
## p22

print(plot_grid(p1,p2,p3,p4,

                p11,p12,
                p21,p22,
                 ncol = 2))

plot_grid(p1,p2,p3,p21,nrow = 2,ncol = 2)

#prior plot

cEBMF.obj <- file_pc$fit_custom

x=file_pc$x
y=file_pc$y

df_prior <-  data.frame(x=x,y=y, pi0 = exp(cEBMF.obj$check_l_prior[[1]][,1]))
P_prior_1 <- ggplot(df_prior, aes(x,y,col=pi0))+
  geom_point(show.legend = FALSE)+
  scale_color_gradient2(low =  "grey", high = "darkblue" ,midpoint = 0.5 ) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  theme_minimal()+theme( axis.text.y=element_blank(),

                         axis.ticks.y=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())
P_prior_1
df_prior <-  data.frame(x=x,y=y, pi0 = exp(cEBMF.obj$check_l_prior[[2]][,1]))
P_prior_2 <- ggplot(df_prior, aes(x,y,col=pi0))+
  geom_point(show.legend = FALSE)+
  scale_color_gradient2(low =  "grey", high = "darkblue" ,midpoint = 0.5 ) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  theme_minimal()+theme( axis.text.y=element_blank(),

                         axis.ticks.y=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())
P_prior_2
df_prior <-  data.frame(x=x,y=y, pi0 = exp(cEBMF.obj$check_l_prior[[3]][,1]))
P_prior_3 <- ggplot(df_prior, aes(x,y,col=pi0))+
  geom_point()+
  scale_color_gradient2(low =  "grey", high = "darkblue" ,midpoint = 0.5 ) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  theme_cowplot(font_size = 10) +
  theme( axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
P_prior_3

print(plot_grid(P_prior_1,P_prior_2,P_prior_3,

                ncol = 3))
