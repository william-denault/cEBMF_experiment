# Peter's attempt at an interesting analysis of the "toy" data set.
library(NNLM)
library(ggplot2)
library(cowplot)
library(Rtsne)
library(ebnm)
library(fastTopics)

set.seed(1)

# Prepare and plot the data.
cluster_colors <- c("darkorange","dodgerblue","darkblue")
load("fit_plot_Neurips.RData")
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

# PCA
# ---
pca <- prcomp(Z)
pdat2 <- cbind(sim,pca$x[,1:2])
pdat2 <- pdat2[order(pdat2$cluster,decreasing = TRUE),]
p2 <- ggplot(pdat2,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "PCA") +
  theme_cowplot(font_size = 10)

# NMF
#
# fit <- nnmf(Z,k = 3,method = "scd",loss = "mse",verbose = 0,
#             n.threads = 2,rel.tol = 1e-8,max.iter = 100)
# W <- fit$W
# W <- W / rowSums(W)
# out <- prcomp(W)
# pdat3 <- cbind(sim,out$x[,1:2])
# p3 <- ggplot(pdat3,aes(x = PC1,y = PC2,color = cluster)) +
#   geom_point() +
#   scale_color_manual(values = cluster_colors) +
#   labs(title = "NMF") +
#   theme_cowplot(font_size = 10)
#

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
  geom_point(show.legend = FALSE, alpha=0.5) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "cEBMF") +
  theme_cowplot(font_size = 10)
p11

L <-file_pc$fit_custom$loading[,2:3]
colnames(L) <- c("loading2","loading3")
pdat3 <- cbind(sim,L)
pdat3 <- pdat3[order(pdat3$cluster,decreasing = TRUE),]
p12 <- ggplot(pdat3,aes(x = loading2,y = loading3,color = cluster)) +
  geom_point(show.legend = FALSE, alpha=0.5) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "cEBMF") +
  theme_cowplot(font_size = 10)
p12






L <-t(file_pc$LIBD@SpatialPCs[1:2,])
colnames(L) <- c("PC1","PC2")
pdat21 <- cbind(sim,L)
pdat21 <- pdat21[order(pdat21$cluster,decreasing = TRUE),]
p21 <- ggplot(pdat21,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point(show.legend = FALSE, alpha=0.5) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "spaPCA") +
  theme_cowplot(font_size = 10)
p21


L <-t(file_pc$LIBD@SpatialPCs[2:3,])
colnames(L) <- c("PC2","PC3")
pdat22 <- cbind(sim,L)
pdat22 <- pdat22[order(pdat22$cluster,decreasing = TRUE),]
p22 <- ggplot(pdat22,aes(x = PC2,y = PC3,color = cluster)) +
  geom_point(show.legend = FALSE, alpha=0.5) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "spaPCA") +
  theme_cowplot(font_size = 10)
p22

print(plot_grid(p1,p2,p3,p4,

                p11,p12,
                p21,p22,
                 ncol = 2))



#prior plot

cEBMF.obj <- file_pc$fit_custom




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
  theme_minimal()+theme( axis.text.y=element_blank(),

                         axis.ticks.y=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())
P_prior_3

print(plot_grid(P_prior_1,P_prior_2,P_prior_3,

                ncol = 3))
