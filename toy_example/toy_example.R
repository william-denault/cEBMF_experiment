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
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  labs(title = "ground truth") +
  theme_cowplot(font_size = 10)

# PCA
# ---
pca <- prcomp(Z)
pdat2 <- cbind(sim,pca$x[,1:2])
pdat2 <- pdat2[order(pdat2$cluster,decreasing = TRUE),]
p2 <- ggplot(pdat2,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point() +
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
fit_flash <- flash(Z,greedy_Kmax = 2,
                   ebnm_fn = ebnm_point_normal,
                   backfit = TRUE)
# L <- fit_flash$L_pm
L <- ldf(fit_flash,type = "2")$L
colnames(L) <- c("PC1","PC2")
pdat4 <- cbind(sim,L)
pdat4 <- pdat4[order(pdat4$cluster,decreasing = TRUE),]
p3 <- ggplot(pdat4,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  labs(title = "flashier") +
  theme_cowplot(font_size = 10)

print(plot_grid(p1,p2,p3,nrow = 2,ncol = 2))
