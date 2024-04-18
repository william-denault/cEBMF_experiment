library(NNLM)
library(ggplot2)
library(cowplot)
set.seed(1)
cluster_colors <- c("darkorange","dodgerblue","darkblue")
load("fit_plot_Neurips.RData")
Z <- file_pc$Z
sim <- with(file_pc,data.frame(x = x,y = y,cluster = 0))
for (k in 1:3)
  sim[file_pc$L[,k] == 1,"cluster"] <- k
sim <- transform(sim,cluster = factor(cluster))
p1 <- ggplot(sim,aes(x = x,y = y,color = cluster)) +
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  theme_cowplot(font_size = 12)
pca <- prcomp(Z)
pdat2 <- cbind(sim,pca$x[,1:2])
p2 <- ggplot(pdat2,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  theme_cowplot(font_size = 12)
res <- kmeans(pca$x[,1:2],centers = 3)
print(table(true = sim$cluster,est = res$cluster))
#     est
# true   1   2   3
#    1 313   5   1
#    2   1   0 335
#    3   2 341   2
set.seed(1)
fit <- nnmf(scale(Z,center = TRUE,scale = FALSE),
            k = 3,method = "scd",loss = "mse",verbose = 2,
            n.threads = 2,rel.tol = 1e-8,max.iter = 100)
res2 <- kmeans(fit$W,centers = 3)
print(table(true = sim$cluster,est = res2$cluster))
print(round(rbind(colMeans(fit$W[sim$cluster == 1,]),
                  colMeans(fit$W[sim$cluster == 2,]),
                  colMeans(fit$W[sim$cluster == 3,])),
            digits = 2))
