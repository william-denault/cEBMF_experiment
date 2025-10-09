i=4
library(keras)
library(tensorflow)
library(comoR)
library(flashier)
library(ggplot2)
library(fclust)
library(scatterpie)
library(gridExtra)
library(NNLM)

sample_names=c("151507", "151508", "151509", "151510", "151669", "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")
# Here we take the 9th sample as example, in total there are 12 samples (numbered as 1-12), the user can test on other samples if needed.
clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7) # each sample has different ground truth cluster number



load( paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
load(paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))
tt =  as.matrix(count_sub)
truth = KRM_manual_layers_sub$layer_guess_reordered[match(colnames(LIBD@normalized_expr),colnames(count_sub))]

loc =  LIBD @location
tt0 = ( t(as.matrix(LIBD@normalized_expr)) )


maxit=100
X=loc
#define comoR object

library(softImpute)
l2_reg=0.2
Y <- t(t(tt0) - apply(tt0,2,min))
X_l =X








tt0 = ( t(as.matrix(LIBD@normalized_expr)) )




mfairObject <- createMFAIR(tt0,as.data.frame(X_l ), K_max = 7)

mfairObject <- fitGreedy(mfairObject, sf_para = list(verbose_loop = FALSE))

#for (o in 2:min(ncol(cEBMF.obj$loading), ncol(fit_default$L_pm))) {
#  cEBMF.obj$loading[,(o-1)] <- fit_default$L_pm[,o]
#  cEBMF.obj$loading2[,(o-1)]  <- fit_default$L_pm[,o] ^2
#  cEBMF.obj$factor [,(o-1)] <- fit_default$F_pm[,o]
# cEBMF.obj$factor2 [,(o-1)] <- fit_default$F_pm[,o] ^2


#}
res <-mfairObject
save(res,
     file=paste0("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/mfair_spatial_transcriptomics_results/run_spatial_DLPFC_",i,".RData")
)
