i=1
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



load( paste0( "/home/wdenault/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
load(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))
tt =  as.matrix(count_sub)
truth = KRM_manual_layers_sub$layer_guess_reordered[match(colnames(LIBD@normalized_expr),colnames(count_sub))]

loc =  LIBD @location
tt0 = ( t(as.matrix(LIBD@normalized_expr)) )


maxit=50
X=loc
#define comoR object

library(softImpute)
l2_reg=0.00002
Y <- t(t(tt0) - apply(tt0,2,min))
X_l =X

X_f =matrix(rnorm(2* ncol(tt0), sd=3), nrow = ncol(tt0))

param_nnet.x =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'sigmoid',
              input_shape = c(ncol(X_l))) %>%
  layer_dense(units = 64,
              activation = 'sigmoid',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64,
              activation = 'sigmoid',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 64,
              activation = 'sigmoid',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 64,
              activation = 'sigmoid',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 10,
              activation = 'softmax')


param_nnet.y =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(ncol(X_f))) %>%
  layer_dense(units = 10,
              activation = 'softmax')

mnreg_type="keras"
K=3
type_noise='column_wise'
init_type="udv_si_svd"#"flashier_NMF"
maxit=5
tol=1e-3

param_como2 = list()
param_susie =  list(L=5)
maxit_como  = 2

param_como.x  = list(max_class=10,mnreg_type="keras",
                     prior="mix_exp" ,
                     epoch     =50,
                     batch_size= 2500)
param_como.y  = list(max_class=10,mnreg_type="constant_mnreg",
                     prior="mix_exp"  )
tt0 = ( t(as.matrix(LIBD@normalized_expr)) )



fit_nmf <- nnmf(tt0,k = clusterNum[i],method = "scd",loss = "mse",verbose = 0,
                n.threads = 2,rel.tol = 1e-8,max.iter = 100)

k = clusterNum[i]
n   <- nrow(tt0)
m   <- ncol(tt0)
fl0 <- flash(tt0,greedy_Kmax = 1,ebnm_fn = ebnm_point_exponential,
             S = 0.01,var_type = 2)
W0  <- cbind(fl0$L_pm,matrix(runif(n*k),n,k))
H0  <- t(cbind(fl0$F_pm,matrix(runif(m*k),m,k)))
nmf0 <- nnmf(tt0,k = dim(W0)[2],method = "scd",loss = "mse",n.threads = 4,
             init = list(W = W0,H = H0),max.iter = 4,rel.tol = 1e-8,
             verbose = 2)
nmf <- nnmf(tt0,k = k + 1,method = "scd",loss = "mse",n.threads = 4,
            init = list(W = W0,H = H0),max.iter = 50,rel.tol = 1e-8,
            verbose = 2)
fl <- flash_init(tt0,S = 0.01,var_type = 2)
fl <- flash_factors_init(fl,
                         list(nmf0$W,t(nmf0$H)),
                         ebnm_point_exponential)
fl <- flash_backfit(fl,maxiter = 100,verbose = 3)




mnreg_type="keras"
K=3
type_noise='column_wise'
init_type="udv_si_svd"#"flashier_NMF"
maxit=50
tol=1e-3

param_como2 = list()
param_susie =  list(L=5)
maxit_como  = 2

param_como.x  = list(max_class=10,mnreg_type="keras",
                     prior="mix_exp" ,
                     epoch     =250,
                     batch_size= 2000)
param_como.y  = list(max_class=10,mnreg_type="constant_mnreg",
                     prior="mix_exp"  )
tt0 = ( t(as.matrix(LIBD@normalized_expr)) )

fl_nmf <- flash_init(tt0,S = 0.01,var_type = 2)
fl_nmf <- flash_factors_init(fl_nmf,
                             list(nmf$W,t(nmf$H)),
                             ebnm_point_exponential)
fit_default<- fl_nmf

intercept <-   fit_default$L_pm[,1]%*% t(fit_default$F_pm[,1])

cEBMF.obj <- comoR:::init_cEBMF (tt0 -intercept  ,# removed estimated intercept
                                 X_l,
                                 X_f,
                                 mnreg_type.x="keras",
                                 mnreg_type.y="constant_mnreg",
                                 K=clusterNum[i],
                                 type_noise    = type_noise,
                                 init_type     = init_type,
                                 param_como.x  =  param_como.x,
                                 param_como.y  =  param_como.y,
                                 maxit_como    = 1,
                                 param_nnet.x  = param_nnet.x,
                                 param_como2   = param_como2,
                                 param_susie   = param_susie,
                                 check_l_prior = TRUE )





for (o in 2:min(ncol(cEBMF.obj$loading), ncol(fit_default$L_pm))) {
  cEBMF.obj$loading[,o] <- fit_default$L_pm[,o]
  cEBMF.obj$loading2[,o]  <- fit_default$L_pm[,o] ^2
  cEBMF.obj$factor [,o] <- fit_default$F_pm[,o]
  cEBMF.obj$factor2 [,o] <- fit_default$F_pm[,o] ^2


}

for (o in 1:maxit) {#5 is good
  cEBMF.obj <- comoR:::cEBMF_iter  (cEBMF.obj)
  cEBMF.obj <- comoR:::out_prep.cEBMF(cEBMF.obj)
  save(cEBMF.obj, file=paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData"))
}



