analyse_slice <- function(i){



  library(keras)
  library(tensorflow)
  library(comoR)
  library(flashier)
  library(ggplot2)
  library(fclust)
  library(scatterpie)
  library(gridExtra)


  sample_names=c("151507", "151508", "151509", "151510", "151669", "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")
  # Here we take the 9th sample as example, in total there are 12 samples (numbered as 1-12), the user can test on other samples if needed.
  clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7) # each sample has different ground truth cluster number



  load( paste0( "/home/wdenault/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
  load(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))
  tt =  as.matrix(count_sub)
  truth = KRM_manual_layers_sub$layer_guess_reordered[match(colnames(LIBD@normalized_expr),colnames(count_sub))]

  loc =  LIBD @location
  tt0 = ( t(as.matrix(LIBD@normalized_expr)) )


  maxit=100
  X=loc
  #define comoR object

 if(file.exists(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData"))){
    load(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData"))
   for (o in 1:maxit) {
     cEBMF.obj <- comoR:::cEBMF_iter  (cEBMF.obj)
     cEBMF.obj <- comoR:::out_prep.cEBMF(cEBMF.obj)
     save(cEBMF.obj, file=paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData"))
   }

  }else{

    library(softImpute)
    l2_reg=0.01
    X_l =X

    X_f =matrix(rnorm(2* ncol(tt0), sd=3), nrow = ncol(tt0))

    param_nnet.x =keras_model_sequential() %>%
      layer_dense(units = 64,
                  activation = 'relu',
                  input_shape = c(ncol(X_l))) %>%
      layer_dense(units = 64,
                  activation = 'relu',
                  kernel_regularizer = regularizer_l2(l2_reg)) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 64,
                  activation = 'relu',
                  kernel_regularizer = regularizer_l2(l2_reg)) %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 64,
                  activation = 'relu' ) %>%
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
    type_noise='constant'
    init_type="flashier_NMF"
    maxit=10
    tol=1e-3

    param_como2 = list()
    param_susie =  list(L=5)
    maxit_como  = 2

    param_como.x  = list(max_class=10,mnreg_type="keras",
                         prior="mix_exp" ,
                         epoch     =100,
                         batch_size= 500)
    param_como.y  = list(max_class=10,mnreg_type="keras",
                         prior="mix_exp" ,
                         epoch     =20,
                         batch_size= 100)
      cEBMF.obj <- comoR:::init_cEBMF (Y,
                               X_l,
                               X_f,
                               mnreg_type=mnreg_type,
                               K=K,
                               type_noise    = type_noise,
                               init_type     = init_type,
                               param_como.x  =  param_como.x,
                               param_como.y  =  param_como.y,
                               maxit_como    = maxit_como,
                               param_nnet.x  = param_nnet.x,
                               param_nnet.y  = param_nnet.y,
                               param_como2   = param_como2,
                               param_susie   = param_susie

      )### Need to carry info about como obj
      for (o in 1:maxit) {
        cEBMF.obj <- comoR:::cEBMF_iter  (cEBMF.obj)
        cEBMF.obj <- comoR:::out_prep.cEBMF(cEBMF.obj)
        save(cEBMF.obj, file=paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData"))
      }
  }



}
