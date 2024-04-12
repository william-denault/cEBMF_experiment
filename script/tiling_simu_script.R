
tiling_sim <-  function(noise_level,seed=1){

  max_class=10
  max_iter_como=10
  set.seed(seed)
  x <-runif(1000)
  y <-runif(1000)
  X = cbind(x,y)
  max_iter_cEBMF=10
  library(flashier)

  library(keras)

  library(tensorflow)

  #problem fro set.seed(1)
  f <- matrix(NA, nrow = 3, ncol =200)
  for ( i in 1:ncol (f)){

    t1<- sample (c(0,1), size=1)
    t2<- sample (c(0,1), size=1)

    f[1,i] <- t1*rnorm(n=1)
    f[2,i] <- t2*rnorm(n=1)

    f[3,i] <- t2*rnorm(n=1)

  }
  L <- matrix(NA, ncol=3, nrow=length(x))

  factor  <- c()

  for (i in 1:length(x)){

    if ( (x[i] <.33 & y[i] <.33 )|(x[i] >.33 & y[i] >.33 &  x[i] <.66 & y[i] <.66) | (x[i] >.66 & y[i] >.66 )){
      L[i,] <- c(1,0,0)
      factor=c(factor,1)
    }else{
      if ( (x[i] <.33 & y[i] >.66 )|(x[i] >.33 & y[i] <.33 &  x[i] <.66  ) | (x[i] >.66 & y[i] >.33  & y[i] <.66)){
        L[i,] <- c(0,1,0)
        factor=c(factor,2)
      }else{
        L[i,] <- c(0,0,1)
        factor=c(factor,3)
      }
    }


  }

  df = data.frame(x=x,y=y, Factor=as.factor(factor))



  Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=noise_level), nrow = nrow(L))

  X_l =X
  X_f = matrix(rnorm(2* ncol(Z)), ncol = 2)


  l2_reg = 0.001

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

  library(softImpute)
  res_nnet <-cEBMF  ( Y=Z,
                      X_l,
                      X_f,
                      mnreg_type="keras",
                      K=4,
                      type_noise='constant',
                      init_type="udv",
                      maxit=10,
                      tol=1e-3 ,
                      param_como.x  = list(max_class=10,mnreg_type="keras",
                                           prior="mix_exp" ,
                                           epoch     =50,
                                           batch_size= 500),
                      param_como.y  = list(max_class=10,mnreg_type="keras",
                                           prior="mix_norm" ,
                                           epoch     =50,
                                           batch_size= 100),
                      param_nnet.x  =param_nnet.x ,
                      param_nnet.y  =param_nnet.y,

                      maxit_como  = 2)



  library(flashier)

  fit_default <- flash(Z, greedy_Kmax = 5)



  library(keras)
  library(tensorflow)
  library(comoR)


  cebnm_L <- function( x,s,g_init=FALSE,fix_g=TRUE, output){

    if (length(x) == 3){ ### just to satisfy check of custom function
      return (ebnm_flat(x))
    }
    Z <- matrix( 1, nrow=length(x), ncol=1)
    param_como = list(max_class= 10,
                      mnreg_type="keras",
                      prior    ='mix_norm',
                      epoch     =150)
    data <- comoR:::como_prep_data (betahat=x,
                                    se=s, X=X,
                                    Z =Z )

    # you need to retreive the actual number of mixture component in the model
    num_classes <- length( autoselect_scales_mix_norm(data$betahat, data$se,10))

    #define the nnet paramet using Keras syntax
    param_nnet =keras_model_sequential() %>%
      layer_dense(units = 64,
                  activation = 'relu',
                  input_shape = c(ncol(X))) %>%
      layer_dense(units = 64,
                  activation = 'relu' ) %>%
      layer_dense(units = 64,
                  activation = 'relu' ) %>%
      layer_dense(units = 64,
                  activation = 'relu' ) %>%
      layer_dense(units = num_classes,
                  activation = 'softmax')

    # run comoR
    fit  <- rlang::exec( "data_initialize_como", !!! param_como ,
                         data= data,
                         param_nnet= param_nnet) # initialize the model from the data
    fit <- comoR:::fit.como (  fit, data, max_iter = 10 )


    est <- comoR:::post_mean_sd (fit,data)



    g <- ashr::normalmix(rep(1/length(fit$f_list),length(fit$f_list)),
                         rep( 0, length(fit$f_list)),
                         do.call(c, lapply( 1: length(fit$f_list) ,
                                            function(k) {sqrt(fit$f_list [[k]]$var) } )
                         )
    )

    out <- list( data= data.frame(x=data$betahat,
                                  s=data$se),
                 posterior = data.frame(mean= est$mean,
                                        second_moment=(est$sd^2+est$mean^2)
                 ) ,
                 fitted_g = g,
                 log_likelihood=sum( comoR:::compute_data_loglikelihood(fit, data) * (fit$post_assignment))

    )

    return( out)

  }




  library(flashier)
  fit_custom <- flash_init(Z, var_type = 2 ) %>%

    flash_set_verbose(0) %>%
    flash_greedy(
      Kmax = 5,
      ebnm_fn = c(cebnm_L, ebnm_ash)
    )



  svd_res  = svd(Z)
  #load a spatial data
  load("/home/wdenault/cEBMF_RCC_experiments/misc/run_spatial_DLPFC9.RData")


  library(SpatialPCA)
  LIBD @normalized_expr =t(Z)
  LIBD @location =X
  LIBD = SpatialPCA_buildKernel(LIBD, kerneltype="gaussian", bandwidthtype="SJ",bandwidth.set.by.user=NULL)
  LIBD = SpatialPCA_EstimateLoading(LIBD,fast=FALSE,SpatialPCnum=20)
  LIBD = SpatialPCA_SpatialPCs(LIBD, fast=FALSE)
  library(irlba)
  library(PMA)
  ssvd_res = ssvd(Z, k=3)
  svd_res  = svd(Z)

  rmse = function(mat1 , mat2){

    squared_diff <- (mat1-  mat2)^2

    # Compute mean of squared differences
    mean_squared_diff <- mean(squared_diff)

    # Compute RMSE
    rmse <- sqrt(mean_squared_diff)
    return (rmse)
  }



  cv.out <- PMD.cv(Z, type="standard", sumabss=seq(0.1, 0.6, len=20))
  PMD_res <- PMD(Z,
                 type="standard",
                 sumabs=cv.out$bestsumabs,
                 K=3, v=cv.out$v.init
  )



  rmse_cEBMF0       <- rmse(c(res_nnet$Y_fit)  ,c(L%*%f))
  rmse_flash        <- rmse(c(fitted(fit_default )) ,c(L%*%f))
  rmse_cEBMF_nnet   <- rmse(c(fitted(fit_custom )) ,c(L%*%f))
  rmse_spatial_PCA  <- rmse(t(LIBD@SpatialPCs) %*%  t(LIBD@W),c(L%*%f))



  rmse_PMD         <- rmse(c(L%*%f), PMD_res$u%*%diag(PMD_res$d)%*%t(PMD_res$v))
  rmse_svd         <- rmse(c(L%*%f), svd_res$u[,1:3]%*%diag(svd_res$d[ 1:3])%*%t(svd_res$v[,1:3]))
  rmse_ssvd        <- rmse(c(L%*%f), ssvd_res$u%*%ssvd_res$d%*%t(ssvd_res$v))
  rmse_out         <- c(rmse_cEBMF0 , rmse_cEBMF_nnet ,
                        rmse_flash  ,
                        rmse_spatial_PCA,
                        rmse_PMD,
                        rmse_svd,
                        rmse_ssvd)
  names( rmse_out  ) <- c("cEBMF0", "cEBMF",
                          "EBMF",
                          "Spatial PCA",
                          "SVD",
                          "SSVD",
                          "PMD" )

  return(out =list( rmse = rmse_out,
                    #ARI = ARI_out,
                    noise_level = noise_level))

}
